//! Persistent cache implementation using bincode serialization.
//!
//! Provides disk-based caching with versioning and corruption handling.

use crate::semantic::passes::manager::{ModuleCacheKey, ModuleCacheResult};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

/// Configuration for the persistent cache
#[derive(Debug, Clone)]
pub struct CacheConfig {
    /// Cache directory path
    pub cache_dir: PathBuf,
    /// Cache version (for invalidating incompatible caches)
    pub version: u32,
    /// Maximum cache size in bytes (0 = unlimited)
    pub max_size_bytes: u64,
    /// Maximum number of entries (0 = unlimited)
    pub max_entries: usize,
    /// Use memory-mapped caching for large caches (>10MB)
    pub use_memory_mapping: bool,
    /// Enable verbose logging for cache operations
    pub verbose_logging: bool,
}

impl Default for CacheConfig {
    fn default() -> Self {
        Self {
            cache_dir: PathBuf::from(".coral/cache"),
            version: 2,                        // Bumped for CacheEntry structure change
            max_size_bytes: 100 * 1024 * 1024, // 100MB
            max_entries: 1000,
            use_memory_mapping: false,
            verbose_logging: false, // Disabled by default to avoid spam
        }
    }
}

/// Errors that can occur during cache operations
#[derive(Debug, thiserror::Error)]
pub enum CacheError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Serialization encode error: {0}")]
    EncodeError(#[from] bincode::error::EncodeError),

    #[error("Serialization decode error: {0}")]
    DecodeError(#[from] bincode::error::DecodeError),

    #[error("Cache version mismatch: expected {expected}, found {found}")]
    VersionMismatch { expected: u32, found: u32 },

    #[error("Cache corrupted")]
    Corrupted,
}

/// Metadata for cache entries
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct CacheMetadata {
    /// Cache version
    version: u32,
    /// Timestamp when cache was created
    created_at: u64,
    /// Number of entries in cache
    entry_count: usize,
}

/// A cache entry with access tracking for LRU eviction
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CacheEntry {
    /// The cached result
    pub result: ModuleCacheResult,
    /// Last access timestamp (UNIX epoch seconds)
    pub last_accessed: u64,
    /// Size of this entry in bytes (approximate)
    pub size_bytes: u64,
}

/// Persistent cache that stores analysis results to disk
#[derive(Debug)]
pub struct PersistentCache {
    config: CacheConfig,
    cache_file: PathBuf,
    in_memory_cache: HashMap<ModuleCacheKey, CacheEntry>,
    metadata: CacheMetadata,
    dirty: bool,
}

impl PersistentCache {
    /// Create a new persistent cache
    pub fn new(config: CacheConfig) -> Result<Self, CacheError> {
        let cache_file = config.cache_dir.join("analysis_cache.bin");

        // Ensure cache directory exists
        fs::create_dir_all(&config.cache_dir)?;

        let (in_memory_cache, metadata) = Self::load_cache(&cache_file, config.version)?;

        // If this is a newly created cache (file didn't exist), mark as dirty to persist metadata
        let was_file_missing = !cache_file.exists();

        let mut cache = Self {
            config,
            cache_file,
            in_memory_cache,
            metadata,
            dirty: false,
        };

        // Always persist newly created caches to ensure version metadata is saved
        if was_file_missing {
            let _ = cache.persist(); // Ignore errors during initialization
        }

        Ok(cache)
    }

    /// Load cache from disk
    fn load_cache(
        cache_file: &Path,
        expected_version: u32,
    ) -> Result<(HashMap<ModuleCacheKey, CacheEntry>, CacheMetadata), CacheError> {
        if !cache_file.exists() {
            // Create new cache metadata
            let metadata = CacheMetadata {
                version: expected_version,
                created_at: SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
                entry_count: 0,
            };
            return Ok((HashMap::new(), metadata));
        }

        // Read and deserialize cache
        let data = fs::read(cache_file)?;

        // Try to load as new format (CacheEntry)
        if expected_version >= 2
            && let Ok(((metadata, cache), _bytes_read)) =
                bincode::serde::decode_from_slice::<
                    (CacheMetadata, HashMap<ModuleCacheKey, CacheEntry>),
                    _,
                >(&data, bincode::config::standard())
            && metadata.version == expected_version
        {
            return Ok((cache, metadata));
        }

        // Try to load as old format (ModuleCacheResult) and migrate
        if let Ok(((metadata, old_cache), _bytes_read)) =
            bincode::serde::decode_from_slice::<
                (CacheMetadata, HashMap<ModuleCacheKey, ModuleCacheResult>),
                _,
            >(&data, bincode::config::standard())
        {
            // Always migrate old format to new format for consistency
            let current_time = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();

            let mut new_cache = HashMap::new();
            for (key, result) in old_cache {
                // Calculate approximate size
                let diagnostics_size = result.all_diagnostics.len() as u64 * 100;
                let pass_results_size = result.pass_results.len() as u64 * 50;
                let size_bytes = diagnostics_size + pass_results_size + 100;

                let entry = CacheEntry {
                    result,
                    last_accessed: current_time,
                    size_bytes,
                };
                new_cache.insert(key, entry);
            }

            // Update metadata
            let new_metadata = CacheMetadata {
                version: expected_version,
                created_at: metadata.created_at,
                entry_count: new_cache.len(),
            };

            return Ok((new_cache, new_metadata));
        }

        // If we can't load the cache, create a new one
        let metadata = CacheMetadata {
            version: expected_version,
            created_at: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            entry_count: 0,
        };
        Ok((HashMap::new(), metadata))
    }

    /// Get a cached result
    pub fn get(&mut self, key: &ModuleCacheKey) -> Option<&ModuleCacheResult> {
        if let Some(entry) = self.in_memory_cache.get_mut(key) {
            // Update access time
            entry.last_accessed = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
            self.dirty = true;
            Some(&entry.result)
        } else {
            None
        }
    }

    /// Insert a result into the cache
    pub fn insert(
        &mut self,
        key: ModuleCacheKey,
        result: ModuleCacheResult,
    ) -> Result<(), CacheError> {
        // Calculate approximate size of the entry
        let entry_size = self.calculate_entry_size(&result);

        // Evict entries if needed before inserting
        self.evict_if_needed(entry_size)?;

        // Create cache entry with current timestamp
        let entry = CacheEntry {
            result,
            last_accessed: SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            size_bytes: entry_size,
        };

        self.in_memory_cache.insert(key, entry);
        self.metadata.entry_count = self.in_memory_cache.len();
        self.dirty = true;

        Ok(())
    }

    /// Calculate approximate size of a cache entry in bytes
    fn calculate_entry_size(&self, result: &ModuleCacheResult) -> u64 {
        // Rough estimation: each diagnostic is ~100 bytes, each pass result ~50 bytes
        let diagnostics_size = result.all_diagnostics.len() as u64 * 100;
        let pass_results_size = result.pass_results.len() as u64 * 50;
        diagnostics_size + pass_results_size + 100 // Base overhead
    }

    /// Evict entries using LRU policy if limits would be exceeded
    fn evict_if_needed(&mut self, new_entry_size: u64) -> Result<(), CacheError> {
        // Check entry count limit
        if self.config.max_entries > 0 && self.in_memory_cache.len() >= self.config.max_entries {
            self.evict_lru_entries(1)?;
        }

        // Check size limit
        let current_size: u64 = self.in_memory_cache.values().map(|e| e.size_bytes).sum();
        if self.config.max_size_bytes > 0
            && current_size + new_entry_size > self.config.max_size_bytes
        {
            // Calculate how much space we need to free
            let space_needed =
                (current_size + new_entry_size).saturating_sub(self.config.max_size_bytes);

            // Find entries to evict (LRU - least recently used)
            let mut entries_by_access: Vec<_> = self.in_memory_cache.iter().collect();
            entries_by_access.sort_by_key(|(_, entry)| entry.last_accessed);

            let mut evicted_count = 0;
            let mut freed_space = 0u64;

            for (key, entry) in entries_by_access {
                if freed_space >= space_needed {
                    break;
                }
                freed_space += entry.size_bytes;
                evicted_count += 1;
                if self.config.verbose_logging {
                    eprintln!(
                        "Cache eviction: removing entry for module '{}'",
                        key.module_name
                    );
                }
            }

            if evicted_count > 0 {
                self.evict_lru_entries(evicted_count)?;
            }
        }

        Ok(())
    }

    /// Evict the least recently used entries
    fn evict_lru_entries(&mut self, count: usize) -> Result<(), CacheError> {
        // Sort entries by access time (oldest first) and collect keys
        let mut entries_by_access: Vec<_> = self.in_memory_cache.iter().collect();
        entries_by_access.sort_by_key(|(_, entry)| entry.last_accessed);

        // Collect the keys to remove (clone them to avoid borrowing issues)
        let keys_to_remove: Vec<_> = entries_by_access
            .into_iter()
            .take(count)
            .map(|(key, _)| key.clone())
            .collect();

        // Remove the oldest entries
        for key in keys_to_remove {
            self.in_memory_cache.remove(&key);
        }

        Ok(())
    }

    /// Remove an entry from the cache
    pub fn remove(&mut self, key: &ModuleCacheKey) -> bool {
        if self.in_memory_cache.remove(key).is_some() {
            self.metadata.entry_count = self.in_memory_cache.len();
            self.dirty = true;
            true
        } else {
            false
        }
    }

    /// Clear all cached entries
    pub fn clear(&mut self) {
        self.in_memory_cache.clear();
        self.metadata.entry_count = 0;
        self.dirty = true;
    }

    /// Persist cache to disk if dirty
    pub fn persist(&mut self) -> Result<(), CacheError> {
        if !self.dirty {
            return Ok(());
        }

        // Serialize cache data
        let cache_data = (&self.metadata, &self.in_memory_cache);
        let serialized = bincode::serde::encode_to_vec(cache_data, bincode::config::standard())?;

        // Write to temporary file first, then atomically rename
        let temp_file = self.cache_file.with_extension("tmp");
        fs::write(&temp_file, &serialized)?;
        fs::rename(&temp_file, &self.cache_file)?;

        self.dirty = false;
        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            entry_count: self.in_memory_cache.len(),
            is_dirty: self.dirty,
            cache_file_size: self.cache_file.metadata().map(|m| m.len()).unwrap_or(0),
            created_at: self.metadata.created_at,
        }
    }

    /// Force persist regardless of dirty flag
    pub fn force_persist(&mut self) -> Result<(), CacheError> {
        self.dirty = true;
        self.persist()
    }
}

impl Drop for PersistentCache {
    fn drop(&mut self) {
        // Try to persist on drop, but ignore errors since we're in a destructor
        let _ = self.persist();
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    /// Number of entries in cache
    pub entry_count: usize,
    /// Whether cache has unsaved changes
    pub is_dirty: bool,
    /// Size of cache file on disk (bytes)
    pub cache_file_size: u64,
    /// When cache was created (unix timestamp)
    pub created_at: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_cache_result() -> ModuleCacheResult {
        ModuleCacheResult {
            pass_results: HashMap::new(),
            overall_success: true,
            all_diagnostics: Vec::new(),
            analysis_timestamp: 1234567890,
        }
    }

    fn create_test_cache_key(module_name: &str) -> ModuleCacheKey {
        ModuleCacheKey {
            module_name: module_name.to_string(),
            mtime: 1234567890,
            content_hash: Some("test_hash".to_string()),
        }
    }

    #[test]
    fn test_persistent_cache_basic() {
        let temp_dir = TempDir::new().unwrap();
        let config = CacheConfig {
            cache_dir: temp_dir.path().to_path_buf(),
            version: 1,
            max_entries: 10,
            max_size_bytes: 1024 * 1024,
            use_memory_mapping: false,
            verbose_logging: false,
        };

        let mut cache = PersistentCache::new(config).unwrap();
        let key = create_test_cache_key("test_module");
        let result = create_test_cache_result();

        // Insert and retrieve
        cache.insert(key.clone(), result.clone()).unwrap();
        assert!(cache.get(&key).unwrap().overall_success);

        // Persist
        cache.persist().unwrap();

        // Verify file exists
        assert!(temp_dir.path().join("analysis_cache.bin").exists());
    }

    #[test]
    fn test_cache_persistence() {
        let temp_dir = TempDir::new().unwrap();
        let cache_dir = temp_dir.path().to_path_buf();

        // Create cache and add entry
        {
            let config = CacheConfig {
                cache_dir: cache_dir.clone(),
                version: 1,
                max_entries: 10,
                max_size_bytes: 1024 * 1024,
                use_memory_mapping: false,
                verbose_logging: false,
            };

            let mut cache = PersistentCache::new(config).unwrap();
            let key = create_test_cache_key("persistent_test");
            let result = create_test_cache_result();

            cache.insert(key, result).unwrap();
            cache.persist().unwrap();
        }

        // Load cache in new instance
        {
            let config = CacheConfig {
                cache_dir,
                version: 1,
                max_entries: 10,
                max_size_bytes: 1024 * 1024,
                use_memory_mapping: false,
                verbose_logging: false,
            };

            let mut cache = PersistentCache::new(config).unwrap();
            let key = create_test_cache_key("persistent_test");

            assert!(cache.get(&key).is_some());
        }
    }

    #[test]
    fn test_content_based_invalidation() {
        let temp_dir = TempDir::new().unwrap();
        let cache_dir = temp_dir.path().to_path_buf();

        // Create a cache with content hash enabled
        let config = CacheConfig {
            cache_dir: cache_dir.clone(),
            version: 1,
            max_entries: 10,
            max_size_bytes: 1024 * 1024,
            use_memory_mapping: false,
            verbose_logging: false,
        };

        let mut cache = PersistentCache::new(config).unwrap();
        let module_name = "test_module";

        // Create initial cache entry
        let key1 = ModuleCacheKey {
            module_name: module_name.to_string(),
            mtime: 1000,
            content_hash: Some("hash1".to_string()),
        };
        let result1 = create_test_cache_result();
        cache.insert(key1, result1).unwrap();

        // Verify cache hit with same key
        assert!(
            cache
                .get(&ModuleCacheKey {
                    module_name: module_name.to_string(),
                    mtime: 1000,
                    content_hash: Some("hash1".to_string()),
                })
                .is_some()
        );

        // Verify cache miss with different content hash
        assert!(
            cache
                .get(&ModuleCacheKey {
                    module_name: module_name.to_string(),
                    mtime: 1000,
                    content_hash: Some("hash2".to_string()),
                })
                .is_none()
        );

        // Verify cache miss with same mtime but no content hash (fallback mode)
        assert!(
            cache
                .get(&ModuleCacheKey {
                    module_name: module_name.to_string(),
                    mtime: 1000,
                    content_hash: None,
                })
                .is_none()
        );
    }

    #[test]
    fn test_cache_version_mismatch() {
        let temp_dir = TempDir::new().unwrap();
        let cache_dir = temp_dir.path().to_path_buf();

        // Create cache with version 1
        {
            let config = CacheConfig {
                cache_dir: cache_dir.clone(),
                version: 1,
                max_entries: 10,
                max_size_bytes: 1024 * 1024,
                use_memory_mapping: false,
                verbose_logging: false,
            };

            let mut cache = PersistentCache::new(config).unwrap();
            // Force persist to ensure the file is written
            cache.force_persist().unwrap();
        }

        // Verify the file exists and contains version 1
        let cache_file = cache_dir.join("analysis_cache.bin");
        assert!(cache_file.exists(), "Cache file should exist after persist");

        // Try to load with version 2
        {
            let config = CacheConfig {
                cache_dir,
                version: 2,
                max_entries: 10,
                max_size_bytes: 1024 * 1024,
                use_memory_mapping: false,
                verbose_logging: false,
            };

            // Should succeed and migrate the cache from version 1 to version 2
            let cache = PersistentCache::new(config).unwrap();
            assert_eq!(cache.metadata.version, 2);
        }
    }
}
