//! String interner for efficient string deduplication.
//!
//! Interns strings to reduce memory usage and enable fast equality checks.

use super::symbol::Symbol;
use bincode;
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// Common keywords and built-ins to pre-intern.
const COMMON_STRINGS: &[&str] = &[
    // Keywords
    "and",
    "as",
    "assert",
    "async",
    "await",
    "break",
    "class",
    "continue",
    "def",
    "del",
    "elif",
    "else",
    "except",
    "False",
    "finally",
    "for",
    "from",
    "global",
    "if",
    "import",
    "in",
    "is",
    "lambda",
    "None",
    "nonlocal",
    "not",
    "or",
    "pass",
    "raise",
    "return",
    "True",
    "try",
    "while",
    "with",
    "yield",
    // Soft keywords
    "match",
    "case",
    "type",
    // Common built-ins
    "int",
    "float",
    "str",
    "bool",
    "list",
    "dict",
    "set",
    "tuple",
    "range",
    "len",
    "print",
    "input",
    "open",
    "type",
    "isinstance",
    "hasattr",
    "getattr",
    "setattr",
    "self",
    "cls",
];

/// Statistics about interner performance.
#[derive(Debug, Clone, Copy, Default)]
pub struct InternerStats {
    /// Total number of intern calls.
    pub total_interns: u64,
    /// Number of cache hits (string already interned).
    pub cache_hits: u64,
    /// Number of cache misses (new string).
    pub cache_misses: u64,
    /// Total number of unique strings interned.
    pub unique_strings: usize,
    /// Estimated memory usage in bytes.
    pub memory_usage: usize,
}

impl InternerStats {
    /// Calculate the cache hit rate as a percentage.
    pub fn hit_rate(&self) -> f64 {
        if self.total_interns == 0 {
            0.0
        } else {
            (self.cache_hits as f64 / self.total_interns as f64) * 100.0
        }
    }
}

/// String interner for deduplicating strings.
#[derive(Serialize, Deserialize)]
pub struct Interner {
    #[serde(skip)]
    map: HashMap<String, Symbol>,
    strings: Vec<String>,
    #[serde(skip)]
    stats: InternerStats,
}

impl Interner {
    /// Create a new string interner.
    pub fn new() -> Self {
        let mut interner = Interner {
            map: HashMap::new(),
            strings: Vec::new(),
            stats: InternerStats::default(),
        };
        // Pre-intern common strings for faster lookups
        interner.pre_intern_common();
        interner
    }

    /// Pre-intern common keywords and built-ins.
    fn pre_intern_common(&mut self) {
        for s in COMMON_STRINGS {
            self.intern(s);
        }
    }

    /// Intern a string, returning its symbol.
    pub fn intern(&mut self, s: &str) -> Symbol {
        self.stats.total_interns += 1;

        if let Some(&sym) = self.map.get(s) {
            self.stats.cache_hits += 1;
            return sym;
        }

        self.stats.cache_misses += 1;
        let symbol = Symbol::new(self.strings.len() as u32);
        let string = s.to_string();
        self.stats.memory_usage += string.len() + std::mem::size_of::<String>();
        self.strings.push(string.clone());
        self.map.insert(string, symbol);
        self.stats.unique_strings = self.strings.len();
        symbol
    }

    /// Get the string for a symbol.
    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.strings
            .get(symbol.as_u32() as usize)
            .map(|s| s.as_str())
    }

    /// Get the number of interned strings.
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Check if the interner is empty.
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Rebuild the map from the strings vector.
    /// This is needed after deserialization since we skip serializing the map.
    pub fn rebuild_map(&mut self) {
        self.map.clear();
        for (idx, s) in self.strings.iter().enumerate() {
            let symbol = Symbol::new(idx as u32);
            self.map.insert(s.clone(), symbol);
        }
    }

    /// Serialize the interner to bytes.
    pub fn serialize_to_bytes(&self) -> Result<Vec<u8>, bincode::error::EncodeError> {
        bincode::serde::encode_to_vec(self, bincode::config::standard())
    }

    /// Deserialize the interner from bytes.
    pub fn deserialize_from_bytes(bytes: &[u8]) -> Result<Self, bincode::error::DecodeError> {
        let mut interner: Interner =
            bincode::serde::decode_from_slice(bytes, bincode::config::standard())?.0;
        interner.rebuild_map();
        Ok(interner)
    }

    /// Get statistics about the interner.
    pub fn stats(&self) -> InternerStats {
        self.stats
    }

    /// Reset statistics counters.
    pub fn reset_stats(&mut self) {
        self.stats.total_interns = 0;
        self.stats.cache_hits = 0;
        self.stats.cache_misses = 0;
        // Keep unique_strings and memory_usage as they reflect current state
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

/// Thread-safe string interner using RwLock for concurrent access.
pub struct ThreadSafeInterner {
    inner: Arc<RwLock<Interner>>,
}

impl ThreadSafeInterner {
    /// Create a new thread-safe interner.
    pub fn new() -> Self {
        ThreadSafeInterner {
            inner: Arc::new(RwLock::new(Interner::new())),
        }
    }

    /// Intern a string, returning its symbol.
    pub fn intern(&self, s: &str) -> Symbol {
        // Try read lock first for fast path
        {
            let mut interner = self.inner.write();
            interner.stats.total_interns += 1;

            if let Some(&sym) = interner.map.get(s) {
                interner.stats.cache_hits += 1;
                return sym;
            }

            // Not found, so insert it
            interner.stats.cache_misses += 1;
            let symbol = Symbol::new(interner.strings.len() as u32);
            let string = s.to_string();
            interner.stats.memory_usage += string.len() + std::mem::size_of::<String>();
            interner.strings.push(string.clone());
            interner.map.insert(string, symbol);
            interner.stats.unique_strings = interner.strings.len();
            symbol
        }
    }

    /// Get the string for a symbol.
    pub fn resolve(&self, symbol: Symbol) -> Option<String> {
        let interner = self.inner.read();
        interner.strings.get(symbol.as_u32() as usize).cloned()
    }

    /// Get the number of interned strings.
    pub fn len(&self) -> usize {
        self.inner.read().strings.len()
    }

    /// Check if the interner is empty.
    pub fn is_empty(&self) -> bool {
        self.inner.read().strings.is_empty()
    }

    /// Clone the underlying Arc for sharing across threads.
    pub fn clone_handle(&self) -> Self {
        ThreadSafeInterner {
            inner: Arc::clone(&self.inner),
        }
    }

    /// Get statistics about the interner.
    pub fn stats(&self) -> InternerStats {
        self.inner.read().stats()
    }

    /// Reset statistics counters.
    pub fn reset_stats(&self) {
        self.inner.write().reset_stats();
    }
}

impl Default for ThreadSafeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for ThreadSafeInterner {
    fn clone(&self) -> Self {
        self.clone_handle()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let mut interner = Interner::new();
        let sym1 = interner.intern("hello");
        let sym2 = interner.intern("hello");
        let sym3 = interner.intern("world");

        assert_eq!(sym1, sym2);
        assert_ne!(sym1, sym3);
        assert_eq!(interner.resolve(sym1), Some("hello"));
        assert_eq!(interner.resolve(sym3), Some("world"));
    }
}
