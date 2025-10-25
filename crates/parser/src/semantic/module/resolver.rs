//! Module path resolution system for Coral's Python-style module system.
//!
//! This module provides resolution for Coral's module system which follows Python
//! conventions but with .coral file extensions. Supports absolute and relative imports.

use std::path::{Path, PathBuf};

/// Errors that can occur during module path resolution
#[derive(Debug, thiserror::Error)]
pub enum ResolutionError {
    #[error("Module not found: {module_name}")]
    ModuleNotFound { module_name: String },

    #[error("Invalid module path: {path}")]
    InvalidPath { path: String },

    #[error("File system error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Relative import not allowed in package root")]
    RelativeImportInRoot,

    #[error("Circular dependency detected: {module_name}")]
    CircularDependency { module_name: String },
}

/// Result type for module path resolution
pub type ResolutionResult<T> = Result<T, ResolutionError>;

/// Trait for module path resolvers
///
/// Coral uses Python-style module resolution:
/// - Absolute: `foo.bar.baz` -> `foo/bar/baz.coral`
/// - Relative: `from . import sibling` or `from ..parent import module`
pub trait ModulePathResolver {
    /// Resolve a module name to a file path
    ///
    /// # Arguments
    /// * `module_name` - The module name to resolve (e.g., "foo.bar.baz")
    /// * `from_file` - The file from which the import is made (for relative imports)
    /// * `search_paths` - Additional search paths to check
    ///
    /// # Returns
    /// The resolved file path or an error
    fn resolve_module(
        &self,
        module_name: &str,
        from_file: Option<&Path>,
        search_paths: &[PathBuf],
    ) -> ResolutionResult<PathBuf>;

    /// Check if a module path exists and is accessible
    fn module_exists(&self, path: &Path) -> bool {
        path.exists() && path.is_file()
    }
}

/// Standard module resolver for dot-separated module names
///
/// Converts `foo.bar.baz` to `foo/bar/baz.coral`
pub struct StandardResolver {
    root_paths: Vec<PathBuf>,
}

impl StandardResolver {
    pub fn new(root_paths: Vec<PathBuf>) -> Self {
        Self { root_paths }
    }

    /// Convert module name to file path relative to a root
    fn module_name_to_path(&self, module_name: &str) -> PathBuf {
        // Replace dots with path separators
        let relative_path = module_name.replace('.', std::path::MAIN_SEPARATOR_STR);
        PathBuf::from(relative_path).with_extension("coral")
    }
}

impl ModulePathResolver for StandardResolver {
    fn resolve_module(
        &self,
        module_name: &str,
        _from_file: Option<&Path>,
        search_paths: &[PathBuf],
    ) -> ResolutionResult<PathBuf> {
        // Try all root paths and search paths
        let all_paths = self.root_paths.iter().chain(search_paths.iter());

        for root in all_paths {
            let module_path = root.join(self.module_name_to_path(module_name));
            if self.module_exists(&module_path) {
                return Ok(module_path);
            }

            // Also try as a directory with __init__.coral
            let init_file = module_path.with_file_name("__init__.coral");
            if self.module_exists(&init_file) {
                return Ok(init_file);
            }
        }

        Err(ResolutionError::ModuleNotFound {
            module_name: module_name.to_string(),
        })
    }
}

/// Relative import resolver for level-based relative imports
///
/// Coral uses level-based relative imports:
/// - level 1: current directory (.)
/// - level 2: parent directory (..)
/// - level 3: grandparent directory (...)
pub struct RelativeResolver {
    #[allow(dead_code)]
    root_paths: Vec<PathBuf>,
}

impl RelativeResolver {
    pub fn new(root_paths: Vec<PathBuf>) -> Self {
        Self { root_paths }
    }
}

impl ModulePathResolver for RelativeResolver {
    fn resolve_module(
        &self,
        module_name: &str,
        from_file: Option<&Path>,
        search_paths: &[PathBuf],
    ) -> ResolutionResult<PathBuf> {
        // For relative imports, we need the from_file and level information
        // This resolver is designed to work with Coral's level-based system
        // where the module_name is just the dotted path part (e.g., "sibling", "parent.module")
        // and the level is passed separately by the caller

        let from_file = from_file.ok_or(ResolutionError::RelativeImportInRoot)?;

        // Start from the directory containing the importing file
        let base_dir = from_file.parent().unwrap_or(from_file).to_path_buf();

        // For now, this resolver assumes the level has already been processed
        // by the caller (as seen in import_resolution.rs)
        // We just resolve the module name relative to the given base directory

        // Convert module name to path (foo.bar.baz -> foo/bar/baz)
        let module_path = if module_name.is_empty() {
            base_dir
        } else {
            let parts: Vec<&str> = module_name.split('.').collect();
            let mut path = base_dir;
            for part in parts {
                path = path.join(part);
            }
            path
        };

        // Try to find the module file as .coral
        let file_path = module_path.with_extension("coral");
        if self.module_exists(&file_path) {
            return Ok(file_path);
        }

        // Try search paths as fallback
        for search_path in search_paths {
            let search_file_path = search_path
                .join(module_name.replace('.', std::path::MAIN_SEPARATOR_STR))
                .with_extension("coral");
            if self.module_exists(&search_file_path) {
                return Ok(search_file_path);
            }
        }

        Err(ResolutionError::ModuleNotFound {
            module_name: module_name.to_string(),
        })
    }
}

/// Composite resolver that tries multiple strategies
///
/// Tries resolvers in order: standard (for absolute imports)
/// For relative imports, use RelativeResolver directly with level information
pub struct CompositeResolver {
    standard_resolver: StandardResolver,
    #[allow(dead_code)]
    relative_resolver: RelativeResolver,
}

impl CompositeResolver {
    pub fn new(root_paths: Vec<PathBuf>) -> Self {
        Self {
            standard_resolver: StandardResolver::new(root_paths.clone()),
            relative_resolver: RelativeResolver::new(root_paths),
        }
    }

    /// Resolve a relative import with level information
    ///
    /// # Arguments
    /// * `module_name` - The relative module name (e.g., "sibling", "parent.module")
    /// * `level` - Import level (1 = current dir, 2 = parent, etc.)
    /// * `from_file` - The file making the import
    /// * `search_paths` - Additional search paths
    pub fn resolve_relative(
        &self,
        module_name: &str,
        level: u32,
        from_file: &Path,
        search_paths: &[PathBuf],
    ) -> ResolutionResult<PathBuf> {
        if level == 0 {
            return Err(ResolutionError::InvalidPath {
                path: "Relative import level must be >= 1".to_string(),
            });
        }

        // Calculate the base directory based on level
        let mut base_dir = from_file
            .parent()
            .ok_or(ResolutionError::RelativeImportInRoot)?;

        // Go up 'level - 1' directories
        // level 1 = current dir, level 2 = parent, level 3 = grandparent, etc.
        for _ in 1..level {
            base_dir = base_dir.parent().ok_or(ResolutionError::InvalidPath {
                path: format!(
                    "Relative import level {} would go above package root: {}",
                    level,
                    from_file.display()
                ),
            })?;
        }

        // Convert module name to path (foo.bar.baz -> foo/bar/baz)
        let relative_path = if module_name.is_empty() {
            PathBuf::new()
        } else {
            let parts: Vec<&str> = module_name.split('.').collect();
            let mut path = PathBuf::new();
            for part in parts {
                path = path.join(part);
            }
            path
        };

        // Try module as .coral file
        let module_file = base_dir.join(&relative_path).with_extension("coral");
        if self.module_exists(&module_file) {
            return Ok(module_file);
        }

        // Try module as package directory with __init__.coral
        let init_file = base_dir.join(&relative_path).join("__init__.coral");
        if self.module_exists(&init_file) {
            return Ok(init_file);
        }

        // Try search paths as fallback
        for search_path in search_paths {
            let search_file = search_path.join(&relative_path).with_extension("coral");
            if self.module_exists(&search_file) {
                return Ok(search_file);
            }

            let search_init = search_path.join(&relative_path).join("__init__.coral");
            if self.module_exists(&search_init) {
                return Ok(search_init);
            }
        }

        Err(ResolutionError::ModuleNotFound {
            module_name: if module_name.is_empty() {
                format!("(level {})", level)
            } else {
                format!("{} (level {})", module_name, level)
            },
        })
    }
}

impl ModulePathResolver for CompositeResolver {
    fn resolve_module(
        &self,
        module_name: &str,
        from_file: Option<&Path>,
        search_paths: &[PathBuf],
    ) -> ResolutionResult<PathBuf> {
        // For absolute imports, use the standard resolver
        // Relative imports should be handled via resolve_relative method
        self.standard_resolver
            .resolve_module(module_name, from_file, search_paths)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_file(dir: &Path, name: &str, content: &str) {
        std::fs::write(dir.join(name), content).unwrap();
    }

    #[test]
    fn test_standard_resolver() {
        let temp_dir = TempDir::new().unwrap();
        create_test_file(temp_dir.path(), "foo.coral", "");
        std::fs::create_dir_all(temp_dir.path().join("foo")).unwrap();
        create_test_file(&temp_dir.path().join("foo"), "bar.coral", "");

        let resolver = StandardResolver::new(vec![temp_dir.path().to_path_buf()]);

        // Test simple module
        assert!(resolver.resolve_module("foo", None, &[]).is_ok());

        // Test nested module
        assert!(resolver.resolve_module("foo.bar", None, &[]).is_ok());

        // Test non-existent module
        assert!(resolver.resolve_module("nonexistent", None, &[]).is_err());
    }

    #[test]
    fn test_relative_resolver() {
        let temp_dir = TempDir::new().unwrap();
        let subdir = temp_dir.path().join("subdir");
        std::fs::create_dir(&subdir).unwrap();

        create_test_file(temp_dir.path(), "sibling.coral", "");
        create_test_file(&subdir, "child.coral", "");

        let composite = CompositeResolver::new(vec![temp_dir.path().to_path_buf()]);

        // Test current directory import (level 1 = current dir)
        let from_file = temp_dir.path().join("main.coral");
        assert!(
            composite
                .resolve_relative("sibling", 1, &from_file, &[])
                .is_ok()
        );

        // Test parent directory import (level 2 = parent dir)
        assert!(
            composite
                .resolve_relative("sibling", 2, &subdir.join("child.coral"), &[])
                .is_ok()
        );

        // Test invalid relative import (trying to go beyond root)
        assert!(
            composite
                .resolve_relative("nonexistent", 1, &from_file, &[])
                .is_err()
        );
    }

    #[test]
    fn test_composite_resolver() {
        let temp_dir = TempDir::new().unwrap();
        create_test_file(temp_dir.path(), "standard.coral", "");
        std::fs::create_dir_all(temp_dir.path().join("models")).unwrap();
        create_test_file(&temp_dir.path().join("models"), "user.coral", "");

        let resolver = CompositeResolver::new(vec![temp_dir.path().to_path_buf()]);

        // Test absolute imports
        assert!(resolver.resolve_module("standard", None, &[]).is_ok());
        assert!(resolver.resolve_module("models.user", None, &[]).is_ok());

        // Test relative import
        let from_file = temp_dir.path().join("main.coral");
        assert!(
            resolver
                .resolve_relative("standard", 1, &from_file, &[])
                .is_ok()
        );
    }
}
