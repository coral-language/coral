//! Module export registry for Coral's explicit export system.
//!
//! Coral uses an explicit export system where names are private by default
//! and must be explicitly exported using the `export` keyword.

use crate::semantic::types::Type;
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Registry of exported symbols from modules
#[derive(Debug, Clone)]
pub struct ModuleExportRegistry {
    /// Maps module_name -> (exported_name -> ExportInfo)
    exports: HashMap<String, HashMap<String, ExportInfo>>,
}

/// Information about an exported symbol
#[derive(Debug, Clone)]
pub struct ExportInfo {
    /// The original name (before any aliasing with 'as')
    pub original_name: String,
    /// The type of the exported symbol
    pub ty: Type,
    /// Source module if this is a re-export (export Name from module)
    pub source_module: Option<String>,
    /// Track full re-export chain for validation (e.g., ["A", "B", "C"] means A -> B -> C)
    pub reexport_chain: Vec<String>,
    /// Span for error reporting
    pub span: TextRange,
}

impl ModuleExportRegistry {
    /// Create a new empty export registry
    pub fn new() -> Self {
        Self {
            exports: HashMap::new(),
        }
    }

    /// Register an export from a module
    ///
    /// # Arguments
    /// * `module_name` - The module exporting the symbol
    /// * `exported_name` - The name as it will be visible to importers
    /// * `info` - Information about the exported symbol
    pub fn register_export(&mut self, module_name: &str, exported_name: String, info: ExportInfo) {
        self.exports
            .entry(module_name.to_string())
            .or_default()
            .insert(exported_name, info);
    }

    /// Get information about an exported symbol
    ///
    /// # Arguments
    /// * `module_name` - The module to query
    /// * `name` - The exported name to look up
    ///
    /// # Returns
    /// Information about the export if it exists
    pub fn get_export(&self, module_name: &str, name: &str) -> Option<&ExportInfo> {
        self.exports
            .get(module_name)
            .and_then(|module_exports| module_exports.get(name))
    }

    /// Check if a name is exported from a module
    ///
    /// # Arguments
    /// * `module_name` - The module to check
    /// * `name` - The name to check
    ///
    /// # Returns
    /// True if the name is exported
    pub fn is_exported(&self, module_name: &str, name: &str) -> bool {
        self.get_export(module_name, name).is_some()
    }

    /// Get all exported names from a module
    ///
    /// # Arguments
    /// * `module_name` - The module to query
    ///
    /// # Returns
    /// A vector of all exported names (useful for error suggestions)
    pub fn get_all_exports(&self, module_name: &str) -> Vec<String> {
        self.exports
            .get(module_name)
            .map(|exports| exports.keys().cloned().collect())
            .unwrap_or_default()
    }

    /// Get all module names that have exports registered
    pub fn get_all_modules(&self) -> Vec<String> {
        self.exports.keys().cloned().collect()
    }

    /// Clear all exports for a module (useful for incremental compilation)
    pub fn clear_module_exports(&mut self, module_name: &str) {
        self.exports.remove(module_name);
    }

    /// Get the number of exports in a module
    pub fn export_count(&self, module_name: &str) -> usize {
        self.exports
            .get(module_name)
            .map(|exports| exports.len())
            .unwrap_or(0)
    }

    /// Resolve a re-export chain to find the original export
    ///
    /// Returns (origin_module, export_info, chain) or error if not found/circular
    ///
    /// # Arguments
    /// * `module_name` - Starting module name
    /// * `name` - Export name to resolve
    /// * `max_depth` - Maximum chain depth to prevent infinite loops
    ///
    /// # Returns
    /// Result with (origin_module, final_export_info, full_chain) or ReexportError
    pub fn resolve_reexport_chain(
        &self,
        module_name: &str,
        name: &str,
        max_depth: usize,
    ) -> Result<(String, ExportInfo, Vec<String>), ReexportError> {
        let mut visited = HashSet::new();
        let mut chain = vec![module_name.to_string()];
        let mut current_module = module_name;
        let mut current_name = name;

        loop {
            // Check depth limit
            if chain.len() > max_depth {
                return Err(ReexportError::ChainTooDeep {
                    max_depth,
                    chain: chain.clone(),
                });
            }

            // Check for circular chains
            if !visited.insert(current_module.to_string()) {
                return Err(ReexportError::CircularChain {
                    chain: chain.clone(),
                });
            }

            // Look up current export
            if let Some(info) = self.get_export(current_module, current_name) {
                if let Some(source) = &info.source_module {
                    // This is a re-export, follow the chain
                    chain.push(source.clone());
                    current_module = source;
                    current_name = &info.original_name;
                } else {
                    // Found the original export
                    let mut final_info = info.clone();
                    final_info.reexport_chain = chain.clone();
                    return Ok((current_module.to_string(), final_info, chain));
                }
            } else {
                return Err(ReexportError::NotFound {
                    name: current_name.to_string(),
                    module: current_module.to_string(),
                });
            }
        }
    }
}

/// Errors that can occur when resolving re-export chains
#[derive(Debug, Clone)]
pub enum ReexportError {
    /// The export name was not found in the specified module
    NotFound { name: String, module: String },
    /// A circular re-export chain was detected
    CircularChain { chain: Vec<String> },
    /// The re-export chain exceeded the maximum depth
    ChainTooDeep {
        max_depth: usize,
        chain: Vec<String>,
    },
}

impl Default for ModuleExportRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use text_size::TextSize;

    fn make_span() -> TextRange {
        TextRange::new(TextSize::from(0), TextSize::from(10))
    }

    #[test]
    fn test_register_and_get_export() {
        let mut registry = ModuleExportRegistry::new();

        let info = ExportInfo {
            original_name: "my_func".to_string(),
            ty: Type::function(vec![Type::Int], Type::Str),
            source_module: None,
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        registry.register_export("mymodule", "my_func".to_string(), info);

        assert!(registry.is_exported("mymodule", "my_func"));
        assert!(!registry.is_exported("mymodule", "other_func"));

        let export = registry.get_export("mymodule", "my_func").unwrap();
        assert_eq!(export.original_name, "my_func");
    }

    #[test]
    fn test_export_with_alias() {
        let mut registry = ModuleExportRegistry::new();

        let info = ExportInfo {
            original_name: "internal_name".to_string(),
            ty: Type::Int,
            source_module: None,
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        registry.register_export("mymodule", "public_name".to_string(), info);

        // Should be accessible by the public name
        assert!(registry.is_exported("mymodule", "public_name"));
        // But not by the original name
        assert!(!registry.is_exported("mymodule", "internal_name"));

        let export = registry.get_export("mymodule", "public_name").unwrap();
        assert_eq!(export.original_name, "internal_name");
    }

    #[test]
    fn test_re_export() {
        let mut registry = ModuleExportRegistry::new();

        let info = ExportInfo {
            original_name: "User".to_string(),
            ty: Type::Class("User".to_string()),
            source_module: Some("models.user".to_string()),
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        registry.register_export("api", "User".to_string(), info);

        let export = registry.get_export("api", "User").unwrap();
        assert_eq!(export.source_module, Some("models.user".to_string()));
    }

    #[test]
    fn test_get_all_exports() {
        let mut registry = ModuleExportRegistry::new();

        let info1 = ExportInfo {
            original_name: "func1".to_string(),
            ty: Type::function(vec![], Type::None),
            source_module: None,
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        let info2 = ExportInfo {
            original_name: "func2".to_string(),
            ty: Type::function(vec![], Type::None),
            source_module: None,
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        registry.register_export("mymodule", "func1".to_string(), info1);
        registry.register_export("mymodule", "func2".to_string(), info2);

        let exports = registry.get_all_exports("mymodule");
        assert_eq!(exports.len(), 2);
        assert!(exports.contains(&"func1".to_string()));
        assert!(exports.contains(&"func2".to_string()));
    }

    #[test]
    fn test_clear_module_exports() {
        let mut registry = ModuleExportRegistry::new();

        let info = ExportInfo {
            original_name: "func".to_string(),
            ty: Type::function(vec![], Type::None),
            source_module: None,
            reexport_chain: Vec::new(),
            span: make_span(),
        };

        registry.register_export("mymodule", "func".to_string(), info);
        assert!(registry.is_exported("mymodule", "func"));

        registry.clear_module_exports("mymodule");
        assert!(!registry.is_exported("mymodule", "func"));
    }
}
