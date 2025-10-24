//! Module loader for cross-module validation.
//!
//! This module provides functionality to load and parse modules on-demand
//! for cross-module validation, particularly for re-export validation.

use crate::ast::Module;
use crate::error::UnifiedError as Error;
use crate::semantic::module::exports::{ExportInfo, ModuleExportRegistry};
use crate::semantic::passes::module_system::ModuleSystemChecker;
use crate::semantic::passes::name_resolution::NameResolver;
use crate::semantic::types::Type;
use crate::{Arena, Lexer, Parser};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use text_size::TextRange;

/// Cached module analysis results including HIR-derived metadata
#[derive(Clone, Debug)]
pub struct ModuleAnalysisCache {
    /// Parsed AST errors (if any)
    pub errors: Vec<Error>,
    /// Module exports (export_name -> type)
    pub exports: HashMap<String, Type>,
    /// Class attributes (class_name, attr_name) -> type
    pub class_attributes: HashMap<(String, String), Type>,
    /// Class MRO (class_name -> base class list)
    pub class_mro: HashMap<String, Vec<String>>,
}

/// Module loader for cross-module validation
pub struct ModuleLoader<'a> {
    /// Root directory for module resolution
    root_dir: PathBuf,
    /// Arena for allocating AST nodes
    arena: &'a Arena,
    /// Cache of parsed modules with full analysis results
    module_cache: HashMap<PathBuf, (Module<'a>, ModuleAnalysisCache)>,
    /// Export registry tracking all module exports
    export_registry: ModuleExportRegistry,
}

impl<'a> ModuleLoader<'a> {
    /// Create a new module loader
    pub fn new(root_dir: PathBuf, arena: &'a Arena) -> Self {
        Self {
            root_dir,
            arena,
            module_cache: HashMap::new(),
            export_registry: ModuleExportRegistry::new(),
        }
    }

    /// Load and parse a module, returning the AST and any errors
    ///
    /// If the module has already been loaded, returns the cached version.
    pub fn load_module(&mut self, module_path: &Path) -> Result<&Module<'a>, Vec<Error>> {
        // Check cache first
        if self.module_cache.contains_key(module_path) {
            let (module, cache) = self.module_cache.get(module_path).unwrap();
            if cache.errors.is_empty() {
                return Ok(module);
            } else {
                return Err(cache.errors.clone());
            }
        }

        // Read and parse the module
        let source = std::fs::read_to_string(module_path).map_err(|_e| {
            vec![*crate::error::error(
                crate::error::UnifiedErrorKind::ModuleNotFound {
                    module_name: module_path.display().to_string(),
                },
                TextRange::default(),
            )]
        })?;

        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, self.arena);
        let module = parser.parse_module().map_err(|e| vec![*e])?;

        // Run name resolution
        let mut name_resolver = NameResolver::new();
        name_resolver.resolve_module(module);
        let (symbol_table, _name_errors) = name_resolver.into_symbol_table();

        // Validate module system
        let module_name = self.module_path_to_name(module_path);
        let checker = ModuleSystemChecker::with_registry(
            &symbol_table,
            &self.export_registry,
            &module_name,
            10, // Default max depth
        );
        let errors = checker.check(module);

        // Extract and register exports from this module
        let exports = self.extract_exports_map(module, &module_name);

        // Register exports in the registry
        for (export_name, export_type) in &exports {
            self.export_registry.register_export(
                &module_name,
                export_name.clone(),
                ExportInfo {
                    original_name: export_name.clone(),
                    ty: export_type.clone(),
                    source_module: None,
                    reexport_chain: Vec::new(),
                    span: TextRange::default(),
                },
            );
        }

        // Run HIR lowering to extract class metadata
        let (class_attributes, class_mro) = self.extract_class_metadata(module);

        // Create analysis cache with extracted metadata
        let analysis_cache = ModuleAnalysisCache {
            errors: errors.clone(),
            exports,
            class_attributes,
            class_mro,
        };

        // Cache the result with analysis metadata
        self.module_cache
            .insert(module_path.to_path_buf(), (module.clone(), analysis_cache));

        if errors.is_empty() {
            Ok(self.module_cache.get(module_path).map(|(m, _)| m).unwrap())
        } else {
            Err(errors)
        }
    }

    /// Get cached analysis results for a module
    pub fn get_cached_analysis(&self, module_path: &Path) -> Option<&ModuleAnalysisCache> {
        self.module_cache.get(module_path).map(|(_, cache)| cache)
    }

    /// Extract exports from a module and register them
    /// Extract exports from module as a HashMap for caching
    fn extract_exports_map(
        &self,
        module: &Module<'a>,
        _module_name: &str,
    ) -> HashMap<String, Type> {
        use crate::ast::Stmt;
        let mut exports = HashMap::new();

        for stmt in module.body {
            if let Stmt::Export(export) = stmt {
                // Handle regular exports
                if export.module.is_none() {
                    for (name, alias) in export.names {
                        let exported_name = alias.unwrap_or(name);
                        // Type inference would provide actual type, for now use Unknown
                        exports.insert(exported_name.to_string(), Type::Unknown);
                    }
                }
            }
        }
        exports
    }

    /// Extract class attributes and MRO by running HIR lowering
    #[allow(clippy::type_complexity)]
    fn extract_class_metadata(
        &self,
        module: &Module<'a>,
    ) -> (
        HashMap<(String, String), Type>,
        HashMap<String, Vec<String>>,
    ) {
        use crate::arena::Interner;
        use crate::semantic::hir::HirLowerer;

        // Create a temporary interner for HIR lowering
        let mut interner = Interner::new();
        let mut lowerer = HirLowerer::new(self.arena, &mut interner);

        // Run HIR lowering and extract class information
        match lowerer.lower_module(module) {
            Ok(_typed_module) => {
                // Extract comprehensive metadata from class analyzer
                let class_analyzer = lowerer.into_class_analyzer();
                let metadata_map = class_analyzer.export_metadata();

                // Convert to flat structure for compatibility
                let mut class_attributes = HashMap::new();
                let mut class_mro = HashMap::new();

                for (class_name, metadata) in metadata_map {
                    // Store MRO
                    class_mro.insert(class_name.clone(), metadata.mro.clone());

                    // Flatten all attributes with priority-based merging
                    // Priority: Properties > Methods > Class Attrs > Instance Attrs
                    for (prop_name, prop_desc) in &metadata.properties {
                        let key = (class_name.clone(), prop_name.clone());
                        let attr_type = crate::semantic::types::Type::AttributeDescriptor {
                            kind: crate::semantic::types::AttributeKind::Property,
                            getter_type: Box::new(prop_desc.getter_type.clone()),
                            setter_type: prop_desc
                                .setter_type
                                .as_ref()
                                .map(|t| Box::new(t.clone())),
                        };
                        class_attributes.insert(key, attr_type);
                    }

                    for (method_name, method_type) in &metadata.methods {
                        let key = (class_name.clone(), method_name.clone());
                        class_attributes
                            .entry(key)
                            .or_insert_with(|| method_type.clone());
                    }

                    for (attr_name, attr_type) in &metadata.class_attributes {
                        let key = (class_name.clone(), attr_name.clone());
                        class_attributes
                            .entry(key)
                            .or_insert_with(|| attr_type.clone());
                    }

                    for (attr_name, attr_type) in &metadata.instance_attributes {
                        let key = (class_name.clone(), attr_name.clone());
                        class_attributes
                            .entry(key)
                            .or_insert_with(|| attr_type.clone());
                    }

                    if let Some(constructor_type) = &metadata.constructor {
                        let key = (class_name.clone(), "constructor".to_string());
                        class_attributes.insert(key, constructor_type.clone());
                    }
                }

                (class_attributes, class_mro)
            }
            Err(_errors) => {
                // If HIR lowering fails, return empty maps
                (HashMap::new(), HashMap::new())
            }
        }
    }

    #[allow(dead_code)]
    fn extract_and_register_exports(&mut self, module: &Module<'a>, module_name: &str) {
        use crate::ast::Stmt;

        for stmt in module.body {
            if let Stmt::Export(export) = stmt {
                // Handle regular exports
                if export.module.is_none() {
                    for (name, alias) in export.names {
                        let exported_name = alias.unwrap_or(name);
                        let info = ExportInfo {
                            original_name: name.to_string(),
                            ty: Type::Unknown, // Type inference would provide actual type
                            source_module: None,
                            reexport_chain: Vec::new(),
                            span: export.span,
                        };
                        self.export_registry.register_export(
                            module_name,
                            exported_name.to_string(),
                            info,
                        );
                    }
                }
                // Re-exports are handled by looking up in the registry
                // during validation
            }
        }
    }

    /// Convert a module file path to a module name
    fn module_path_to_name(&self, path: &Path) -> String {
        path.strip_prefix(&self.root_dir)
            .unwrap_or(path)
            .with_extension("")
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, ".")
    }

    /// Get the export registry
    pub fn export_registry(&self) -> &ModuleExportRegistry {
        &self.export_registry
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_module_loader_basic() {
        let temp_dir = TempDir::new().unwrap();
        let module_path = temp_dir.path().join("test.coral");

        // Create a simple module
        std::fs::write(
            &module_path,
            r#"
def my_function():
    return 42

export my_function
"#,
        )
        .unwrap();

        let arena = Arena::new();
        let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

        // Load the module
        let result = loader.load_module(&module_path);
        assert!(result.is_ok());

        // Check that exports were registered
        let registry = loader.export_registry();
        assert!(registry.is_exported("test", "my_function"));
    }

    #[test]
    fn test_module_loader_caching() {
        let temp_dir = TempDir::new().unwrap();
        let module_path = temp_dir.path().join("cached.coral");

        std::fs::write(&module_path, "x = 42\nexport x").unwrap();

        let arena = Arena::new();
        let mut loader = ModuleLoader::new(temp_dir.path().to_path_buf(), &arena);

        // Load twice - second should come from cache
        let result1 = loader.load_module(&module_path).is_ok();
        let result2 = loader.load_module(&module_path).is_ok();
        let cache_len = loader.module_cache.len();

        assert!(result1);
        assert!(result2);
        assert_eq!(cache_len, 1);
    }
}
