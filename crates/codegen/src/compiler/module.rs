//! Module compilation orchestration with import/export resolution

use crate::bytecode::{
    BytecodeModule, ExportEntry, ExportKind, ExportTable, ImportEntry, ImportTable,
};
use crate::compiler::context::CompilationContext;
use crate::compiler::stmt::StmtCompiler;
use crate::error::{CodegenError, CodegenResult};
use crate::{IntrinsicRegistry, NativeModuleRegistry};
use coral_parser::ast::{Module, Stmt};
use coral_parser::ParseResultWithMetadata;
use std::collections::HashMap;

pub struct ModuleCompiler {
    module_name: String,
    intrinsic_registry: IntrinsicRegistry,
    native_registry: NativeModuleRegistry,
    import_map: HashMap<String, u16>,
    next_module_id: u16,
}

impl ModuleCompiler {
    pub fn new(module_name: String) -> Self {
        Self {
            module_name,
            intrinsic_registry: IntrinsicRegistry::new(),
            native_registry: NativeModuleRegistry::new(),
            import_map: HashMap::new(),
            next_module_id: 1,
        }
    }

    /// Compile from a complete ParseResult with semantic analysis
    pub fn compile(parse_result: &ParseResultWithMetadata) -> CodegenResult<BytecodeModule> {
        let mut compiler = Self::new("main".to_string());
        compiler.compile_from_parse_result(parse_result)
    }

    /// Compile from the ParseResult with semantic analysis
    pub fn compile_from_parse_result(&mut self, parse_result: &ParseResultWithMetadata) -> CodegenResult<BytecodeModule> {
        let mut bytecode_module = BytecodeModule::new(self.module_name.clone());
        let mut ctx = CompilationContext::with_parser_symbols(
            256,
            Some(parse_result.symbol_table.clone()),
            Some(parse_result.ownership_recommendations.clone()),
        );

        self.process_imports_and_exports(parse_result.module, &mut bytecode_module)?;

        for stmt in parse_result.module.body {
            StmtCompiler::compile(&mut ctx, stmt)?;
        }

        bytecode_module
            .functions
            .extend(ctx.symbol_table.keys().map(|name| {
                let mut func = crate::bytecode::Function::new(
                    name.clone(),
                    ctx.register_allocator.count(),
                    vec![],
                    0,
                );
                func.instructions = ctx.instructions.clone();
                func
            }));

        bytecode_module.constants = ctx.constant_pool;

        Ok(bytecode_module)
    }

    /// Compile from raw AST (legacy, for backwards compatibility during refactoring)
    pub fn compile_module(&mut self, module: &Module) -> CodegenResult<BytecodeModule> {
        let mut bytecode_module = BytecodeModule::new(self.module_name.clone());
        let mut ctx = CompilationContext::new(256);

        self.process_imports_and_exports(module, &mut bytecode_module)?;

        for stmt in module.body {
            StmtCompiler::compile(&mut ctx, stmt)?;
        }

        bytecode_module
            .functions
            .extend(ctx.symbol_table.keys().map(|name| {
                let mut func = crate::bytecode::Function::new(
                    name.clone(),
                    ctx.register_allocator.count(),
                    vec![],
                    0,
                );
                func.instructions = ctx.instructions.clone();
                func
            }));

        bytecode_module.constants = ctx.constant_pool;

        Ok(bytecode_module)
    }

    fn process_imports_and_exports(
        &mut self,
        module: &Module,
        bytecode_module: &mut BytecodeModule,
    ) -> CodegenResult<()> {
        let mut import_table = ImportTable::new();
        let mut export_table = ExportTable::new();

        for stmt in module.body {
            match stmt {
                Stmt::From(from_stmt) => {
                    self.process_from_import(from_stmt, &mut import_table)?;
                }
                Stmt::Import(import_stmt) => {
                    self.process_import(import_stmt, &mut import_table)?;
                }
                Stmt::Export(export_stmt) => {
                    self.process_export(export_stmt, &mut export_table)?;
                }
                Stmt::FuncDef(func_def) => {
                    let is_exported = func_def.name.starts_with("pub_");
                    if is_exported {
                        export_table.entries.insert(
                            func_def.name.to_string(),
                            ExportEntry {
                                name: func_def.name.to_string(),
                                type_id: 0,
                                kind: ExportKind::Function,
                            },
                        );
                    }
                }
                Stmt::ClassDef(class_def) => {
                    let is_exported = class_def.name.starts_with("pub_");
                    if is_exported {
                        export_table.entries.insert(
                            class_def.name.to_string(),
                            ExportEntry {
                                name: class_def.name.to_string(),
                                type_id: 0,
                                kind: ExportKind::Class,
                            },
                        );
                    }
                }
                _ => {}
            }
        }

        bytecode_module.imports = import_table;
        bytecode_module.exports = export_table;

        Ok(())
    }

    fn process_from_import(
        &mut self,
        from_stmt: &coral_parser::ast::FromStmt,
        import_table: &mut ImportTable,
    ) -> CodegenResult<()> {
        let module_name = from_stmt.module.unwrap_or("").to_string();
        if module_name.is_empty() {
            return Ok(());
        }

        let is_native = self.native_registry.is_native_module(&module_name);

        let symbols: Vec<(String, u32)> = from_stmt
            .names
            .iter()
            .map(|(name, _alias)| (name.to_string(), 0))
            .collect();

        let module_id = if is_native {
            self.register_native_module(&module_name)?
        } else {
            self.get_or_register_module(&module_name)?
        };

        if let Some(entry) = import_table.entries.get_mut(&module_name) {
            entry.symbols.extend(symbols);
        } else {
            import_table.entries.insert(
                module_name.clone(),
                ImportEntry {
                    module_name,
                    symbols,
                    module_id,
                    is_native,
                },
            );
        }

        Ok(())
    }

    fn process_import(
        &mut self,
        import_stmt: &coral_parser::ast::ImportStmt,
        import_table: &mut ImportTable,
    ) -> CodegenResult<()> {
        for (name, _alias) in import_stmt.names {
            let module_name = name.to_string();
            let is_native = self.native_registry.is_native_module(&module_name);

            let module_id = if is_native {
                self.register_native_module(&module_name)?
            } else {
                self.get_or_register_module(&module_name)?
            };

            import_table.entries.insert(
                module_name.clone(),
                ImportEntry {
                    module_name,
                    symbols: vec![],
                    module_id,
                    is_native,
                },
            );
        }

        Ok(())
    }

    fn process_export(
        &mut self,
        export_stmt: &coral_parser::ast::ExportStmt,
        export_table: &mut ExportTable,
    ) -> CodegenResult<()> {
        for (name, _alias) in export_stmt.names {
            export_table.entries.insert(
                name.to_string(),
                ExportEntry {
                    name: name.to_string(),
                    type_id: 0,
                    kind: ExportKind::Global,
                },
            );
        }

        Ok(())
    }

    fn register_native_module(&mut self, module_name: &str) -> CodegenResult<u16> {
        if let Some(&module_id) = self.import_map.get(module_name) {
            return Ok(module_id);
        }

        if !self.native_registry.is_native_module(module_name) {
            return Err(CodegenError::ModuleError(format!(
                "Unknown native module: {}",
                module_name
            )));
        }

        let module_id = self.next_module_id;
        self.next_module_id += 1;
        self.import_map.insert(module_name.to_string(), module_id);

        Ok(module_id)
    }

    fn get_or_register_module(&mut self, module_name: &str) -> CodegenResult<u16> {
        if let Some(&module_id) = self.import_map.get(module_name) {
            return Ok(module_id);
        }

        let module_id = self.next_module_id;
        self.next_module_id += 1;
        self.import_map.insert(module_name.to_string(), module_id);

        Ok(module_id)
    }

    pub fn is_intrinsic(&self, name: &str) -> bool {
        self.intrinsic_registry.is_intrinsic(name)
    }

    pub fn get_module_id(&self, module_name: &str) -> Option<u16> {
        self.import_map.get(module_name).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_compiler_new() {
        let compiler = ModuleCompiler::new("test".to_string());
        assert_eq!(compiler.module_name, "test");
        assert_eq!(compiler.next_module_id, 1);
    }

    #[test]
    fn test_intrinsic_detection() {
        let compiler = ModuleCompiler::new("main".to_string());
        assert!(compiler.is_intrinsic("print"));
        assert!(compiler.is_intrinsic("len"));
        assert!(!compiler.is_intrinsic("nonexistent"));
    }

    #[test]
    fn test_register_native_module() {
        let mut compiler = ModuleCompiler::new("main".to_string());
        let id1 = compiler.register_native_module("math").unwrap();
        assert_eq!(id1, 1);

        let id2 = compiler.register_native_module("json").unwrap();
        assert_eq!(id2, 2);

        let id1_again = compiler.register_native_module("math").unwrap();
        assert_eq!(id1, id1_again);
    }

    #[test]
    fn test_get_module_id() {
        let mut compiler = ModuleCompiler::new("main".to_string());
        compiler.register_native_module("math").unwrap();

        assert_eq!(compiler.get_module_id("math"), Some(1));
        assert_eq!(compiler.get_module_id("nonexistent"), None);
    }
}
