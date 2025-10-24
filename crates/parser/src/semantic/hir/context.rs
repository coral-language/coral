//! HIR context for storing and managing HIR data

use crate::arena::symbol::Symbol;
use crate::semantic::symbol::SymbolTable;
use crate::semantic::types::Type;

use super::class_analysis::ClassAnalyzer;
use super::typed_stmt::TypedStmt;
use text_size::TextRange;

/// HIR context that stores the lowered HIR and related metadata
pub struct HirContext<'a> {
    /// The lowered HIR module
    pub module: TypedModule<'a>,
    /// Symbol table with resolved names
    pub symbol_table: SymbolTable,
    /// Type inference context
    pub type_context: Type,
    /// Class hierarchy analyzer
    pub class_analyzer: ClassAnalyzer<'a>,
    /// Source code for error reporting
    pub source: &'a str,
}

/// Typed module (simplified for context)
#[derive(Debug, Clone)]
pub struct TypedModule<'a> {
    pub name: Symbol,
    pub body: &'a [TypedStmt<'a>],
    pub imports: &'a [TypedImport<'a>],
    pub exports: &'a [TypedExport<'a>],
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

/// Typed import
#[derive(Debug, Clone)]
pub struct TypedImport<'a> {
    pub module: Option<Symbol>,
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub level: u32,                            // For relative imports
    pub span: TextRange,
}

/// Typed export
#[derive(Debug, Clone)]
pub struct TypedExport<'a> {
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub module: Option<Symbol>,                // For re-exports
    pub span: TextRange,
}

impl<'a> HirContext<'a> {
    /// Create a new HIR context
    pub fn new(
        module: TypedModule<'a>,
        symbol_table: SymbolTable,
        type_context: Type,
        class_analyzer: ClassAnalyzer<'a>,
        source: &'a str,
    ) -> Self {
        Self {
            module,
            symbol_table,
            type_context,
            class_analyzer,
            source,
        }
    }

    /// Get the module name
    pub fn module_name(&self) -> Symbol {
        self.module.name
    }

    /// Get the module body
    pub fn module_body(&self) -> &[TypedStmt<'a>] {
        self.module.body
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    /// Get the class analyzer
    pub fn class_analyzer(&self) -> &ClassAnalyzer<'a> {
        &self.class_analyzer
    }

    /// Get the source code
    pub fn source(&self) -> &str {
        self.source
    }

    /// Look up a symbol by name
    pub fn lookup_symbol(&self, name: &str) -> Option<(crate::semantic::symbol::Symbol, usize)> {
        self.symbol_table.lookup(name)
    }

    /// Get the type of a symbol
    pub fn get_symbol_type(&self, name: &str) -> Option<Type> {
        self.symbol_table.get_symbol_type(name)
    }

    /// Get the MRO for a class
    pub fn get_class_mro(&self, class_name: Symbol) -> Option<&[Symbol]> {
        self.class_analyzer.get_mro(class_name)
    }

    /// Get the attributes for a class
    pub fn get_class_attributes(&self, class_name: Symbol) -> Option<&[(Symbol, Type)]> {
        self.class_analyzer.get_attributes(class_name)
    }

    /// Get the methods for a class
    pub fn get_class_methods(&self, class_name: Symbol) -> Option<&[(Symbol, Type)]> {
        self.class_analyzer.get_methods(class_name)
    }

    /// Find all function definitions in the module
    pub fn find_functions(&self) -> Vec<&super::typed_stmt::TypedFuncDefStmt<'a>> {
        let mut functions = Vec::new();
        self.collect_functions_from_body(self.module.body, &mut functions);
        functions
    }

    /// Find all class definitions in the module
    pub fn find_classes(&self) -> Vec<&super::typed_stmt::TypedClassDefStmt<'a>> {
        let mut classes = Vec::new();
        self.collect_classes_from_body(self.module.body, &mut classes);
        classes
    }

    /// Find all imports in the module
    pub fn find_imports(&self) -> &[TypedImport<'a>] {
        self.module.imports
    }

    /// Find all exports in the module
    pub fn find_exports(&self) -> &[TypedExport<'a>] {
        self.module.exports
    }

    /// Collect functions from statement body
    fn collect_functions_from_body<'b>(
        &self,
        body: &'b [TypedStmt<'a>],
        functions: &mut Vec<&'b super::typed_stmt::TypedFuncDefStmt<'a>>,
    ) {
        for stmt in body {
            match stmt {
                TypedStmt::FuncDef(func) => {
                    functions.push(func);
                }
                _ => {
                    self.collect_functions_from_nested_stmt(stmt, functions);
                }
            }
        }
    }

    /// Collect classes from statement body
    fn collect_classes_from_body<'b>(
        &self,
        body: &'b [TypedStmt<'a>],
        classes: &mut Vec<&'b super::typed_stmt::TypedClassDefStmt<'a>>,
    ) {
        for stmt in body {
            match stmt {
                TypedStmt::ClassDef(class) => {
                    classes.push(class);
                }
                _ => {
                    self.collect_classes_from_nested_stmt(stmt, classes);
                }
            }
        }
    }

    /// Collect functions from nested statements
    fn collect_functions_from_nested_stmt<'b>(
        &self,
        stmt: &'b TypedStmt<'a>,
        functions: &mut Vec<&'b super::typed_stmt::TypedFuncDefStmt<'a>>,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_functions_from_body(if_stmt.body, functions);
                self.collect_functions_from_body(if_stmt.orelse, functions);
            }
            TypedStmt::While(while_stmt) => {
                self.collect_functions_from_body(while_stmt.body, functions);
                self.collect_functions_from_body(while_stmt.orelse, functions);
            }
            TypedStmt::For(for_stmt) => {
                self.collect_functions_from_body(for_stmt.body, functions);
                self.collect_functions_from_body(for_stmt.orelse, functions);
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_functions_from_body(try_stmt.body, functions);
                self.collect_functions_from_body(try_stmt.orelse, functions);
                self.collect_functions_from_body(try_stmt.finalbody, functions);
                for handler in try_stmt.handlers {
                    self.collect_functions_from_body(handler.body, functions);
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_functions_from_body(with_stmt.body, functions);
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_functions_from_body(case.body, functions);
                }
            }
            _ => {}
        }
    }

    /// Collect classes from nested statements
    fn collect_classes_from_nested_stmt<'b>(
        &self,
        stmt: &'b TypedStmt<'a>,
        classes: &mut Vec<&'b super::typed_stmt::TypedClassDefStmt<'a>>,
    ) {
        match stmt {
            TypedStmt::If(if_stmt) => {
                self.collect_classes_from_body(if_stmt.body, classes);
                self.collect_classes_from_body(if_stmt.orelse, classes);
            }
            TypedStmt::While(while_stmt) => {
                self.collect_classes_from_body(while_stmt.body, classes);
                self.collect_classes_from_body(while_stmt.orelse, classes);
            }
            TypedStmt::For(for_stmt) => {
                self.collect_classes_from_body(for_stmt.body, classes);
                self.collect_classes_from_body(for_stmt.orelse, classes);
            }
            TypedStmt::Try(try_stmt) => {
                self.collect_classes_from_body(try_stmt.body, classes);
                self.collect_classes_from_body(try_stmt.orelse, classes);
                self.collect_classes_from_body(try_stmt.finalbody, classes);
                for handler in try_stmt.handlers {
                    self.collect_classes_from_body(handler.body, classes);
                }
            }
            TypedStmt::With(with_stmt) => {
                self.collect_classes_from_body(with_stmt.body, classes);
            }
            TypedStmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_classes_from_body(case.body, classes);
                }
            }
            _ => {}
        }
    }
}

pub use super::typed_stmt::{TypedClassDefStmt, TypedFuncDefStmt};
