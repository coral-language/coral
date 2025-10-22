//! Module system semantic analysis
//!
//! This pass validates:
//! - Export statements (names must be defined before export)
//! - Module introspection calls (only valid function names)
//! - Re-exports (module names should exist)
//! - Duplicate exports

use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::symbol::table::SymbolTable;
use text_size::TextRange;

/// Valid module introspection function names
const VALID_INTROSPECTION_FUNCTIONS: &[&str] = &["is_main", "name", "path"];

pub struct ModuleSystemChecker<'a> {
    symbol_table: &'a SymbolTable,
    errors: Vec<Error>,
    exported_names: Vec<(&'a str, TextRange)>, // Track exports to detect duplicates
}

impl<'a> ModuleSystemChecker<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            symbol_table,
            errors: Vec::new(),
            exported_names: Vec::new(),
        }
    }

    pub fn check(mut self, module: &Module<'a>) -> Vec<Error> {
        self.check_module(module);
        self.errors
    }

    fn check_module(&mut self, module: &Module<'a>) {
        for stmt in module.body {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::Export(export) => {
                self.check_export(export);
            }
            Stmt::If(if_stmt) => {
                self.check_expr(&if_stmt.test);
                for s in if_stmt.body {
                    self.check_stmt(s);
                }
                for s in if_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::While(while_stmt) => {
                self.check_expr(&while_stmt.test);
                for s in while_stmt.body {
                    self.check_stmt(s);
                }
                for s in while_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::For(for_stmt) => {
                self.check_expr(&for_stmt.target);
                self.check_expr(&for_stmt.iter);
                for s in for_stmt.body {
                    self.check_stmt(s);
                }
                for s in for_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::FuncDef(func) => {
                for s in func.body {
                    self.check_stmt(s);
                }
            }
            Stmt::ClassDef(class) => {
                for s in class.body {
                    self.check_stmt(s);
                }
            }
            Stmt::Try(try_stmt) => {
                for s in try_stmt.body {
                    self.check_stmt(s);
                }
                for handler in try_stmt.handlers {
                    for s in handler.body {
                        self.check_stmt(s);
                    }
                }
                for s in try_stmt.orelse {
                    self.check_stmt(s);
                }
                for s in try_stmt.finalbody {
                    self.check_stmt(s);
                }
            }
            Stmt::With(with_stmt) => {
                for item in with_stmt.items {
                    self.check_expr(&item.context_expr);
                }
                for s in with_stmt.body {
                    self.check_stmt(s);
                }
            }
            Stmt::Match(match_stmt) => {
                self.check_expr(&match_stmt.subject);
                for case in match_stmt.cases {
                    for s in case.body {
                        self.check_stmt(s);
                    }
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.check_expr(&expr_stmt.value);
            }
            Stmt::Assign(assign) => {
                self.check_expr(&assign.value);
            }
            Stmt::AnnAssign(ann_assign) => {
                if let Some(ref value) = ann_assign.value {
                    self.check_expr(value);
                }
            }
            Stmt::AugAssign(aug_assign) => {
                self.check_expr(&aug_assign.value);
            }
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {
                    self.check_expr(value);
                }
            }
            Stmt::Raise(raise) => {
                if let Some(ref exc) = raise.exc {
                    self.check_expr(exc);
                }
            }
            Stmt::Assert(assert_stmt) => {
                self.check_expr(&assert_stmt.test);
            }
            Stmt::Delete(delete) => {
                for target in delete.targets {
                    self.check_expr(target);
                }
            }
            _ => {}
        }
    }

    fn check_expr(&mut self, expr: &Expr<'a>) {
        match expr {
            Expr::ModuleIntrospection(intro) => {
                self.check_module_introspection(intro);
            }
            Expr::BinOp(binop) => {
                self.check_expr(binop.left);
                self.check_expr(binop.right);
            }
            Expr::UnaryOp(unary) => {
                self.check_expr(unary.operand);
            }
            Expr::Compare(cmp) => {
                self.check_expr(cmp.left);
                for comp in cmp.comparators {
                    self.check_expr(comp);
                }
            }
            Expr::Call(call) => {
                self.check_expr(call.func);
                for arg in call.args {
                    self.check_expr(arg);
                }
            }
            Expr::Attribute(attr) => {
                self.check_expr(attr.value);
            }
            Expr::Subscript(sub) => {
                self.check_expr(sub.value);
                self.check_expr(sub.slice);
            }
            Expr::List(list) => {
                for elt in list.elts {
                    self.check_expr(elt);
                }
            }
            Expr::Tuple(tuple) => {
                for elt in tuple.elts {
                    self.check_expr(elt);
                }
            }
            Expr::Set(set) => {
                for elt in set.elts {
                    self.check_expr(elt);
                }
            }
            Expr::Dict(dict) => {
                for (k, v) in dict.keys.iter().zip(dict.values.iter()) {
                    if let Some(key) = k {
                        self.check_expr(key);
                    }
                    self.check_expr(v);
                }
            }
            Expr::Lambda(lambda) => {
                self.check_expr(lambda.body);
            }
            Expr::IfExp(if_exp) => {
                self.check_expr(if_exp.test);
                self.check_expr(if_exp.body);
                self.check_expr(if_exp.orelse);
            }
            Expr::BoolOp(bool_op) => {
                for value in bool_op.values {
                    self.check_expr(value);
                }
            }
            Expr::ListComp(comp) => {
                self.check_expr(comp.elt);
            }
            Expr::DictComp(comp) => {
                self.check_expr(comp.key);
                self.check_expr(comp.value);
            }
            Expr::SetComp(comp) => {
                self.check_expr(comp.elt);
            }
            Expr::GeneratorExp(comp) => {
                self.check_expr(comp.elt);
            }
            Expr::Await(await_expr) => {
                self.check_expr(await_expr.value);
            }
            Expr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    self.check_expr(value);
                }
            }
            Expr::YieldFrom(yield_from) => {
                self.check_expr(yield_from.value);
            }
            Expr::NamedExpr(named) => {
                self.check_expr(named.target);
                self.check_expr(named.value);
            }
            Expr::Starred(starred) => {
                self.check_expr(starred.value);
            }
            _ => {}
        }
    }

    fn check_export(&mut self, export: &ExportStmt<'a>) {
        // If this is a re-export (has module field), we can't validate the names
        // at compile time without module resolution system
        if export.module.is_some() {
            // For re-exports, we just track the exported names for duplicate detection
            for (name, _alias) in export.names {
                self.check_duplicate_export(name, export.span);
            }
            return;
        }

        // For regular exports, check that each name is defined
        for (name, _alias) in export.names {
            // Check if already exported
            self.check_duplicate_export(name, export.span);

            // Check if name is defined in module scope
            if self
                .symbol_table
                .module_scope()
                .lookup_local(name, |_| ())
                .is_none()
            {
                self.errors.push(*error(
                    ErrorKind::ExportUndefined {
                        name: name.to_string(),
                    },
                    export.span,
                ));
            }
        }
    }

    fn check_duplicate_export(&mut self, name: &'a str, span: TextRange) {
        // Check if this name was already exported
        if let Some((_, first_span)) = self.exported_names.iter().find(|(n, _)| *n == name) {
            self.errors.push(*error(
                ErrorKind::DuplicateExport {
                    name: name.to_string(),
                    first_span: *first_span,
                },
                span,
            ));
        } else {
            self.exported_names.push((name, span));
        }
    }

    fn check_module_introspection(&mut self, intro: &ModuleIntrospectionExpr<'a>) {
        if !VALID_INTROSPECTION_FUNCTIONS.contains(&intro.function) {
            // Find closest valid function name for suggestion
            let suggestion = VALID_INTROSPECTION_FUNCTIONS
                .iter()
                .min_by_key(|valid| levenshtein_distance(intro.function, valid))
                .map(|s| s.to_string());

            self.errors.push(*error(
                ErrorKind::InvalidIntrospection {
                    function: intro.function.to_string(),
                    suggestion,
                },
                intro.span,
            ));
        }
    }
}

/// Calculate Levenshtein distance between two strings
fn levenshtein_distance(s1: &str, s2: &str) -> usize {
    let len1 = s1.chars().count();
    let len2 = s2.chars().count();

    if len1 == 0 {
        return len2;
    }
    if len2 == 0 {
        return len1;
    }

    let mut matrix = vec![vec![0; len2 + 1]; len1 + 1];

    for (i, row) in matrix.iter_mut().enumerate().take(len1 + 1) {
        row[0] = i;
    }
    for j in 0..=len2 {
        matrix[0][j] = j;
    }

    let s1_chars: Vec<char> = s1.chars().collect();
    let s2_chars: Vec<char> = s2.chars().collect();

    for i in 1..=len1 {
        for j in 1..=len2 {
            let cost = if s1_chars[i - 1] == s2_chars[j - 1] {
                0
            } else {
                1
            };
            matrix[i][j] = std::cmp::min(
                std::cmp::min(
                    matrix[i - 1][j] + 1, // deletion
                    matrix[i][j - 1] + 1, // insertion
                ),
                matrix[i - 1][j - 1] + cost, // substitution
            );
        }
    }

    matrix[len1][len2]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::passes::name_resolution::NameResolver;
    use crate::{Arena, Lexer, Parser};

    fn check_module_system(source: &str) -> Result<(), Vec<Error>> {
        let arena = Arena::new();
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, &arena);
        let module = parser.parse_module().expect("Parse failed");

        // First run name resolution to build symbol table
        let mut name_resolver = NameResolver::new();
        name_resolver.resolve_module(module);
        let (symbol_table, _name_errors) = name_resolver.into_symbol_table();

        // Then check module system
        let checker = ModuleSystemChecker::new(&symbol_table);
        let errors = checker.check(module);

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    #[test]
    fn test_export_defined_name() {
        let source = r#"
x = 42
export x
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_export_undefined_name() {
        let source = r#"
export undefined_variable
"#;
        let result = check_module_system(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            ErrorKind::ExportUndefined { name } => {
                assert_eq!(name, "undefined_variable");
            }
            _ => panic!("Expected ExportUndefined error"),
        }
    }

    #[test]
    fn test_export_function() {
        let source = r#"
def my_function():
    pass

export my_function
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_export_class() {
        let source = r#"
class MyClass:
    pass

export MyClass
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_duplicate_export() {
        let source = r#"
x = 42
export x
export x
"#;
        let result = check_module_system(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            ErrorKind::DuplicateExport { name, .. } => {
                assert_eq!(name, "x");
            }
            _ => panic!("Expected DuplicateExport error"),
        }
    }

    #[test]
    fn test_export_multiple_names() {
        let source = r#"
x = 1
y = 2
z = 3
export x, y, z
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_export_with_alias() {
        let source = r#"
original_name = 42
export original_name as exported_name
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_reexport_from_module() {
        let source = r#"
export add, subtract from math
"#;
        // Re-exports are allowed even if we can't validate the source module
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_valid_module_introspection_is_main() {
        let source = r#"
if module::is_main():
    print("Main module")
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_valid_module_introspection_name() {
        let source = r#"
name = module::name()
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_valid_module_introspection_path() {
        let source = r#"
path = module::path()
"#;
        assert!(check_module_system(source).is_ok());
    }

    #[test]
    fn test_invalid_module_introspection() {
        let source = r#"
result = module::invalid_function()
"#;
        let result = check_module_system(source);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0].kind {
            ErrorKind::InvalidIntrospection {
                function,
                suggestion,
            } => {
                assert_eq!(function, "invalid_function");
                assert!(suggestion.is_some());
            }
            _ => panic!("Expected InvalidIntrospection error"),
        }
    }

    #[test]
    fn test_export_before_definition_ok() {
        // Note: Name resolution processes all definitions first,
        // so exports can appear before the actual definition
        let source = r#"
export x
x = 42
"#;
        let result = check_module_system(source);
        // This should pass because name resolution defines x when it sees the assignment
        assert!(result.is_ok());
    }

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("is_main", "is_main"), 0);
        assert_eq!(levenshtein_distance("is_main", "ismain"), 1);
        assert_eq!(levenshtein_distance("is_main", "name"), 6); // Changed from 4 to 6
        assert_eq!(levenshtein_distance("path", "pth"), 1);
    }
}
