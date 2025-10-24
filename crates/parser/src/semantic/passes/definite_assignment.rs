use crate::ast::nodes::{Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::control_flow::{
    BasicBlock, ControlFlowGraph, DefiniteAssignmentAnalysis, solve_dataflow,
};
use std::collections::{HashMap, HashSet};

/// Definite assignment analysis pass
///
/// Detects use of uninitialized variables by performing dataflow analysis
/// on the control flow graph. A variable is considered initialized if it
/// has been assigned on all paths leading to its use.
pub struct DefiniteAssignmentPass {
    errors: Vec<Error>,
    current_function: Option<String>,
    /// Module-level definitions (classes, functions, global variables)
    module_level_defs: HashSet<String>,
}

impl Default for DefiniteAssignmentPass {
    fn default() -> Self {
        Self::new()
    }
}

impl DefiniteAssignmentPass {
    /// Create a new definite assignment pass
    pub fn new() -> Self {
        DefiniteAssignmentPass {
            errors: Vec::new(),
            current_function: None,
            module_level_defs: HashSet::new(),
        }
    }

    /// Collect module-level definitions (class names, function names, global variables)
    fn collect_module_defs(&mut self, module: &Module) {
        for stmt in module.body {
            match stmt {
                Stmt::ClassDef(class) => {
                    self.module_level_defs.insert(class.name.to_string());
                }
                Stmt::FuncDef(func) => {
                    self.module_level_defs.insert(func.name.to_string());
                }
                Stmt::Assign(assign) => {
                    // Add module-level assignments
                    for target in assign.targets {
                        if let crate::ast::expr::Expr::Name(name) = target {
                            self.module_level_defs.insert(name.id.to_string());
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Check a module using pre-built CFGs from control flow analysis
    pub fn check_module_with_cfgs(
        &mut self,
        module: &Module,
        function_cfgs: &HashMap<String, ControlFlowGraph>,
    ) -> Vec<Error> {
        // First, collect all module-level definitions
        self.collect_module_defs(module);

        // Then analyze each statement
        for stmt in module.body {
            self.analyze_stmt(stmt, function_cfgs);
        }
        std::mem::take(&mut self.errors)
    }

    fn analyze_stmt(&mut self, stmt: &Stmt, function_cfgs: &HashMap<String, ControlFlowGraph>) {
        match stmt {
            Stmt::FuncDef(func) => {
                self.current_function = Some(func.name.to_string());
                if let Some(cfg) = function_cfgs.get(func.name) {
                    self.check_function_cfg(func, cfg);
                }
                self.current_function = None;
            }
            Stmt::ClassDef(class) => {
                for stmt in class.body {
                    self.analyze_stmt(stmt, function_cfgs);
                }
            }
            _ => {}
        }
    }

    fn check_function_cfg(
        &mut self,
        func: &crate::ast::nodes::FuncDefStmt,
        cfg: &ControlFlowGraph,
    ) {
        let initial_defs = self.get_initial_defs(func);
        let analysis = DefiniteAssignmentAnalysis;
        let result = solve_dataflow(cfg, &analysis);

        cfg.entry_block();

        for (block_id, block) in &cfg.blocks {
            if let Some(in_set) = result.in_values.get(block_id) {
                let mut current_defs = in_set.clone();
                current_defs.extend(initial_defs.iter().cloned());

                self.check_block_statements(block, &current_defs);
            }
        }
    }

    fn check_block_statements(&mut self, block: &BasicBlock, in_set: &HashSet<String>) {
        let mut defined = in_set.clone();

        for stmt_info in &block.statements {
            for used_var in &stmt_info.uses {
                if !defined.contains(used_var) && !self.is_builtin_or_global(used_var) {
                    self.errors.push(*error(
                        ErrorKind::UninitializedVariable {
                            var_name: used_var.clone(),
                        },
                        stmt_info.span,
                    ));
                }
            }

            for def_var in &stmt_info.defs {
                if stmt_info.uses.contains(def_var) && !defined.contains(def_var) {
                    self.errors.push(*error(
                        ErrorKind::SelfReferentialInitialization {
                            var_name: def_var.clone(),
                        },
                        stmt_info.span,
                    ));
                }
                defined.insert(def_var.clone());
            }
        }
    }

    fn get_initial_defs(&self, func: &crate::ast::nodes::FuncDefStmt) -> HashSet<String> {
        let mut defs = HashSet::new();

        // Positional-only arguments (Python 3.8+)
        for arg in func.args.posonlyargs {
            defs.insert(arg.arg.to_string());
        }

        // Regular positional arguments
        for arg in func.args.args {
            defs.insert(arg.arg.to_string());
        }

        // *args
        if let Some(ref vararg) = func.args.vararg {
            defs.insert(vararg.arg.to_string());
        }

        // Keyword-only arguments
        for kwonly in func.args.kwonlyargs {
            defs.insert(kwonly.arg.to_string());
        }

        // **kwargs
        if let Some(ref kwarg) = func.args.kwarg {
            defs.insert(kwarg.arg.to_string());
        }

        defs
    }

    fn is_builtin_or_global(&self, name: &str) -> bool {
        // Check if it's a builtin type/function OR a module-level definition
        crate::semantic::builtins::is_builtin(name) || self.module_level_defs.contains(name)
    }

    /// Get the collected errors
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::passes::control_flow::ControlFlowAnalyzer;

    fn check_code(source: &str) -> Vec<Error> {
        let arena = crate::arena::Arena::new();
        let lexer = crate::lexer::Lexer::new(source);
        let mut parser = crate::parser::Parser::new(lexer, &arena);
        let module = parser.parse_module().unwrap();

        let mut cf_analyzer = ControlFlowAnalyzer::new();
        cf_analyzer.analyze_module(module);

        let mut pass = DefiniteAssignmentPass::new();
        pass.check_module_with_cfgs(module, &cf_analyzer.function_cfgs)
    }

    #[test]
    fn test_initialized_variable() {
        let source = r#"
def foo():
    x = 5
    return x
"#;
        let errors = check_code(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_uninitialized_variable() {
        let source = r#"
def foo():
    return x
"#;
        let errors = check_code(source);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            errors[0].kind,
            ErrorKind::UninitializedVariable { .. }
        ));
    }

    #[test]
    fn test_function_parameter() {
        let source = r#"
def foo(x):
    return x
"#;
        let errors = check_code(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_conditional_initialization_both_branches() {
        let source = r#"
def foo(cond):
    if cond:
        x = 1
    else:
        x = 2
    return x
"#;
        let errors = check_code(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_builtin_variables() {
        let source = r#"
def foo():
    return len([1, 2, 3])
"#;
        let errors = check_code(source);
        assert_eq!(errors.len(), 0);
    }
}
