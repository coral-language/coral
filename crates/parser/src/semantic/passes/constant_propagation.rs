use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::control_flow::{
    BasicBlock, ControlFlowGraph, DataflowAnalysis, solve_dataflow,
};
use std::collections::{HashMap, HashSet};

/// Lattice value for constant propagation
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Bottom,
    Constant(LiteralValue),
    Top,
}

/// Literal values that can be propagated
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

impl ConstantValue {
    fn meet(values: Vec<Self>) -> Self {
        if values.is_empty() {
            return ConstantValue::Bottom;
        }

        let first = &values[0];
        if values.iter().all(|v| v == first) {
            first.clone()
        } else {
            ConstantValue::Top
        }
    }
}

/// Constant propagation dataflow analysis
#[derive(Clone)]
pub struct ConstantPropagationAnalysis;

impl ConstantPropagationAnalysis {
    pub fn new() -> Self {
        ConstantPropagationAnalysis
    }
}

impl Default for ConstantPropagationAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

impl DataflowAnalysis for ConstantPropagationAnalysis {
    type Value = HashMap<String, ConstantValue>;

    fn initial_value(&self) -> Self::Value {
        HashMap::new()
    }

    fn transfer(&self, block: &BasicBlock, mut input: Self::Value) -> Self::Value {
        for stmt_info in &block.statements {
            for used_var in &stmt_info.uses {
                if !input.contains_key(used_var) {
                    input.insert(used_var.clone(), ConstantValue::Top);
                }
            }

            for def_var in &stmt_info.defs {
                input.insert(def_var.clone(), ConstantValue::Top);
            }
        }

        input
    }

    fn meet(&self, values: Vec<Self::Value>) -> Self::Value {
        if values.is_empty() {
            return HashMap::new();
        }

        let mut result = HashMap::new();
        let mut all_vars = HashSet::new();

        for map in &values {
            for key in map.keys() {
                all_vars.insert(key.clone());
            }
        }

        for var in all_vars {
            let var_values: Vec<ConstantValue> = values
                .iter()
                .map(|m| m.get(&var).cloned().unwrap_or(ConstantValue::Bottom))
                .collect();

            result.insert(var, ConstantValue::meet(var_values));
        }

        result
    }

    fn is_forward(&self) -> bool {
        true
    }
}

/// Constant propagation pass
///
/// Performs constant propagation and folding to:
/// - Evaluate expressions with constant operands at compile time
/// - Detect constant conditions and dead branches
/// - Simplify expressions for better code generation
pub struct ConstantPropagationPass {
    errors: Vec<Error>,
    warnings: Vec<Error>,
    pure_functions: HashSet<String>,
    current_function: Option<String>,
}

impl Default for ConstantPropagationPass {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantPropagationPass {
    /// Create a new constant propagation pass
    pub fn new() -> Self {
        let mut pure_functions = HashSet::new();
        pure_functions.insert("abs".to_string());
        pure_functions.insert("len".to_string());
        pure_functions.insert("min".to_string());
        pure_functions.insert("max".to_string());
        pure_functions.insert("ord".to_string());
        pure_functions.insert("chr".to_string());

        ConstantPropagationPass {
            errors: Vec::new(),
            warnings: Vec::new(),
            pure_functions,
            current_function: None,
        }
    }

    /// Check a module using pre-built CFGs
    pub fn check_module_with_cfgs(
        &mut self,
        module: &Module,
        function_cfgs: &HashMap<String, ControlFlowGraph>,
    ) -> (Vec<Error>, Vec<Error>) {
        self.analyze_module_body(module.body, function_cfgs);

        (
            std::mem::take(&mut self.errors),
            std::mem::take(&mut self.warnings),
        )
    }

    fn analyze_module_body(
        &mut self,
        stmts: &[Stmt],
        function_cfgs: &HashMap<String, ControlFlowGraph>,
    ) {
        for stmt in stmts {
            self.analyze_stmt(stmt, function_cfgs);
        }
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
            Stmt::If(if_stmt) => {
                if let Some(constant_value) =
                    self.evaluate_condition(&if_stmt.test, &HashMap::new())
                {
                    self.warnings.push(*error(
                        ErrorKind::ConstantCondition {
                            value: constant_value,
                            suggestion: if constant_value {
                                "The condition is always true".to_string()
                            } else {
                                "The condition is always false".to_string()
                            },
                        },
                        if_stmt.test.span(),
                    ));

                    if constant_value && !if_stmt.orelse.is_empty() {
                        self.warnings.push(*error(
                            ErrorKind::DeadBranch {
                                branch_type: "else".to_string(),
                                reason: "condition is always true".to_string(),
                            },
                            if_stmt.orelse[0].span(),
                        ));
                    } else if !constant_value {
                        self.warnings.push(*error(
                            ErrorKind::DeadBranch {
                                branch_type: "then".to_string(),
                                reason: "condition is always false".to_string(),
                            },
                            if_stmt.body[0].span(),
                        ));
                    }
                }
            }
            Stmt::While(while_stmt) => {
                if let Some(constant_value) =
                    self.evaluate_condition(&while_stmt.test, &HashMap::new())
                    && !constant_value
                {
                    self.warnings.push(*error(
                        ErrorKind::ConstantCondition {
                            value: constant_value,
                            suggestion: "Loop will never execute".to_string(),
                        },
                        while_stmt.test.span(),
                    ));
                }
            }
            _ => {}
        }
    }

    fn check_function_cfg(
        &mut self,
        _func: &crate::ast::nodes::FuncDefStmt,
        cfg: &ControlFlowGraph,
    ) {
        let analysis = ConstantPropagationAnalysis::new();
        let _result = solve_dataflow(cfg, &analysis);
    }

    fn evaluate_condition(
        &self,
        expr: &Expr,
        constants: &HashMap<String, ConstantValue>,
    ) -> Option<bool> {
        match self.evaluate_constant_expr(expr, constants) {
            ConstantValue::Constant(LiteralValue::Bool(b)) => Some(b),
            ConstantValue::Constant(LiteralValue::Int(i)) => Some(i != 0),
            ConstantValue::Constant(LiteralValue::None) => Some(false),
            _ => None,
        }
    }

    fn evaluate_constant_expr(
        &self,
        expr: &Expr,
        constants: &HashMap<String, ConstantValue>,
    ) -> ConstantValue {
        match expr {
            Expr::Constant(c) => self.parse_constant(c.value),
            Expr::Name(name) => {
                if name.id == "True" {
                    ConstantValue::Constant(LiteralValue::Bool(true))
                } else if name.id == "False" {
                    ConstantValue::Constant(LiteralValue::Bool(false))
                } else if name.id == "None" {
                    ConstantValue::Constant(LiteralValue::None)
                } else {
                    constants
                        .get(name.id)
                        .cloned()
                        .unwrap_or(ConstantValue::Top)
                }
            }
            Expr::BinOp(binop) => {
                let left = self.evaluate_constant_expr(binop.left, constants);
                let right = self.evaluate_constant_expr(binop.right, constants);
                self.fold_binary_op(binop.op, &left, &right)
            }
            Expr::UnaryOp(unary) => {
                let operand = self.evaluate_constant_expr(unary.operand, constants);
                self.fold_unary_op(unary.op, &operand)
            }
            Expr::BoolOp(boolop) => {
                let is_and = boolop.op == "and";
                let mut result = if is_and {
                    ConstantValue::Constant(LiteralValue::Bool(true))
                } else {
                    ConstantValue::Constant(LiteralValue::Bool(false))
                };

                for value in boolop.values {
                    let val = self.evaluate_constant_expr(value, constants);

                    if let ConstantValue::Constant(LiteralValue::Bool(b)) = val {
                        if is_and {
                            if !b {
                                return ConstantValue::Constant(LiteralValue::Bool(false));
                            }
                        } else if b {
                            return ConstantValue::Constant(LiteralValue::Bool(true));
                        }
                        result = val;
                    } else {
                        return ConstantValue::Top;
                    }
                }

                result
            }
            Expr::Compare(compare) => {
                if compare.ops.len() != 1 || compare.comparators.len() != 1 {
                    return ConstantValue::Top;
                }

                let left = self.evaluate_constant_expr(compare.left, constants);
                let right = self.evaluate_constant_expr(&compare.comparators[0], constants);

                self.fold_comparison(compare.ops[0], &left, &right)
            }
            Expr::Call(call) => {
                if let Expr::Name(func_name) = call.func
                    && self.is_pure_function(func_name.id)
                    && call.args.len() == 1
                {
                    let arg = self.evaluate_constant_expr(&call.args[0], constants);
                    return self.evaluate_pure_function(func_name.id, &arg);
                }
                ConstantValue::Top
            }
            _ => ConstantValue::Top,
        }
    }

    fn parse_constant(&self, value: &str) -> ConstantValue {
        match value {
            "True" => ConstantValue::Constant(LiteralValue::Bool(true)),
            "False" => ConstantValue::Constant(LiteralValue::Bool(false)),
            "None" => ConstantValue::Constant(LiteralValue::None),
            _ => {
                if let Ok(i) = value.parse::<i64>() {
                    ConstantValue::Constant(LiteralValue::Int(i))
                } else if let Ok(f) = value.parse::<f64>() {
                    ConstantValue::Constant(LiteralValue::Float(f))
                } else if value.starts_with('"') || value.starts_with('\'') {
                    let s = value.trim_matches(|c| c == '"' || c == '\'');
                    ConstantValue::Constant(LiteralValue::String(s.to_string()))
                } else {
                    ConstantValue::Top
                }
            }
        }
    }

    fn fold_binary_op(
        &self,
        op: &str,
        left: &ConstantValue,
        right: &ConstantValue,
    ) -> ConstantValue {
        match (left, right) {
            (ConstantValue::Constant(l), ConstantValue::Constant(r)) => {
                self.fold_binary_literal(op, l, r)
            }
            _ => ConstantValue::Top,
        }
    }

    fn fold_binary_literal(
        &self,
        op: &str,
        left: &LiteralValue,
        right: &LiteralValue,
    ) -> ConstantValue {
        match (left, right) {
            (LiteralValue::Int(l), LiteralValue::Int(r)) => {
                let result = match op {
                    "+" => l.checked_add(*r),
                    "-" => l.checked_sub(*r),
                    "*" => l.checked_mul(*r),
                    "/" => {
                        if *r != 0 {
                            Some(l / r)
                        } else {
                            None
                        }
                    }
                    "%" => {
                        if *r != 0 {
                            Some(l % r)
                        } else {
                            None
                        }
                    }
                    "**" => {
                        if *r >= 0 && *r <= 100 {
                            l.checked_pow(*r as u32)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                result
                    .map(|v| ConstantValue::Constant(LiteralValue::Int(v)))
                    .unwrap_or(ConstantValue::Top)
            }
            (LiteralValue::Float(l), LiteralValue::Float(r)) => {
                let result = match op {
                    "+" => l + r,
                    "-" => l - r,
                    "*" => l * r,
                    "/" => l / r,
                    "**" => l.powf(*r),
                    _ => return ConstantValue::Top,
                };
                ConstantValue::Constant(LiteralValue::Float(result))
            }
            (LiteralValue::String(l), LiteralValue::String(r)) => {
                if op == "+" {
                    ConstantValue::Constant(LiteralValue::String(format!("{}{}", l, r)))
                } else {
                    ConstantValue::Top
                }
            }
            _ => ConstantValue::Top,
        }
    }

    fn fold_comparison(
        &self,
        op: &str,
        left: &ConstantValue,
        right: &ConstantValue,
    ) -> ConstantValue {
        match (left, right) {
            (ConstantValue::Constant(l), ConstantValue::Constant(r)) => {
                self.compare_literals(op, l, r)
            }
            _ => ConstantValue::Top,
        }
    }

    fn compare_literals(
        &self,
        op: &str,
        left: &LiteralValue,
        right: &LiteralValue,
    ) -> ConstantValue {
        let result = match (left, right) {
            (LiteralValue::Int(l), LiteralValue::Int(r)) => match op {
                "==" => l == r,
                "!=" => l != r,
                "<" => l < r,
                "<=" => l <= r,
                ">" => l > r,
                ">=" => l >= r,
                _ => return ConstantValue::Top,
            },
            (LiteralValue::Bool(l), LiteralValue::Bool(r)) => match op {
                "==" => l == r,
                "!=" => l != r,
                _ => return ConstantValue::Top,
            },
            (LiteralValue::String(l), LiteralValue::String(r)) => match op {
                "==" => l == r,
                "!=" => l != r,
                _ => return ConstantValue::Top,
            },
            (LiteralValue::None, LiteralValue::None) => match op {
                "==" | "is" => true,
                "!=" | "is not" => false,
                _ => return ConstantValue::Top,
            },
            _ => return ConstantValue::Top,
        };

        ConstantValue::Constant(LiteralValue::Bool(result))
    }

    fn fold_unary_op(&self, op: &str, operand: &ConstantValue) -> ConstantValue {
        match operand {
            ConstantValue::Constant(lit) => self.fold_unary_literal(op, lit),
            _ => ConstantValue::Top,
        }
    }

    fn fold_unary_literal(&self, op: &str, operand: &LiteralValue) -> ConstantValue {
        match (op, operand) {
            ("-", LiteralValue::Int(i)) => i
                .checked_neg()
                .map(|v| ConstantValue::Constant(LiteralValue::Int(v)))
                .unwrap_or(ConstantValue::Top),
            ("-", LiteralValue::Float(f)) => ConstantValue::Constant(LiteralValue::Float(-f)),
            ("not", LiteralValue::Bool(b)) => ConstantValue::Constant(LiteralValue::Bool(!b)),
            ("not", LiteralValue::Int(i)) => ConstantValue::Constant(LiteralValue::Bool(*i == 0)),
            ("not", LiteralValue::None) => ConstantValue::Constant(LiteralValue::Bool(true)),
            _ => ConstantValue::Top,
        }
    }

    fn evaluate_pure_function(&self, func_name: &str, arg: &ConstantValue) -> ConstantValue {
        match (func_name, arg) {
            ("abs", ConstantValue::Constant(LiteralValue::Int(i))) => {
                ConstantValue::Constant(LiteralValue::Int(i.abs()))
            }
            ("abs", ConstantValue::Constant(LiteralValue::Float(f))) => {
                ConstantValue::Constant(LiteralValue::Float(f.abs()))
            }
            ("len", ConstantValue::Constant(LiteralValue::String(s))) => {
                ConstantValue::Constant(LiteralValue::Int(s.len() as i64))
            }
            _ => ConstantValue::Top,
        }
    }

    fn is_pure_function(&self, name: &str) -> bool {
        self.pure_functions.contains(name)
    }

    /// Get the collected errors
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Get the collected warnings
    pub fn warnings(&self) -> &[Error] {
        &self.warnings
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::passes::control_flow::ControlFlowAnalyzer;

    fn check_code(source: &str) -> (Vec<Error>, Vec<Error>) {
        let arena = crate::arena::Arena::new();
        let lexer = crate::lexer::Lexer::new(source);
        let mut parser = crate::parser::Parser::new(lexer, &arena);
        let module = parser.parse_module().unwrap();

        let mut cf_analyzer = ControlFlowAnalyzer::new();
        cf_analyzer.analyze_module(module);

        let mut pass = ConstantPropagationPass::new();
        pass.check_module_with_cfgs(module, &cf_analyzer.function_cfgs)
    }

    #[test]
    fn test_constant_condition_true() {
        let source = r#"
if True:
    x = 1
else:
    x = 2
"#;
        let (errors, warnings) = check_code(source);
        assert_eq!(errors.len(), 0);
        assert!(!warnings.is_empty());
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w.kind, ErrorKind::ConstantCondition { .. }))
        );
    }

    #[test]
    fn test_constant_condition_false() {
        let source = r#"
if False:
    x = 1
else:
    x = 2
"#;
        let (errors, warnings) = check_code(source);
        assert_eq!(errors.len(), 0);
        assert!(!warnings.is_empty());
        assert!(
            warnings
                .iter()
                .any(|w| matches!(w.kind, ErrorKind::ConstantCondition { .. }))
        );
    }

    #[test]
    fn test_no_constant_condition() {
        let source = r#"
def foo(x):
    if x > 0:
        return 1
    else:
        return 2
"#;
        let (errors, warnings) = check_code(source);
        assert_eq!(errors.len(), 0);
        assert_eq!(warnings.len(), 0);
    }
}
