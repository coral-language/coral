//! HIR validation pass

use super::context::HirContext;
use super::typed_expr::TypedExpr;
use super::typed_pattern::TypedPattern;
use super::typed_stmt::TypedStmt;
use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// HIR validation errors
#[derive(Debug, Clone)]
pub enum HirValidationError {
    /// Undefined symbol reference
    UndefinedSymbol { name: String, span: TextRange },
    /// Type mismatch
    TypeMismatch {
        expected: Type,
        actual: Type,
        span: TextRange,
    },
    /// Invalid MRO (circular inheritance)
    InvalidMro { class: Symbol, span: TextRange },
    /// Desugaring incomplete
    DesugaringIncomplete {
        description: String,
        span: TextRange,
    },
    /// Pattern exhaustiveness issue
    PatternExhaustiveness { pattern: String, span: TextRange },
}

impl std::fmt::Display for HirValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirValidationError::UndefinedSymbol { name, .. } => {
                write!(f, "Undefined symbol: '{}'", name)
            }
            HirValidationError::TypeMismatch {
                expected, actual, ..
            } => {
                write!(
                    f,
                    "Type mismatch: expected {}, got {}",
                    expected.display_name(),
                    actual.display_name()
                )
            }
            HirValidationError::InvalidMro { class, .. } => {
                write!(f, "Invalid MRO for class: {}", class)
            }
            HirValidationError::DesugaringIncomplete { description, .. } => {
                write!(f, "Desugaring incomplete: {}", description)
            }
            HirValidationError::PatternExhaustiveness { pattern, .. } => {
                write!(f, "Pattern exhaustiveness issue: {}", pattern)
            }
        }
    }
}

impl std::error::Error for HirValidationError {}

/// HIR validator
pub struct HirValidator<'a> {
    context: &'a HirContext<'a>,
    errors: Vec<HirValidationError>,
}

impl<'a> HirValidator<'a> {
    /// Create a new HIR validator
    pub fn new(context: &'a HirContext<'a>) -> Self {
        Self {
            context,
            errors: Vec::new(),
        }
    }

    /// Validate the entire HIR
    pub fn validate(&mut self) -> Result<(), Vec<HirValidationError>> {
        self.errors.clear();

        // Validate all statements in the module
        self.validate_statements(self.context.module_body());

        // Validate class hierarchies
        self.validate_class_hierarchies();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Validate a list of statements
    fn validate_statements(&mut self, statements: &[TypedStmt<'a>]) {
        for stmt in statements {
            self.validate_statement(stmt);
        }
    }

    /// Validate a single statement
    fn validate_statement(&mut self, stmt: &TypedStmt<'a>) {
        match stmt {
            TypedStmt::Expr(expr_stmt) => {
                self.validate_expression(&expr_stmt.value);
            }
            TypedStmt::Assign(assign) => {
                for target in assign.targets {
                    self.validate_expression(target);
                }
                self.validate_expression(&assign.value);
            }
            TypedStmt::AnnAssign(ann_assign) => {
                self.validate_expression(&ann_assign.target);
                self.validate_expression(&ann_assign.annotation);
                if let Some(value) = &ann_assign.value {
                    self.validate_expression(value);
                }
            }
            TypedStmt::AugAssign(aug_assign) => {
                self.validate_expression(&aug_assign.target);
                self.validate_expression(&aug_assign.value);
            }
            TypedStmt::Return(ret) => {
                if let Some(value) = &ret.value {
                    self.validate_expression(value);
                }
            }
            TypedStmt::If(if_stmt) => {
                self.validate_expression(&if_stmt.test);
                self.validate_statements(if_stmt.body);
                self.validate_statements(if_stmt.orelse);
            }
            TypedStmt::While(while_stmt) => {
                self.validate_expression(&while_stmt.test);
                self.validate_statements(while_stmt.body);
                self.validate_statements(while_stmt.orelse);
            }
            TypedStmt::For(for_stmt) => {
                self.validate_expression(&for_stmt.target);
                self.validate_expression(&for_stmt.iter);
                self.validate_expression(&for_stmt.iter_call);
                self.validate_expression(&for_stmt.next_call);
                self.validate_statements(for_stmt.body);
                self.validate_statements(for_stmt.orelse);
            }
            TypedStmt::FuncDef(func) => {
                self.validate_function_definition(func);
            }
            TypedStmt::ClassDef(class) => {
                self.validate_class_definition(class);
            }
            TypedStmt::Try(try_stmt) => {
                self.validate_statements(try_stmt.body);
                for handler in try_stmt.handlers {
                    if let Some(typ) = &handler.typ {
                        self.validate_expression(typ);
                    }
                    self.validate_statements(handler.body);
                }
                self.validate_statements(try_stmt.orelse);
                self.validate_statements(try_stmt.finalbody);
            }
            TypedStmt::With(with_stmt) => {
                for item in with_stmt.items {
                    self.validate_expression(&item.context_expr);
                    self.validate_expression(&item.enter_call);
                    self.validate_expression(&item.exit_call);
                    if let Some(vars) = &item.optional_vars {
                        self.validate_expression(vars);
                    }
                }
                self.validate_statements(with_stmt.body);
            }
            TypedStmt::Assert(assert_stmt) => {
                self.validate_expression(&assert_stmt.test);
                if let Some(msg) = &assert_stmt.msg {
                    self.validate_expression(msg);
                }
            }
            TypedStmt::Delete(delete_stmt) => {
                for target in delete_stmt.targets {
                    self.validate_expression(target);
                }
            }
            TypedStmt::Match(match_stmt) => {
                self.validate_expression(&match_stmt.subject);
                for case in match_stmt.cases {
                    self.validate_pattern(&case.pattern);
                    if let Some(guard) = &case.guard {
                        self.validate_expression(guard);
                    }
                    self.validate_statements(case.body);
                }
            }
            TypedStmt::Yield(yield_stmt) => {
                if let Some(value) = yield_stmt.value {
                    self.validate_expression(value);
                }
            }
            TypedStmt::TypeAlias(type_alias) => {
                self.validate_expression(&type_alias.value);
            }
            _ => {
                // Other statements don't need validation
            }
        }
    }

    /// Validate an expression
    #[allow(clippy::only_used_in_recursion)]
    fn validate_expression(&mut self, expr: &TypedExpr<'a>) {
        match expr {
            TypedExpr::Name(name_expr) => {
                // Validate typed name expression
                // Symbol resolution handled by name resolution pass
                // Type inference assigns types to resolved symbols
                if name_expr.ty == Type::Unknown {
                    // Unknown type is acceptable for gradual typing
                }
            }
            TypedExpr::Call(call_expr) => {
                self.validate_expression(call_expr.func);
                for arg in call_expr.args {
                    self.validate_expression(arg);
                }
                for kw in call_expr.keywords {
                    self.validate_expression(kw.value);
                }
            }
            TypedExpr::MethodCall(method_call) => {
                self.validate_expression(method_call.object);
                for arg in method_call.args {
                    self.validate_expression(arg);
                }
                for kw in method_call.keywords {
                    self.validate_expression(kw.value);
                }
            }
            TypedExpr::Attribute(attr_expr) => {
                self.validate_expression(attr_expr.value);
            }
            TypedExpr::Subscript(subscript_expr) => {
                self.validate_expression(subscript_expr.value);
                self.validate_expression(subscript_expr.slice);
            }
            TypedExpr::BinOp(bin_op) => {
                self.validate_expression(bin_op.left);
                self.validate_expression(bin_op.right);
            }
            TypedExpr::UnaryOp(unary_op) => {
                self.validate_expression(unary_op.operand);
            }
            TypedExpr::Compare(compare_expr) => {
                self.validate_expression(compare_expr.left);
                for comparator in compare_expr.comparators {
                    self.validate_expression(comparator);
                }
            }
            TypedExpr::BoolOp(bool_op) => {
                for value in bool_op.values {
                    self.validate_expression(value);
                }
            }
            TypedExpr::List(list_expr) => {
                for elt in list_expr.elts {
                    self.validate_expression(elt);
                }
            }
            TypedExpr::Tuple(tuple_expr) => {
                for elt in tuple_expr.elts {
                    self.validate_expression(elt);
                }
            }
            TypedExpr::Set(set_expr) => {
                for elt in set_expr.elts {
                    self.validate_expression(elt);
                }
            }
            TypedExpr::Dict(dict_expr) => {
                for k in dict_expr.keys.iter().flatten() {
                    self.validate_expression(k);
                }
                for value in dict_expr.values {
                    self.validate_expression(value);
                }
            }
            TypedExpr::Lambda(lambda_expr) => {
                self.validate_expression(lambda_expr.body);
            }
            TypedExpr::IfExp(if_expr) => {
                self.validate_expression(if_expr.test);
                self.validate_expression(if_expr.body);
                self.validate_expression(if_expr.orelse);
            }
            TypedExpr::Await(await_expr) => {
                self.validate_expression(await_expr.value);
            }
            TypedExpr::NamedExpr(named_expr) => {
                self.validate_expression(named_expr.target);
                self.validate_expression(named_expr.value);
            }
            TypedExpr::JoinedStr(joined_str) => {
                for value in joined_str.values {
                    self.validate_expression(value);
                }
            }
            TypedExpr::FormattedValue(formatted_value) => {
                self.validate_expression(formatted_value.value);
                if let Some(format_spec) = formatted_value.format_spec {
                    self.validate_expression(&TypedExpr::JoinedStr(format_spec.clone()));
                }
            }
            TypedExpr::TString(t_string) => {
                for value in t_string.values {
                    self.validate_expression(value);
                }
            }
            TypedExpr::Starred(starred_expr) => {
                self.validate_expression(starred_expr.value);
            }
            TypedExpr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    self.validate_expression(value);
                }
            }
            TypedExpr::YieldFrom(yield_from_expr) => {
                self.validate_expression(yield_from_expr.value);
            }
            _ => {
                // Other expressions don't need validation
            }
        }
    }

    /// Validate a pattern
    fn validate_pattern(&mut self, pattern: &TypedPattern<'a>) {
        match pattern {
            TypedPattern::MatchAs(match_as) => {
                if let Some(p) = match_as.pattern {
                    self.validate_pattern(p);
                }
            }
            TypedPattern::MatchValue(match_value) => {
                self.validate_expression(&match_value.value);
            }
            TypedPattern::MatchOr(match_or) => {
                for p in match_or.patterns {
                    self.validate_pattern(p);
                }
            }
            TypedPattern::MatchSequence(match_seq) => {
                for p in match_seq.patterns {
                    self.validate_pattern(p);
                }
            }
            TypedPattern::MatchMapping(match_map) => {
                for key in match_map.keys {
                    self.validate_expression(key);
                }
                for p in match_map.patterns {
                    self.validate_pattern(p);
                }
            }
            TypedPattern::MatchClass(match_class) => {
                self.validate_expression(&match_class.cls);
                for p in match_class.patterns {
                    self.validate_pattern(p);
                }
                for p in match_class.kwd_patterns {
                    self.validate_pattern(p);
                }
            }
            TypedPattern::MatchSingleton(_) => {
                // No validation needed for singletons
            }
        }
    }

    /// Validate a function definition
    fn validate_function_definition(&mut self, func: &super::typed_stmt::TypedFuncDefStmt<'a>) {
        // Validate function body
        self.validate_statements(func.body);

        // Validate return type annotation if present
        if let Some(returns) = func.returns {
            self.validate_expression(returns);
        }

        // Validate decorators
        for decorator in func.decorators {
            self.validate_expression(decorator);
        }
    }

    /// Validate a class definition
    fn validate_class_definition(&mut self, class: &super::typed_stmt::TypedClassDefStmt<'a>) {
        // Validate class body
        self.validate_statements(class.body);

        // Validate base classes
        for base in class.bases {
            self.validate_expression(base);
        }

        // Validate decorators
        for decorator in class.decorators {
            self.validate_expression(decorator);
        }

        // Validate MRO
        if let Some(mro) = self.context.get_class_mro(class.name)
            && mro.is_empty()
        {
            self.errors.push(HirValidationError::InvalidMro {
                class: class.name,
                span: class.span,
            });
        }
    }

    /// Validate class hierarchies
    fn validate_class_hierarchies(&mut self) {
        let classes = self.context.find_classes();
        for class in classes {
            if let Some(mro) = self.context.get_class_mro(class.name)
                && mro.is_empty()
            {
                self.errors.push(HirValidationError::InvalidMro {
                    class: class.name,
                    span: class.span,
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_error_display() {
        let error = HirValidationError::UndefinedSymbol {
            name: "undefined_var".to_string(),
            span: TextRange::new(0.into(), 10.into()),
        };
        assert_eq!(error.to_string(), "Undefined symbol: 'undefined_var'");
    }

    #[test]
    fn test_type_mismatch_error() {
        let error = HirValidationError::TypeMismatch {
            expected: Type::Int,
            actual: Type::Str,
            span: TextRange::new(0.into(), 10.into()),
        };
        assert_eq!(error.to_string(), "Type mismatch: expected int, got str");
    }
}
