// Decorator resolution and validation
//
// This pass validates decorator usage:
// 1. Checks that decorators reference valid callables
// 2. Validates decorator signatures are compatible with decorated items
// 3. Detects common decorator errors (e.g., @property on classes)
// 4. Validates decorator stacking order (e.g., @classmethod before @property)

#![allow(clippy::only_used_in_recursion)]

use crate::ast::expr::{CallExpr, Expr};
use crate::ast::nodes::{ClassDefStmt, FuncDefStmt, Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::symbol::SymbolTable;
use text_size::TextRange;

/// Known decorator names and their properties
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DecoratorKind {
    Property,
    StaticMethod,
    ClassMethod,
    AbstractMethod,
    DataClass,
    Override,
    Final,
    Cached,
    Operator,
}

impl DecoratorKind {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "property" => Some(Self::Property),
            "staticmethod" => Some(Self::StaticMethod),
            "classmethod" => Some(Self::ClassMethod),
            "abstractmethod" | "abc.abstractmethod" => Some(Self::AbstractMethod),
            "dataclass" | "dataclasses.dataclass" => Some(Self::DataClass),
            "override" | "typing.override" => Some(Self::Override),
            "final" | "typing.final" => Some(Self::Final),
            "cached_property" | "functools.cached_property" => Some(Self::Cached),
            "lru_cache" | "functools.lru_cache" => Some(Self::Cached),
            "operator" => Some(Self::Operator),
            _ => None,
        }
    }

    fn is_function_only(&self) -> bool {
        matches!(
            self,
            Self::Property
                | Self::StaticMethod
                | Self::ClassMethod
                | Self::AbstractMethod
                | Self::Override
                | Self::Cached
                | Self::Operator
        )
    }

    fn is_class_only(&self) -> bool {
        matches!(self, Self::DataClass)
    }

    /// Check if this decorator must come before another in stacking order
    fn must_precede(&self, other: &Self) -> bool {
        match (self, other) {
            // @classmethod or @staticmethod must come before @property
            (Self::ClassMethod | Self::StaticMethod, Self::Property) => true,
            // @abstractmethod typically comes after other decorators
            (Self::Property | Self::ClassMethod | Self::StaticMethod, Self::AbstractMethod) => true,
            _ => false,
        }
    }
}

/// Decorator resolver
pub struct DecoratorResolver<'a> {
    symbol_table: &'a SymbolTable,
    errors: Vec<Error>,
}

impl<'a> DecoratorResolver<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            symbol_table,
            errors: Vec::new(),
        }
    }

    pub fn check_module(&mut self, module: &Module<'a>) -> Vec<Error> {
        for stmt in module.body {
            self.check_stmt(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    fn check_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::FuncDef(func) => {
                self.check_function_decorators(func);
                // Check nested functions
                for s in func.body {
                    self.check_stmt(s);
                }
            }
            Stmt::ClassDef(class) => {
                self.check_class_decorators(class);
                // Check methods
                for s in class.body {
                    self.check_stmt(s);
                }
            }
            Stmt::If(if_stmt) => {
                for s in if_stmt.body {
                    self.check_stmt(s);
                }
                for s in if_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::While(while_stmt) => {
                for s in while_stmt.body {
                    self.check_stmt(s);
                }
                for s in while_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::For(for_stmt) => {
                for s in for_stmt.body {
                    self.check_stmt(s);
                }
                for s in for_stmt.orelse {
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
                for s in with_stmt.body {
                    self.check_stmt(s);
                }
            }
            Stmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    for s in case.body {
                        self.check_stmt(s);
                    }
                }
            }
            _ => {}
        }
    }

    fn check_function_decorators(&mut self, func: &FuncDefStmt<'a>) {
        if func.decorators.is_empty() {
            return;
        }

        let mut seen_decorators: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        let mut decorator_kinds = Vec::new();

        for (i, decorator) in func.decorators.iter().enumerate() {
            // Extract decorator name
            let (name, span) = self.extract_decorator_name(decorator);

            // Check for duplicates
            if let Some(&first_idx) = seen_decorators.get(&name) {
                self.errors.push(*error(
                    ErrorKind::DuplicateDecorator {
                        name: name.clone(),
                        first_span: func.decorators[first_idx].span(),
                        second_span: span,
                    },
                    func.decorators[first_idx].span(),
                ));
            } else {
                seen_decorators.insert(name.clone(), i);
            }

            // Validate decorator expression
            if !self.is_valid_decorator_expr(decorator) {
                self.errors.push(*error(
                    ErrorKind::InvalidDecoratorExpression {
                        decorator: self.expr_to_string(decorator),
                    },
                    span,
                ));
                continue;
            }

            // Validate decorator factory arguments if it's a call expression
            if let Expr::Call(call) = decorator {
                self.validate_decorator_factory_call(&name, call, span);
            }

            // Check if decorator is defined
            if !self.is_decorator_defined(&name) {
                self.errors.push(*error(
                    ErrorKind::UndefinedDecorator { name: name.clone() },
                    span,
                ));
            }

            // Check decorator kind
            if let Some(kind) = DecoratorKind::from_name(&name) {
                // Check if decorator is valid for functions
                if kind.is_class_only() {
                    self.errors.push(*error(
                        ErrorKind::InvalidDecoratorTarget {
                            decorator: name.clone(),
                            target: "function".to_string(),
                        },
                        span,
                    ));
                }

                // Validate @operator decorator usage
                if kind == DecoratorKind::Operator {
                    use crate::ast::protocols::Protocols;
                    if !Protocols::is_special_method(func.name) {
                        self.errors.push(*error(
                            ErrorKind::InvalidDecoratorTarget {
                                decorator: "operator".to_string(),
                                target: format!(
                                    "method '{}' is not a valid operator method. Valid names: add, subtract, multiply, str, repr, equals, less_than, iter, next, len, getitem, contains, call, enter, exit, etc.",
                                    func.name
                                ),
                            },
                            span,
                        ));
                    }

                    // Validate operator method signature
                    self.validate_operator_signature(func);
                }

                decorator_kinds.push((kind, name, span));
            }
        }

        // Check decorator stacking order
        self.check_decorator_order(&decorator_kinds);

        // Check if special method names have @operator decorator
        self.check_special_method_decorator(func, &decorator_kinds);
    }

    fn check_class_decorators(&mut self, class: &ClassDefStmt<'a>) {
        if class.decorators.is_empty() {
            return;
        }

        let mut seen_decorators: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();

        for (i, decorator) in class.decorators.iter().enumerate() {
            let (name, span) = self.extract_decorator_name(decorator);

            // Check for duplicates
            if let Some(&first_idx) = seen_decorators.get(&name) {
                self.errors.push(*error(
                    ErrorKind::DuplicateDecorator {
                        name: name.clone(),
                        first_span: class.decorators[first_idx].span(),
                        second_span: span,
                    },
                    class.decorators[first_idx].span(),
                ));
            } else {
                seen_decorators.insert(name.clone(), i);
            }

            // Validate decorator expression
            if !self.is_valid_decorator_expr(decorator) {
                self.errors.push(*error(
                    ErrorKind::InvalidDecoratorExpression {
                        decorator: self.expr_to_string(decorator),
                    },
                    span,
                ));
                continue;
            }

            // Validate decorator factory arguments if it's a call expression
            if let Expr::Call(call) = decorator {
                self.validate_decorator_factory_call(&name, call, span);
            }

            // Check if decorator is defined
            if !self.is_decorator_defined(&name) {
                self.errors.push(*error(
                    ErrorKind::UndefinedDecorator { name: name.clone() },
                    span,
                ));
            }

            // Check decorator kind
            if let Some(kind) = DecoratorKind::from_name(&name) {
                // Check if decorator is valid for classes
                if kind.is_function_only() {
                    self.errors.push(*error(
                        ErrorKind::InvalidDecoratorTarget {
                            decorator: name,
                            target: "class".to_string(),
                        },
                        span,
                    ));
                }
            }
        }
    }

    fn check_decorator_order(&mut self, decorators: &[(DecoratorKind, String, TextRange)]) {
        for i in 0..decorators.len() {
            for j in i + 1..decorators.len() {
                let (kind1, name1, _) = &decorators[i];
                let (kind2, name2, span2) = &decorators[j];

                // Check if kind2 must come before kind1 (wrong order)
                if kind2.must_precede(kind1) {
                    self.errors.push(*error(
                        ErrorKind::InvalidDecoratorOrder {
                            reason: format!("@{} must come before @{}", name2, name1),
                        },
                        *span2,
                    ));
                }
            }
        }
    }

    /// Check if special method names have the required @operator decorator
    fn check_special_method_decorator(
        &mut self,
        func: &FuncDefStmt<'a>,
        decorator_kinds: &[(DecoratorKind, String, TextRange)],
    ) {
        use crate::ast::protocols::Protocols;

        // Skip constructor - it's a keyword, not a decorated method
        if func.name == "constructor" {
            return;
        }

        // Check if this is a special method name
        if Protocols::is_special_method(func.name) {
            // Check if it has @operator decorator
            let has_operator = decorator_kinds
                .iter()
                .any(|(kind, _, _)| *kind == DecoratorKind::Operator);

            if !has_operator {
                self.errors.push(*error(
                    ErrorKind::InvalidSyntax {
                        message: format!(
                            "Method '{}' is a special operator method and requires the @operator decorator",
                            func.name
                        ),
                    },
                    func.span,
                ));
            }
        }
    }

    /// Extract the decorator name from a decorator expression
    fn extract_decorator_name(&self, expr: &Expr<'a>) -> (String, TextRange) {
        match expr {
            Expr::Name(name) => (name.id.to_string(), name.span),
            Expr::Attribute(attr) => {
                // Handle dotted names like abc.abstractmethod
                let mut parts = Vec::new();
                let mut current = expr;
                loop {
                    match current {
                        Expr::Attribute(a) => {
                            parts.push(a.attr);
                            current = a.value;
                        }
                        Expr::Name(n) => {
                            parts.push(n.id);
                            break;
                        }
                        _ => break,
                    }
                }
                parts.reverse();
                (parts.join("."), attr.span)
            }
            Expr::Call(call) => {
                // Decorator with arguments: @decorator(args)
                self.extract_decorator_name(call.func)
            }
            _ => ("".to_string(), expr.span()),
        }
    }

    /// Check if decorator expression is valid (name, attribute, or call)
    fn is_valid_decorator_expr(&self, expr: &Expr<'a>) -> bool {
        match expr {
            Expr::Name(_) | Expr::Attribute(_) => true,
            Expr::Call(call) => self.is_valid_decorator_expr(call.func),
            _ => false,
        }
    }

    /// Check if a decorator name is defined (simplified check)
    fn is_decorator_defined(&self, name: &str) -> bool {
        // Check for well-known builtins
        if matches!(
            name,
            "property"
                | "staticmethod"
                | "classmethod"
                | "abstractmethod"
                | "dataclass"
                | "override"
                | "final"
                | "operator"
        ) {
            return true;
        }

        // Check if it's a dotted name from known modules
        if name.starts_with("abc.")
            || name.starts_with("dataclasses.")
            || name.starts_with("typing.")
            || name.starts_with("functools.")
        {
            return true;
        }

        // Check symbol table
        self.symbol_table.lookup(name).is_some()
    }

    /// Convert expression to string for error messages
    fn expr_to_string(&self, expr: &Expr<'a>) -> String {
        match expr {
            Expr::Name(name) => name.id.to_string(),
            Expr::Attribute(attr) => {
                format!("{}.{}", self.expr_to_string(attr.value), attr.attr)
            }
            Expr::Call(call) => format!("{}(...)", self.expr_to_string(call.func)),
            Expr::Constant(c) => format!("{:?}", c.value),
            Expr::List(_) => "[...]".to_string(),
            Expr::Tuple(_) => "(...)".to_string(),
            Expr::Dict(_) => "{...}".to_string(),
            _ => "<expression>".to_string(),
        }
    }

    /// Validate decorator factory call arguments
    fn validate_decorator_factory_call(
        &mut self,
        decorator_name: &str,
        call: &CallExpr<'a>,
        span: TextRange,
    ) {
        // Validate known decorator factories
        match decorator_name {
            "dataclass" => {
                // @dataclass can accept keyword arguments like:
                // @dataclass(frozen=True, order=True, etc.)
                self.validate_dataclass_args(call, span);
            }
            "property" | "staticmethod" | "classmethod" | "abstractmethod" => {
                // These decorators should NOT be called (no arguments)
                self.errors.push(*error(
                    ErrorKind::InvalidDecoratorExpression {
                        decorator: format!(
                            "'{}' should not be called with arguments",
                            decorator_name
                        ),
                    },
                    span,
                ));
            }
            "operator" => {
                // @operator should NOT be called
                self.errors.push(*error(
                    ErrorKind::InvalidDecoratorExpression {
                        decorator: "'operator' should not be called with arguments".to_string(),
                    },
                    span,
                ));
            }
            _ => {
                // For custom decorators, we can't validate arguments without type information
                // This would require type inference integration to check:
                // 1. The decorator returns a callable
                // 2. The returned callable accepts the decorated function/class
                // 3. The factory arguments match the decorator's signature
            }
        }
    }

    /// Validate @dataclass decorator arguments
    fn validate_dataclass_args(&mut self, call: &CallExpr<'a>, span: TextRange) {
        // Valid @dataclass keyword arguments (based on Python's dataclasses)
        let valid_kwargs = [
            "init",
            "repr",
            "eq",
            "order",
            "unsafe_hash",
            "frozen",
            "match_args",
            "kw_only",
            "slots",
        ];

        // Check for positional arguments (not allowed for @dataclass)
        if !call.args.is_empty() {
            self.errors.push(*error(
                ErrorKind::InvalidDecoratorExpression {
                    decorator: "@dataclass does not accept positional arguments".to_string(),
                },
                span,
            ));
        }

        // Validate keyword arguments
        for keyword in call.keywords {
            if let Some(arg_name) = keyword.arg {
                if !valid_kwargs.contains(&arg_name) {
                    self.errors.push(*error(
                        ErrorKind::InvalidDecoratorExpression {
                            decorator: format!(
                                "@dataclass has no keyword argument '{}'. Valid options: {}",
                                arg_name,
                                valid_kwargs.join(", ")
                            ),
                        },
                        keyword.value.span(),
                    ));
                }

                // Validate argument types (should be bool for most)
                match arg_name {
                    "init" | "repr" | "eq" | "order" | "unsafe_hash" | "frozen" | "match_args"
                    | "kw_only" | "slots" => {
                        // These should all be boolean values (True/False)
                        // We check if it's a Name expr with value "True" or "False"
                        let is_bool = match &keyword.value {
                            Expr::Name(name) => name.id == "True" || name.id == "False",
                            Expr::Constant(c) => c.value == "True" || c.value == "False",
                            _ => false,
                        };

                        if !is_bool {
                            self.errors.push(*error(
                                ErrorKind::InvalidDecoratorExpression {
                                    decorator: format!(
                                        "@dataclass argument '{}' should be a boolean (True or False)",
                                        arg_name
                                    ),
                                },
                                keyword.value.span(),
                            ));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Validate operator method signature
    fn validate_operator_signature(&mut self, func: &FuncDefStmt<'a>) {
        let func_name = func.name;
        let param_count = func.args.args.len();

        // Operator methods must have at least self parameter
        if param_count == 0 {
            self.errors.push(*error(
                ErrorKind::InvalidSyntax {
                    message: format!(
                        "Operator method '{}' must have at least 'self' parameter",
                        func_name
                    ),
                },
                func.span,
            ));
            return;
        }

        // Binary operators need exactly 2 parameters (self + other)
        let binary_ops = [
            "__add__",
            "__sub__",
            "__mul__",
            "__truediv__",
            "__mod__",
            "__pow__",
            "__floordiv__",
            "__and__",
            "__or__",
            "__xor__",
            "__lshift__",
            "__rshift__",
            "__eq__",
            "__ne__",
            "__lt__",
            "__le__",
            "__gt__",
            "__ge__",
            "__matmul__",
            "__divmod__",
            // Also check without underscores (if used)
            "add",
            "sub",
            "mul",
            "truediv",
            "mod",
            "pow",
            "floordiv",
            "and",
            "or",
            "xor",
            "lshift",
            "rshift",
            "eq",
            "ne",
            "lt",
            "le",
            "gt",
            "ge",
            "matmul",
            "divmod",
        ];

        if binary_ops.contains(&func_name) && param_count != 2 {
            self.errors.push(*error(
                ErrorKind::InvalidSyntax {
                    message: format!(
                        "Binary operator '{}' must have exactly 2 parameters (self, other), found {}",
                        func_name, param_count
                    ),
                },
                func.span,
            ));
            return;
        }

        // Unary operators need exactly 1 parameter (self)
        let unary_ops = [
            "__neg__",
            "__pos__",
            "__invert__",
            "__abs__",
            "neg",
            "pos",
            "invert",
            "abs",
        ];

        if unary_ops.contains(&func_name) && param_count != 1 {
            self.errors.push(*error(
                ErrorKind::InvalidSyntax {
                    message: format!(
                        "Unary operator '{}' must have exactly 1 parameter (self), found {}",
                        func_name, param_count
                    ),
                },
                func.span,
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decorator_kind_recognition() {
        assert_eq!(
            DecoratorKind::from_name("property"),
            Some(DecoratorKind::Property)
        );
        assert_eq!(
            DecoratorKind::from_name("staticmethod"),
            Some(DecoratorKind::StaticMethod)
        );
        assert_eq!(
            DecoratorKind::from_name("dataclass"),
            Some(DecoratorKind::DataClass)
        );
        assert_eq!(DecoratorKind::from_name("unknown"), None);
    }

    #[test]
    fn test_decorator_precedence() {
        let classmethod = DecoratorKind::ClassMethod;
        let property = DecoratorKind::Property;
        let abstractmethod = DecoratorKind::AbstractMethod;

        assert!(classmethod.must_precede(&property));
        assert!(property.must_precede(&abstractmethod));
        assert!(!property.must_precede(&classmethod));
    }

    #[test]
    fn test_function_only_decorators() {
        assert!(DecoratorKind::Property.is_function_only());
        assert!(DecoratorKind::ClassMethod.is_function_only());
        assert!(!DecoratorKind::DataClass.is_function_only());
    }

    #[test]
    fn test_class_only_decorators() {
        assert!(DecoratorKind::DataClass.is_class_only());
        assert!(!DecoratorKind::Property.is_class_only());
    }
}
