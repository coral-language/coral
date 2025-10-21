// Decorator resolution and validation
//
// This pass validates decorator usage:
// 1. Checks that decorators reference valid callables
// 2. Validates decorator signatures are compatible with decorated items
// 3. Detects common decorator errors (e.g., @property on classes)
// 4. Validates decorator stacking order (e.g., @classmethod before @property)

#![allow(clippy::only_used_in_recursion)]

use crate::ast::expr::Expr;
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

                decorator_kinds.push((kind, name, span));
            }
        }

        // Check decorator stacking order
        self.check_decorator_order(&decorator_kinds);
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
