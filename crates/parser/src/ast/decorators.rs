//! Decorator-related AST nodes and utilities.

use super::expr::Expr;
use text_size::TextRange;

/// A decorator application.
#[derive(Debug, Clone)]
pub struct Decorator<'a> {
    /// The decorator expression (function/call)
    pub expr: Expr<'a>,
    /// Location of the @ symbol
    pub span: TextRange,
}

impl<'a> Decorator<'a> {
    /// Create a new decorator.
    pub fn new(expr: Expr<'a>, span: TextRange) -> Self {
        Decorator { expr, span }
    }

    /// Get the decorator's expression.
    pub fn expression(&self) -> &Expr<'a> {
        &self.expr
    }
}

/// Decorator context for tracking decorator chains.
#[derive(Debug, Clone)]
pub struct DecoratorList<'a> {
    decorators: &'a [Expr<'a>],
}

impl<'a> DecoratorList<'a> {
    /// Create a new decorator list.
    pub fn new(decorators: &'a [Expr<'a>]) -> Self {
        DecoratorList { decorators }
    }

    /// Get all decorators.
    pub fn all(&self) -> &'a [Expr<'a>] {
        self.decorators
    }

    /// Check if there are any decorators.
    pub fn is_empty(&self) -> bool {
        self.decorators.is_empty()
    }

    /// Get the number of decorators.
    pub fn len(&self) -> usize {
        self.decorators.len()
    }

    /// Get a decorator by index.
    pub fn get(&self, index: usize) -> Option<&Expr<'a>> {
        self.decorators.get(index)
    }

    /// Iterate over decorators.
    pub fn iter(&self) -> impl Iterator<Item = &Expr<'a>> {
        self.decorators.iter()
    }
}
