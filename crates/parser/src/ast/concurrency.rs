//! Concurrency-related AST nodes (async/await).

use super::expr::Expr;
use super::nodes::{ForStmt, FuncDefStmt, WithStmt};
use text_size::TextRange;

/// Async function definition marker.
#[derive(Debug, Clone, Copy)]
pub struct AsyncMarker {
    /// Location of the 'async' keyword
    pub span: TextRange,
}

/// Helper to check if a function is async.
pub trait AsyncFunction<'a> {
    /// Check if the function is async.
    fn is_async(&self) -> bool;
}

impl<'a> AsyncFunction<'a> for FuncDefStmt<'a> {
    fn is_async(&self) -> bool {
        self.is_async
    }
}

/// Helper to check if a for loop is async.
pub trait AsyncFor<'a> {
    /// Check if the for loop is async.
    fn is_async(&self) -> bool;
}

impl<'a> AsyncFor<'a> for ForStmt<'a> {
    fn is_async(&self) -> bool {
        self.is_async
    }
}

/// Helper to check if a with statement is async.
pub trait AsyncWith<'a> {
    /// Check if the with statement is async.
    fn is_async(&self) -> bool;
}

impl<'a> AsyncWith<'a> for WithStmt<'a> {
    fn is_async(&self) -> bool {
        self.is_async
    }
}

/// Await expression utilities.
pub struct AwaitHelper;

impl AwaitHelper {
    /// Check if an expression is an await expression.
    pub fn is_await_expr(expr: &Expr) -> bool {
        matches!(expr, Expr::Await(_))
    }

    /// Extract the awaited expression from an await expression.
    pub fn awaited_value<'a>(expr: &'a Expr<'a>) -> Option<&'a Expr<'a>> {
        match expr {
            Expr::Await(await_expr) => Some(await_expr.value),
            _ => None,
        }
    }
}

/// Async context checker for semantic analysis.
pub struct AsyncContext {
    in_async_function: bool,
}

impl AsyncContext {
    /// Create a new async context.
    pub fn new() -> Self {
        AsyncContext {
            in_async_function: false,
        }
    }

    /// Enter an async function.
    pub fn enter_async_function(&mut self) {
        self.in_async_function = true;
    }

    /// Exit an async function.
    pub fn exit_async_function(&mut self) {
        self.in_async_function = false;
    }

    /// Check if currently in an async function.
    pub fn is_in_async_function(&self) -> bool {
        self.in_async_function
    }

    /// Validate an await expression (must be in async context).
    pub fn validate_await(&self) -> Result<(), &'static str> {
        if self.in_async_function {
            Ok(())
        } else {
            Err("'await' outside async function")
        }
    }
}

impl Default for AsyncContext {
    fn default() -> Self {
        Self::new()
    }
}
