//! Mutable visitor pattern for AST transformation.
//!
//! **Note**: Due to the arena-allocated immutable AST structure in Coral,
//! true mutable visiting is not possible. This module is provided for
//! API compatibility but has limited functionality.
//!
//! For AST transformations, consider:
//! 1. Creating a new AST with modifications
//! 2. Using the HIR (High-level IR) layer which supports transformations
//! 3. Building transformation passes in the semantic analysis phase

use crate::ast::*;

/// Trait for visiting AST nodes with potential transformation intent.
///
/// **Important**: The arena-allocated AST structure means most transformations
/// require creating new nodes rather than mutating in place.
pub trait MutVisitor<'a> {
    fn visit_module(&mut self, module: &Module<'a>) {
        // Default: visit all statements
        for stmt in module.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, _stmt: &Stmt<'a>) {
        // Override in implementation to perform custom logic
    }

    fn visit_expr(&mut self, _expr: &Expr<'a>) {
        // Override in implementation to perform custom logic
    }

    fn visit_pattern(&mut self, _pattern: &Pattern<'a>) {
        // Override in implementation to perform custom logic
    }
}
