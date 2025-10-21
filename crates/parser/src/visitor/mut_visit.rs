//! Mutable visitor pattern for AST transformation.
//!
//! This module provides a mutable visitor pattern that works with Coral's
//! arena-allocated immutable AST. While true in-place mutation isn't possible
//! due to the arena design, this trait enables transformation passes by:
//!
//! 1. **Traversal**: Walking the AST tree to identify transformation opportunities
//! 2. **Analysis**: Collecting information about nodes that need transformation
//! 3. **Transformation**: Creating new AST nodes with modifications (see `transform.rs`)
//!
//! ## Usage Pattern
//!
//! ```rust,ignore
//! use coral_parser::visitor::{MutVisitor, Transformer};
//!
//! struct MyTransformer<'a> {
//!     transformer: Transformer<'a>,
//!     changes_made: Vec<String>,
//! }
//!
//! impl<'a> MutVisitor<'a> for MyTransformer<'a> {
//!     fn visit_expr(&mut self, expr: &Expr<'a>) {
//!         match expr {
//!             Expr::Constant(c) if c.value == "old_value" => {
//!                 // Mark for transformation
//!                 self.changes_made.push("Replaced constant".to_string());
//!             }
//!             _ => {
//!                 // Continue traversal
//!                 super::mut_walk::walk_expr(self, expr);
//!             }
//!         }
//!     }
//! }
//! ```
//!
//! ## Transformation Strategy
//!
//! For actual AST transformations, use the companion `Transformer` struct
//! which provides arena-aware helpers for creating modified AST nodes:
//!
//! ```rust,ignore
//! // In your transformation logic
//! let new_expr = self.transformer.transform_expr(expr, |expr| {
//!     // Create modified version
//!     match expr {
//!         Expr::Constant(c) => Expr::Constant(ConstantExpr {
//!             value: "new_value",
//!             span: c.span,
//!         }),
//!         other => other.clone(),
//!     }
//! });
//! ```

use crate::ast::*;

/// Trait for visiting AST nodes with potential transformation intent.
///
/// This trait mirrors the immutable `Visitor` trait but accepts `&mut self`
/// to allow implementations to maintain state during traversal. Default
/// implementations use the `mut_walk` module for consistent traversal behavior.
///
/// **Important**: The arena-allocated AST structure means most transformations
/// require creating new nodes rather than mutating in place. See the `transform`
/// module for arena-aware transformation helpers.
pub trait MutVisitor<'a> {
    fn visit_module(&mut self, module: &Module<'a>) {
        super::mut_walk::walk_module(self, module);
    }

    fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
        super::mut_walk::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &Expr<'a>) {
        super::mut_walk::walk_expr(self, expr);
    }

    fn visit_pattern(&mut self, pattern: &Pattern<'a>) {
        super::mut_walk::walk_pattern(self, pattern);
    }

    fn visit_match_case(&mut self, case: &MatchCase<'a>) {
        super::mut_walk::walk_match_case(self, case);
    }

    fn visit_excepthandler(&mut self, handler: &ExceptHandler<'a>) {
        super::mut_walk::walk_excepthandler(self, handler);
    }

    fn visit_arguments(&mut self, args: &Arguments<'a>) {
        super::mut_walk::walk_arguments(self, args);
    }

    fn visit_arg(&mut self, arg: &Arg<'a>) {
        super::mut_walk::walk_arg(self, arg);
    }

    fn visit_keyword(&mut self, keyword: &Keyword<'a>) {
        super::mut_walk::walk_keyword(self, keyword);
    }

    fn visit_withitem(&mut self, item: &WithItem<'a>) {
        super::mut_walk::walk_withitem(self, item);
    }

    fn visit_comprehension(&mut self, comp: &Comprehension<'a>) {
        super::mut_walk::walk_comprehension(self, comp);
    }

    fn visit_type_param(&mut self, type_param: &TypeParam<'a>) {
        super::mut_walk::walk_type_param(self, type_param);
    }
}
