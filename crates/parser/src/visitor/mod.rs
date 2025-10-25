//! Visitor pattern for AST traversal with concurrency support.
//!
//! This module provides multiple visitor patterns for working with Coral's AST:
//!
//! ## Immutable Visitor (`Visitor` trait)
//!
//! For read-only traversal of the AST. Provides thread-safe concurrent traversal
//! and is the foundation for analysis passes.
//!
//! ```rust
//! use std::sync::atomic::{AtomicUsize, Ordering};
//! use coral_parser::visitor::Visitor;
//! use coral_parser::Expr;
//! use coral_parser::visitor::walk;
//!
//! struct NodeCounter {
//!     count: AtomicUsize,
//! }
//!
//! impl<'a> Visitor<'a> for NodeCounter {
//!     fn visit_expr(&self, expr: &Expr<'a>) {
//!         self.count.fetch_add(1, Ordering::SeqCst);
//!         walk::walk_expr(self, expr); // Continue traversal
//!     }
//! }
//! ```
//!
//! ## Mutable Visitor (`MutVisitor` trait)
//!
//! For traversal that needs to maintain state. Useful for collecting information
//! about nodes that need transformation or performing analysis that requires
//! mutable state.
//!
//! ## Transformation API (`transform` module)
//!
//! For creating modified AST nodes in Coral's arena-allocated system. Since the
//! AST is immutable, transformations require allocating new nodes.
//!
//! ```rust,ignore
//! let transformer = Transformer::new(&arena);
//! let new_expr = transformer.transform_expr(old_expr, |expr| {
//!     match expr {
//!         Expr::Constant(c) if c.value == "42" => {
//!             Expr::Constant(ConstantExpr {
//!                 value: "forty-two",
//!                 span: c.span,
//!             })
//!         }
//!         other => other.clone(),
//!     }
//! });
//! ```
//!
//! ## Parallel Traversal (`ParallelVisitor`)
//!
//! For concurrent analysis of independent AST subtrees using rayon.
//!
//! ## Walk Functions
//!
//! Each visitor type has corresponding walk functions that implement the default
//! traversal behavior for each AST node type.

pub mod mut_visit;
pub mod mut_walk;
pub mod parallel;
pub mod transform;
pub mod walk;

use crate::ast::*;
use std::sync::Arc;

/// Trait for visiting AST nodes.
pub trait Visitor<'a>: Send + Sync {
    fn visit_module(&self, module: &Module<'a>) {
        walk::walk_module(self, module);
    }

    fn visit_stmt(&self, stmt: &Stmt<'a>) {
        walk::walk_stmt(self, stmt);
    }

    fn visit_expr(&self, expr: &Expr<'a>) {
        walk::walk_expr(self, expr);
    }

    fn visit_pattern(&self, pattern: &Pattern<'a>) {
        walk::walk_pattern(self, pattern);
    }

    fn visit_match_case(&self, case: &MatchCase<'a>) {
        walk::walk_match_case(self, case);
    }

    fn visit_excepthandler(&self, handler: &ExceptHandler<'a>) {
        walk::walk_excepthandler(self, handler);
    }

    fn visit_arguments(&self, args: &Arguments<'a>) {
        walk::walk_arguments(self, args);
    }

    fn visit_arg(&self, arg: &Arg<'a>) {
        walk::walk_arg(self, arg);
    }

    fn visit_keyword(&self, keyword: &Keyword<'a>) {
        walk::walk_keyword(self, keyword);
    }

    fn visit_withitem(&self, item: &WithItem<'a>) {
        walk::walk_withitem(self, item);
    }

    fn visit_comprehension(&self, comp: &Comprehension<'a>) {
        walk::walk_comprehension(self, comp);
    }

    fn visit_type_param(&self, type_param: &TypeParam<'a>) {
        walk::walk_type_param(self, type_param);
    }
}

/// Concurrent visitor runner.
pub struct VisitorRunner<V: Visitor<'static>> {
    visitor: Arc<V>,
}

impl<V: Visitor<'static>> VisitorRunner<V> {
    pub fn new(visitor: V) -> Self {
        VisitorRunner {
            visitor: Arc::new(visitor),
        }
    }

    pub fn visit_module(&self, module: &Module<'static>) {

        self.visitor.visit_module(module);
    }
}
