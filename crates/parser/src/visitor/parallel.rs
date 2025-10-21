//! Parallel visitor for concurrent AST traversal.
//!
//! This module provides utilities for parallel AST traversal using rayon.
//! Useful for analysis passes that can be parallelized across independent
//! subtrees or modules.

use super::Visitor;
use crate::ast::*;
use rayon::prelude::*;
use std::sync::Arc;

/// Concurrent visitor runner.
pub struct ParallelVisitor<V: Visitor<'static>> {
    visitor: Arc<V>,
}

impl<V: Visitor<'static>> ParallelVisitor<V> {
    pub fn new(visitor: V) -> Self {
        ParallelVisitor {
            visitor: Arc::new(visitor),
        }
    }

    /// Visit a module with potential parallel traversal.
    pub fn visit_module(&self, module: &Module<'static>) {
        self.visitor.visit_module(module);
    }

    /// Visit multiple modules in parallel.
    pub fn visit_modules(&self, modules: &[&Module<'static>]) {
        modules.par_iter().for_each(|module| {
            self.visitor.visit_module(module);
        });
    }

    /// Visit statements in a module body in parallel.
    ///
    /// This is useful when statements are independent and can be analyzed
    /// concurrently (e.g., top-level function definitions).
    pub fn visit_module_parallel(&self, module: &Module<'static>) {
        module.body.par_iter().for_each(|stmt| {
            self.visitor.visit_stmt(stmt);
        });
    }

    /// Get a reference to the inner visitor.
    pub fn visitor(&self) -> &Arc<V> {
        &self.visitor
    }
}

/// Helper function to visit a collection of statements in parallel.
pub fn visit_stmts_parallel<'a, V: Visitor<'a> + Sync>(visitor: &V, stmts: &[&Stmt<'a>]) {
    stmts.par_iter().for_each(|stmt| {
        visitor.visit_stmt(stmt);
    });
}

/// Helper function to visit a collection of expressions in parallel.
pub fn visit_exprs_parallel<'a, V: Visitor<'a> + Sync>(visitor: &V, exprs: &[&Expr<'a>]) {
    exprs.par_iter().for_each(|expr| {
        visitor.visit_expr(expr);
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::Arena;
    use std::sync::atomic::{AtomicUsize, Ordering};

    struct CountingVisitor {
        count: AtomicUsize,
    }

    impl CountingVisitor {
        fn new() -> Self {
            CountingVisitor {
                count: AtomicUsize::new(0),
            }
        }

        fn get_count(&self) -> usize {
            self.count.load(Ordering::SeqCst)
        }
    }

    impl<'a> Visitor<'a> for CountingVisitor {
        fn visit_stmt(&self, stmt: &Stmt<'a>) {
            self.count.fetch_add(1, Ordering::SeqCst);
            super::super::walk::walk_stmt(self, stmt);
        }
    }

    #[test]
    fn test_parallel_visitor() {
        let arena = Box::leak(Box::new(Arena::new()));
        let visitor = CountingVisitor::new();
        let parallel = ParallelVisitor::new(visitor);

        // Create a simple module with multiple statements
        let pass_span = text_size::TextRange::default();
        let stmt1 = Stmt::Pass(pass_span);
        let stmt2 = Stmt::Pass(pass_span);
        let stmt3 = Stmt::Pass(pass_span);
        let stmts = arena.alloc_slice_vec(vec![stmt1, stmt2, stmt3]);

        let module = Module {
            body: stmts,
            span: pass_span,
            docstring: None,
        };

        parallel.visit_module(&module);

        // Should have visited 3 statements
        assert_eq!(parallel.visitor().get_count(), 3);
    }
}
