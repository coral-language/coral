//! Visitor pattern for AST traversal with concurrency support.

pub mod mut_visit;
pub mod parallel;
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
        // Use rayon for parallel traversal if needed
        self.visitor.visit_module(module);
    }
}
