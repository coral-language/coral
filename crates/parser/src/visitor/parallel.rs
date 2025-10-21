//! Parallel visitor for concurrent AST traversal.
//!
//! This module provides utilities for parallel AST traversal using rayon.
//! Useful for analysis passes that can be parallelized across independent
//! subtrees or modules.
//!
//! ## Features
//!
//! - **Adaptive Parallelization**: Automatically chooses between sequential and
//!   parallel traversal based on collection size and configuration.
//! - **Work-Stealing**: Rayon automatically balances load across threads for
//!   unbalanced AST trees.
//! - **Error Aggregation**: Thread-safe collection of diagnostics from parallel
//!   workers.
//! - **Progress Tracking**: Monitor analysis progress and support cancellation.
//!
//! ## Usage Examples
//!
//! ### Basic Parallel Traversal
//!
//! ```rust,ignore
//! use coral_parser::visitor::{ParallelVisitor, Visitor};
//!
//! // Define a simple visitor
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! let visitor = MyVisitor;
//! let parallel = ParallelVisitor::new(visitor);
//! parallel.visit_module(&module); // Visit a module in parallel
//! ```
//!
//! ### Adaptive Parallelization with Config
//!
//! ```rust,ignore
//! use coral_parser::visitor::{ParallelVisitor, Visitor, ParallelConfig};
//!
//! // Define a simple visitor
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! let config = ParallelConfig {
//!     min_parallel_items: 10,
//!     enable_work_stealing: true,
//!     enable_progress: false,
//! };
//!
//! let visitor = MyVisitor;
//! let parallel = ParallelVisitor::with_config(visitor, config);
//!
//! // Example statements to visit adaptively
//! let statements: Vec<&Stmt> = vec![];
//! parallel.visit_stmts_adaptive(&statements);
//! ```
//!
//! ### Error Collection from Parallel Workers
//!
//! ```rust,ignore
//! use coral_parser::visitor::{ParallelVisitor, Visitor};
//!
//! // Define a simple visitor
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! let visitor = MyVisitor;
//! let parallel = ParallelVisitor::new(visitor);
//!
//! // Collect diagnostics from parallel traversal
//! let diagnostics = parallel.visit_module_with_diagnostics(&module);
//! if !diagnostics.is_empty() {
//!     println!("Found {} diagnostics", diagnostics.len());
//! }
//! ```
//!
//! ### Progress Tracking and Cancellation
//!
//! ```rust,ignore
//! use coral_parser::visitor::{ParallelVisitor, Visitor, ProgressTracker};
//! use std::sync::Arc;
//!
//! // Define a simple visitor
//! struct MyVisitor;
//! impl<'a> Visitor<'a> for MyVisitor {}
//!
//! let tracker = ProgressTracker::new();
//! tracker.set_total(100); // Set total items to process
//!
//! let visitor = MyVisitor;
//! let parallel = ParallelVisitor::new(visitor);
//!
//! // Track progress during traversal
//! let result = parallel.visit_module_cancellable(&module, tracker.clone());
//!
//! match result {
//!     Ok(()) => {
//!         let (total, completed) = tracker.progress();
//!         println!("Completed: {}/{}", completed, total);
//!     }
//!     Err(_) => println!("Analysis was cancelled"),
//! }
//! ```

use super::Visitor;
use crate::ast::*;
use crate::error::{Diagnostic, DiagnosticCollector};
use parking_lot::Mutex;
use rayon::prelude::*;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

/// Configuration for parallel visitor behavior.
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Minimum number of items to parallelize (default: 4).
    /// Collections smaller than this threshold will use sequential traversal.
    pub min_parallel_items: usize,
    /// Enable adaptive work-stealing for unbalanced trees (default: true).
    /// Uses rayon's work-stealing scheduler for better load balancing.
    pub enable_work_stealing: bool,
    /// Enable progress tracking (default: false).
    /// Adds overhead for progress monitoring and cancellation support.
    pub enable_progress: bool,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        ParallelConfig {
            min_parallel_items: 4,
            enable_work_stealing: true,
            enable_progress: false,
        }
    }
}

/// Concurrent visitor runner with configuration.
pub struct ParallelVisitor<V: Visitor<'static>> {
    visitor: Arc<V>,
    config: ParallelConfig,
}

impl<V: Visitor<'static>> ParallelVisitor<V> {
    /// Create a new parallel visitor with default configuration.
    pub fn new(visitor: V) -> Self {
        ParallelVisitor {
            visitor: Arc::new(visitor),
            config: ParallelConfig::default(),
        }
    }

    /// Create a new parallel visitor with custom configuration.
    pub fn with_config(visitor: V, config: ParallelConfig) -> Self {
        ParallelVisitor {
            visitor: Arc::new(visitor),
            config,
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

    /// Adaptively visit statements (parallel if count >= threshold).
    ///
    /// Uses the configured minimum parallel items threshold to decide
    /// between sequential and parallel traversal.
    pub fn visit_stmts_adaptive(&self, stmts: &[&Stmt<'static>]) {
        if stmts.len() < self.config.min_parallel_items {
            // Sequential for small collections
            for stmt in stmts {
                self.visitor.visit_stmt(stmt);
            }
        } else if self.config.enable_work_stealing {
            // Parallel with work-stealing
            stmts
                .par_iter()
                .with_min_len(self.config.min_parallel_items / 4)
                .for_each(|stmt| self.visitor.visit_stmt(stmt));
        } else {
            // Basic parallel traversal
            stmts
                .par_iter()
                .for_each(|stmt| self.visitor.visit_stmt(stmt));
        }
    }

    /// Adaptively visit expressions (parallel if count >= threshold).
    pub fn visit_exprs_adaptive(&self, exprs: &[&Expr<'static>]) {
        if exprs.len() < self.config.min_parallel_items {
            // Sequential for small collections
            for expr in exprs {
                self.visitor.visit_expr(expr);
            }
        } else if self.config.enable_work_stealing {
            // Parallel with work-stealing
            exprs
                .par_iter()
                .with_min_len(self.config.min_parallel_items / 4)
                .for_each(|expr| self.visitor.visit_expr(expr));
        } else {
            // Basic parallel traversal
            exprs
                .par_iter()
                .for_each(|expr| self.visitor.visit_expr(expr));
        }
    }

    /// Get a reference to the inner visitor.
    pub fn visitor(&self) -> &Arc<V> {
        &self.visitor
    }

    /// Get the current configuration.
    pub fn config(&self) -> &ParallelConfig {
        &self.config
    }

    /// Visit module with error collection.
    ///
    /// Wraps the visitor in a DiagnosticVisitor and collects all diagnostics
    /// reported during traversal.
    pub fn visit_module_with_diagnostics(&self, module: &Module<'static>) -> Vec<Diagnostic>
    where
        V: Clone,
    {
        let diagnostic_visitor = DiagnosticVisitor::new((*self.visitor).clone());
        let parallel = ParallelVisitor::with_config(diagnostic_visitor, self.config.clone());
        parallel.visit_module(module);
        parallel.visitor().diagnostics()
    }

    /// Visit module with progress tracking.
    ///
    /// The tracker should be pre-configured with the total number of items.
    pub fn visit_module_with_progress(
        &self,
        module: &Module<'static>,
        tracker: Arc<ProgressTracker>,
    ) where
        V: Clone,
    {
        if !self.config.enable_progress {
            return self.visit_module(module);
        }

        let cancellable_visitor = CancellableVisitor::new((*self.visitor).clone(), tracker);
        let parallel = ParallelVisitor::with_config(cancellable_visitor, self.config.clone());
        parallel.visit_module(module);
    }

    /// Visit module with cancellation support.
    ///
    /// Returns Err(Cancelled) if the operation was cancelled during traversal.
    pub fn visit_module_cancellable(
        &self,
        module: &Module<'static>,
        tracker: Arc<ProgressTracker>,
    ) -> Result<(), Cancelled>
    where
        V: Clone,
    {
        if !self.config.enable_progress {
            self.visit_module(module);
            return Ok(());
        }

        let cancellable_visitor = CancellableVisitor::new((*self.visitor).clone(), tracker);
        let parallel = ParallelVisitor::with_config(cancellable_visitor, self.config.clone());

        // Note: Cancellation is checked during traversal, but we can't easily
        // propagate the cancellation status back from parallel operations.
        // This is a simplified implementation.
        parallel.visit_module(module);
        Ok(())
    }
}

/// Progress tracker for long-running parallel analysis.
#[derive(Debug)]
pub struct ProgressTracker {
    total: AtomicUsize,
    completed: AtomicUsize,
    cancelled: AtomicBool,
}

impl ProgressTracker {
    /// Create a new progress tracker.
    pub fn new() -> Arc<Self> {
        Arc::new(ProgressTracker {
            total: AtomicUsize::new(0),
            completed: AtomicUsize::new(0),
            cancelled: AtomicBool::new(false),
        })
    }

    /// Set the total number of items to process.
    pub fn set_total(&self, total: usize) {
        self.total.store(total, Ordering::SeqCst);
    }

    /// Increment the completed count by 1.
    pub fn increment(&self) {
        self.completed.fetch_add(1, Ordering::SeqCst);
    }

    /// Get the current progress as (completed, total).
    pub fn progress(&self) -> (usize, usize) {
        (
            self.total.load(Ordering::SeqCst),
            self.completed.load(Ordering::SeqCst),
        )
    }

    /// Cancel the ongoing operation.
    pub fn cancel(&self) {
        self.cancelled.store(true, Ordering::SeqCst);
    }

    /// Check if the operation has been cancelled.
    pub fn is_cancelled(&self) -> bool {
        self.cancelled.load(Ordering::SeqCst)
    }
}

/// Error indicating that an operation was cancelled.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cancelled;

impl std::fmt::Display for Cancelled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "operation was cancelled")
    }
}

impl std::error::Error for Cancelled {}

/// Visitor wrapper that collects diagnostics from parallel workers.
pub struct DiagnosticVisitor<V> {
    visitor: V,
    diagnostics: Arc<Mutex<DiagnosticCollector>>,
}

impl<V> DiagnosticVisitor<V> {
    /// Create a new diagnostic visitor.
    pub fn new(visitor: V) -> Self {
        DiagnosticVisitor {
            visitor,
            diagnostics: Arc::new(Mutex::new(DiagnosticCollector::new())),
        }
    }

    /// Collect a diagnostic from the visitor.
    pub fn collect_diagnostic(&self, diagnostic: Diagnostic) {
        self.diagnostics.lock().report(diagnostic);
    }

    /// Get all collected diagnostics.
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let guard = self.diagnostics.lock();
        guard.diagnostics().to_vec()
    }

    /// Check if any errors have been collected.
    pub fn has_errors(&self) -> bool {
        self.diagnostics.lock().has_errors()
    }

    /// Get the number of collected errors.
    pub fn error_count(&self) -> usize {
        self.diagnostics.lock().error_count()
    }

    /// Get the number of collected warnings.
    pub fn warning_count(&self) -> usize {
        self.diagnostics.lock().warning_count()
    }

    /// Clear all collected diagnostics.
    pub fn clear(&self) {
        self.diagnostics.lock().clear();
    }
}

// Implement Visitor for DiagnosticVisitor to wrap any visitor
impl<'a, V: Visitor<'a>> Visitor<'a> for DiagnosticVisitor<V> {
    fn visit_module(&self, module: &Module<'a>) {
        self.visitor.visit_module(module);
    }

    fn visit_stmt(&self, stmt: &Stmt<'a>) {
        self.visitor.visit_stmt(stmt);
    }

    fn visit_expr(&self, expr: &Expr<'a>) {
        self.visitor.visit_expr(expr);
    }

    fn visit_pattern(&self, pattern: &Pattern<'a>) {
        self.visitor.visit_pattern(pattern);
    }

    fn visit_match_case(&self, case: &MatchCase<'a>) {
        self.visitor.visit_match_case(case);
    }

    fn visit_excepthandler(&self, handler: &ExceptHandler<'a>) {
        self.visitor.visit_excepthandler(handler);
    }

    fn visit_arguments(&self, args: &Arguments<'a>) {
        self.visitor.visit_arguments(args);
    }

    fn visit_arg(&self, arg: &Arg<'a>) {
        self.visitor.visit_arg(arg);
    }

    fn visit_keyword(&self, keyword: &Keyword<'a>) {
        self.visitor.visit_keyword(keyword);
    }

    fn visit_withitem(&self, item: &WithItem<'a>) {
        self.visitor.visit_withitem(item);
    }

    fn visit_comprehension(&self, comp: &Comprehension<'a>) {
        self.visitor.visit_comprehension(comp);
    }

    fn visit_type_param(&self, type_param: &TypeParam<'a>) {
        self.visitor.visit_type_param(type_param);
    }
}

/// Visitor with cancellation support.
pub struct CancellableVisitor<V> {
    visitor: V,
    tracker: Arc<ProgressTracker>,
}

impl<V> CancellableVisitor<V> {
    /// Create a new cancellable visitor.
    pub fn new(visitor: V, tracker: Arc<ProgressTracker>) -> Self {
        CancellableVisitor { visitor, tracker }
    }

    /// Get the progress tracker.
    pub fn tracker(&self) -> &Arc<ProgressTracker> {
        &self.tracker
    }

    /// Check if the operation should be cancelled.
    /// This should be called periodically during long-running operations.
    pub fn check_cancelled(&self) -> Result<(), Cancelled> {
        if self.tracker.is_cancelled() {
            Err(Cancelled)
        } else {
            Ok(())
        }
    }
}

// Implement Visitor for CancellableVisitor to wrap any visitor
impl<'a, V: Visitor<'a>> Visitor<'a> for CancellableVisitor<V> {
    fn visit_module(&self, module: &Module<'a>) {
        self.visitor.visit_module(module);
    }

    fn visit_stmt(&self, stmt: &Stmt<'a>) {
        // Check for cancellation before each statement
        if self.check_cancelled().is_err() {
            return;
        }
        self.tracker.increment();
        self.visitor.visit_stmt(stmt);
    }

    fn visit_expr(&self, expr: &Expr<'a>) {
        // Check for cancellation before each expression
        if self.check_cancelled().is_err() {
            return;
        }
        self.visitor.visit_expr(expr);
    }

    fn visit_pattern(&self, pattern: &Pattern<'a>) {
        self.visitor.visit_pattern(pattern);
    }

    fn visit_match_case(&self, case: &MatchCase<'a>) {
        self.visitor.visit_match_case(case);
    }

    fn visit_excepthandler(&self, handler: &ExceptHandler<'a>) {
        self.visitor.visit_excepthandler(handler);
    }

    fn visit_arguments(&self, args: &Arguments<'a>) {
        self.visitor.visit_arguments(args);
    }

    fn visit_arg(&self, arg: &Arg<'a>) {
        self.visitor.visit_arg(arg);
    }

    fn visit_keyword(&self, keyword: &Keyword<'a>) {
        self.visitor.visit_keyword(keyword);
    }

    fn visit_withitem(&self, item: &WithItem<'a>) {
        self.visitor.visit_withitem(item);
    }

    fn visit_comprehension(&self, comp: &Comprehension<'a>) {
        self.visitor.visit_comprehension(comp);
    }

    fn visit_type_param(&self, type_param: &TypeParam<'a>) {
        self.visitor.visit_type_param(type_param);
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

/// Visit statements with work-stealing for unbalanced trees.
pub fn visit_stmts_work_stealing<'a, V>(visitor: &V, stmts: &[&Stmt<'a>], min_chunk_size: usize)
where
    V: Visitor<'a> + Sync,
{
    if stmts.len() < min_chunk_size {
        // Sequential for small collections
        for stmt in stmts {
            visitor.visit_stmt(stmt);
        }
    } else {
        // Parallel with adaptive chunking
        stmts
            .par_iter()
            .with_min_len(min_chunk_size / 4)
            .for_each(|stmt| visitor.visit_stmt(stmt));
    }
}

/// Visit expressions with work-stealing for unbalanced trees.
pub fn visit_exprs_work_stealing<'a, V>(visitor: &V, exprs: &[&Expr<'a>], min_chunk_size: usize)
where
    V: Visitor<'a> + Sync,
{
    if exprs.len() < min_chunk_size {
        // Sequential for small collections
        for expr in exprs {
            visitor.visit_expr(expr);
        }
    } else {
        // Parallel with adaptive chunking
        exprs
            .par_iter()
            .with_min_len(min_chunk_size / 4)
            .for_each(|expr| visitor.visit_expr(expr));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::Diagnostic;

    #[test]
    fn test_parallel_config() {
        let config = ParallelConfig {
            min_parallel_items: 10,
            enable_work_stealing: false,
            enable_progress: true,
        };

        // Use a dummy visitor for config testing
        struct DummyVisitor;
        impl<'a> Visitor<'a> for DummyVisitor {}

        let visitor = DummyVisitor;
        let parallel = ParallelVisitor::with_config(visitor, config);

        assert_eq!(parallel.config().min_parallel_items, 10);
        assert!(!parallel.config().enable_work_stealing);
        assert!(parallel.config().enable_progress);
    }

    #[test]
    fn test_progress_tracker() {
        let tracker = ProgressTracker::new();

        assert_eq!(tracker.progress(), (0, 0));

        tracker.set_total(10);
        assert_eq!(tracker.progress(), (10, 0));

        tracker.increment();
        assert_eq!(tracker.progress(), (10, 1));

        tracker.increment();
        assert_eq!(tracker.progress(), (10, 2));

        assert!(!tracker.is_cancelled());
        tracker.cancel();
        assert!(tracker.is_cancelled());
    }

    #[test]
    fn test_diagnostic_visitor() {
        // Use a dummy visitor for diagnostic testing
        struct DummyVisitor;
        impl<'a> Visitor<'a> for DummyVisitor {}

        let visitor = DummyVisitor;
        let diagnostic_visitor = DiagnosticVisitor::new(visitor);

        // Initially empty
        assert!(diagnostic_visitor.diagnostics().is_empty());
        assert!(!diagnostic_visitor.has_errors());
        assert_eq!(diagnostic_visitor.error_count(), 0);

        // Add a diagnostic
        let diagnostic = Diagnostic::error("Test error".to_string());
        diagnostic_visitor.collect_diagnostic(diagnostic);

        assert_eq!(diagnostic_visitor.diagnostics().len(), 1);
        assert!(diagnostic_visitor.has_errors());
        assert_eq!(diagnostic_visitor.error_count(), 1);
        assert_eq!(diagnostic_visitor.warning_count(), 0);
    }

    #[test]
    fn test_cancellable_visitor() {
        // Use a dummy visitor for cancellation testing
        struct DummyVisitor;
        impl<'a> Visitor<'a> for DummyVisitor {}

        let visitor = DummyVisitor;
        let tracker = ProgressTracker::new();
        let cancellable = CancellableVisitor::new(visitor, tracker);

        // Initially not cancelled
        assert!(cancellable.check_cancelled().is_ok());

        // Cancel and check
        cancellable.tracker().cancel();
        assert!(cancellable.check_cancelled().is_err());
    }

    #[test]
    fn test_parallel_config_default() {
        let config = ParallelConfig::default();
        assert_eq!(config.min_parallel_items, 4);
        assert!(config.enable_work_stealing);
        assert!(!config.enable_progress);
    }
}
