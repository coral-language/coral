// Semantic analysis module for Coral
// This module provides semantic analysis passes that operate on the AST
// produced by the parser, including type checking, name resolution,
// and advanced static analysis.

pub mod cache;
pub mod hir;
pub mod metrics;
pub mod module;
pub mod parallel;
pub mod passes;
pub mod symbol;
pub mod types;
