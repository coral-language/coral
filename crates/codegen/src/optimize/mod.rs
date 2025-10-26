//! Optimization passes for bytecode
//!
//! This module provides bytecode optimization passes including:
//! - Constant folding
//! - Dead code elimination
//! - Peephole optimization
//! - Inline optimizations

pub mod passes;

pub use passes::OptimizationPass;
