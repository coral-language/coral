//! AST to IR lowering
//! Prepares AST for bytecode generation with control flow analysis

pub mod control_flow;
pub mod ssa;
pub mod closure;
