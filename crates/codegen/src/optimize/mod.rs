//! Optimization passes for bytecode

pub mod peephole;
pub mod const_fold;
pub mod dead_code;
pub mod inline;
