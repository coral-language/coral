//! Compiler orchestration
//!
//! Main compilation pass that transforms AST into bytecode.

pub mod context;
pub mod expr;
pub mod stmt;
pub mod pattern;
pub mod function;
pub mod class;
pub mod module;
pub mod async_lowering;
