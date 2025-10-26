//! Compiler orchestration
//!
//! Main compilation pass that transforms AST into bytecode.

pub mod async_lowering;
pub mod class;
pub mod context;
pub mod error_handling;
pub mod expr;
pub mod function;
pub mod module;
pub mod pattern;
pub mod stmt;
