//! Codegen error types

use thiserror::Error;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("Register pressure exceeded: {0}")]
    RegisterPressure(String),

    #[error("Invalid type: {0}")]
    InvalidType(String),

    #[error("Unresolved import: {module} -> {symbol}")]
    UnresolvedImport { module: String, symbol: String },

    #[error("Compilation error: {0}")]
    CompilationError(String),

    #[error("Invalid instruction: {0}")]
    InvalidInstruction(String),

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Async context error: {0}")]
    AsyncError(String),

    #[error("Pattern matching error: {0}")]
    PatternError(String),

    #[error("Module error: {0}")]
    ModuleError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

pub type CodegenResult<T> = Result<T, CodegenError>;
