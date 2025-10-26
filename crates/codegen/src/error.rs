//! Codegen error types

use text_size::TextRange;
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

    #[error("Compilation error at {}: {}", format_range(.range), .message)]
    CompilationErrorWithLocation { message: String, range: TextRange },

    #[error("Invalid instruction: {0}")]
    InvalidInstruction(String),

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },

    #[error("Type mismatch at {}: expected {}, got {}", format_range(.range), .expected, .actual)]
    TypeMismatchWithLocation {
        expected: String,
        actual: String,
        range: TextRange,
    },

    #[error("Async context error: {0}")]
    AsyncError(String),

    #[error("Async context error at {}: {}", format_range(.range), .message)]
    AsyncErrorWithLocation { message: String, range: TextRange },

    #[error("Pattern matching error: {0}")]
    PatternError(String),

    #[error("Pattern matching error at {}: {}", format_range(.range), .message)]
    PatternErrorWithLocation { message: String, range: TextRange },

    #[error("Module error: {0}")]
    ModuleError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

fn format_range(range: &TextRange) -> String {
    format!("{:?}..{:?}", range.start(), range.end())
}

impl CodegenError {
    pub fn with_location(msg: String, range: TextRange) -> Self {
        CodegenError::CompilationErrorWithLocation {
            message: msg,
            range,
        }
    }

    pub fn type_mismatch_at(expected: String, actual: String, range: TextRange) -> Self {
        CodegenError::TypeMismatchWithLocation {
            expected,
            actual,
            range,
        }
    }

    pub fn async_error_at(message: String, range: TextRange) -> Self {
        CodegenError::AsyncErrorWithLocation { message, range }
    }

    pub fn pattern_error_at(message: String, range: TextRange) -> Self {
        CodegenError::PatternErrorWithLocation { message, range }
    }
}

impl From<String> for CodegenError {
    fn from(err: String) -> Self {
        CodegenError::CompilationError(err)
    }
}

pub type CodegenResult<T> = Result<T, CodegenError>;
