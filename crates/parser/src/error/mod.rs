//! Centralized error handling system for the Coral compiler.
//!
//! This module provides a unified error system that handles all types of errors
//! from lexical analysis through semantic analysis. All errors are centralized
//! here with consistent error codes, messages, and diagnostic information.

pub mod catalog;
pub mod codes;
pub mod config;
pub mod context;
pub mod diagnostic;
pub mod formatter;
pub mod kinds;
pub mod recovery;
pub mod suggestions;
pub mod types;
pub mod warnings;

pub use catalog::{ErrorCategory, ErrorMetadata};
pub use codes::{ErrorCode, Severity};
pub use config::{ErrorConfig, WarningConfig};
pub use context::ErrorContext;
pub use diagnostic::{Diagnostic, DiagnosticCollector};
pub use formatter::DiagnosticFormatter;
pub use kinds::ErrorKind as UnifiedErrorKind;
pub use recovery::{
    RecoveryAction, RecoveryConfig, RecoveryManager, RecoveryStats, RecoveryStrategy, SyncPoint,
};
pub use suggestions::TypoSuggester;
pub use types::{Error as UnifiedError, ErrorContextExt, RelatedInformation, error};
pub use warnings::{Warning, WarningCategory, WarningCollector, WarningKind};

// Legacy re-exports for backward compatibility with parser
// TODO: Migrate parser to use UnifiedError directly and remove these
pub use kinds::ErrorKind;
pub use types::Error as ParseError;

/// Legacy type alias for backward compatibility
/// TODO: Migrate to Result<T, UnifiedError>
pub type ParseResult<T> = Result<T, ParseError>;
