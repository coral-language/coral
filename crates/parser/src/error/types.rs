//! Unified error type for all Coral compiler errors.
//!
//! This module provides the main Error struct that unifies all error types
//! from lexical analysis through semantic analysis into a single, consistent
//! error representation.

use super::codes::ErrorCode;
use super::context::ErrorContext;
use super::diagnostic::Diagnostic;
use super::kinds::ErrorKind;
use text_size::TextRange;
use thin_vec::ThinVec;

/// Additional information related to the main error.
#[derive(Debug, Clone)]
pub struct RelatedInformation {
    pub span: TextRange,
    pub message: String,
}

/// Unified error type for all Coral compiler errors.
#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: TextRange,
    pub related_info: ThinVec<RelatedInformation>,
    pub notes: ThinVec<String>,
}

impl Error {
    /// Create a new error with the given kind and span.
    pub fn new(kind: ErrorKind, span: TextRange) -> Self {
        Self {
            kind,
            span,
            related_info: ThinVec::new(),
            notes: ThinVec::new(),
        }
    }

    /// Add related information with a span and message.
    pub fn with_related(mut self, span: TextRange, message: String) -> Self {
        self.related_info.push(RelatedInformation { span, message });
        self
    }

    /// Add a note to the error.
    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    /// Convert this error to a diagnostic for display.
    pub fn to_diagnostic(&self, source: &str) -> Diagnostic {
        let metadata = self.kind.metadata();

        // Use metadata description as the message (detailed)
        // Use metadata title as the title (short)
        let mut diagnostic = Diagnostic::new(metadata.severity, metadata.description.to_string())
            .with_code(metadata.code)
            .with_error_type(metadata.error_type.to_string())
            .with_title(metadata.title.to_string())
            .with_context(ErrorContext::new(source.to_string(), self.span));

        // Add suggestion from metadata if available
        if let Some(suggestion) = metadata.suggestion {
            diagnostic = diagnostic.with_suggestion(suggestion.to_string());
        }

        // Add related information as additional context
        for related in &self.related_info {
            let context = ErrorContext::new(source.to_string(), related.span)
                .with_message(related.message.clone());
            diagnostic = diagnostic.with_context(context);
        }

        diagnostic
    }

    /// Get the error code for this error.
    pub fn code(&self) -> ErrorCode {
        self.kind.metadata().code
    }

    /// Get the error category.
    pub fn category(&self) -> &'static str {
        self.kind.metadata().category.as_str()
    }

    /// Get the error type name.
    pub fn error_type(&self) -> &'static str {
        self.kind.metadata().error_type
    }

    /// Get the error title.
    pub fn title(&self) -> &'static str {
        self.kind.metadata().title
    }

    /// Get the error description.
    pub fn description(&self) -> &'static str {
        self.kind.metadata().description
    }

    /// Get the error suggestion.
    pub fn suggestion(&self) -> Option<&'static str> {
        self.kind.metadata().suggestion
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.code(), self.kind.format_message())
    }
}

/// Extension trait for adding error context.
pub trait ErrorContextExt {
    /// Add related span information.
    fn with_span(self, span: TextRange, message: String) -> Self;

    /// Add a note.
    fn with_note(self, note: String) -> Self;
}

impl ErrorContextExt for Error {
    fn with_span(mut self, span: TextRange, message: String) -> Self {
        self.related_info.push(RelatedInformation { span, message });
        self
    }

    fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }
}

/// Creates a new unified error with the given kind and span.
/// This is the preferred way to create errors throughout the codebase.
/// Returns a boxed error to keep Result types small.
#[inline]
pub fn error(kind: ErrorKind, span: TextRange) -> Box<Error> {
    Box::new(Error::new(kind, span))
}
