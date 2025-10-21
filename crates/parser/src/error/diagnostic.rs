//! Diagnostic reporting for parse errors.

use super::codes::{ErrorCode, Severity};
use super::context::ErrorContext;
use text_size::TextRange;

/// A diagnostic report for a parse error.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Error severity
    pub severity: Severity,
    /// Error code
    pub code: Option<ErrorCode>,
    /// Error type name (e.g., "SyntaxError", "TypeError")
    pub error_type: Option<String>,
    /// Error title (short description)
    pub title: Option<String>,
    /// Primary error message (detailed description)
    pub message: String,
    /// Suggestion for fixing the error
    pub suggestion: Option<String>,
    /// Error context
    pub context: Option<ErrorContext>,
}

impl Diagnostic {
    /// Create a new diagnostic.
    pub fn new(severity: Severity, message: String) -> Self {
        Diagnostic {
            severity,
            code: None,
            error_type: None,
            title: None,
            message,
            suggestion: None,
            context: None,
        }
    }

    /// Create an error diagnostic.
    pub fn error(message: String) -> Self {
        Self::new(Severity::Error, message)
    }

    /// Create a warning diagnostic.
    pub fn warning(message: String) -> Self {
        Self::new(Severity::Warning, message)
    }

    /// Create an info diagnostic.
    pub fn info(message: String) -> Self {
        Self::new(Severity::Info, message)
    }

    /// Set the error code.
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    /// Set the error type name.
    pub fn with_error_type(mut self, error_type: String) -> Self {
        self.error_type = Some(error_type);
        self
    }

    /// Set the error title.
    pub fn with_title(mut self, title: String) -> Self {
        self.title = Some(title);
        self
    }

    /// Set the suggestion for fixing the error.
    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestion = Some(suggestion);
        self
    }

    /// Set the error context.
    pub fn with_context(mut self, context: ErrorContext) -> Self {
        self.context = Some(context);
        self
    }

    /// Create a diagnostic with code and context from source span.
    pub fn with_code_and_context(
        severity: Severity,
        code: ErrorCode,
        message: String,
        source: &str,
        span: TextRange,
    ) -> Self {
        let context = ErrorContext::new(source.to_string(), span);
        Self::new(severity, message)
            .with_code(code)
            .with_context(context)
    }

    /// Create a diagnostic from source and span.
    pub fn from_source(
        severity: Severity,
        message: String,
        source: String,
        span: TextRange,
    ) -> Self {
        let context = ErrorContext::new(source, span);
        Self::new(severity, message).with_context(context)
    }
}

/// Diagnostic collector for batch reporting.
#[derive(Debug, Clone)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

impl DiagnosticCollector {
    /// Create a new diagnostic collector.
    pub fn new() -> Self {
        DiagnosticCollector {
            diagnostics: Vec::new(),
            error_count: 0,
            warning_count: 0,
        }
    }

    /// Report a diagnostic.
    pub fn report(&mut self, diagnostic: Diagnostic) {
        match diagnostic.severity {
            Severity::Error | Severity::Fatal => self.error_count += 1,
            Severity::Warning => self.warning_count += 1,
            _ => {}
        }
        self.diagnostics.push(diagnostic);
    }

    /// Check if there are any errors.
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Get the number of errors.
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Get the number of warnings.
    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    /// Get all diagnostics.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Clear all diagnostics.
    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.warning_count = 0;
    }
}

impl Default for DiagnosticCollector {
    fn default() -> Self {
        Self::new()
    }
}
