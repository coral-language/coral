//! Warning system for non-fatal issues detected during compilation.
//!
//! This module provides a unified warning system that integrates with the
//! error architecture. Warnings are organized by category and can be
//! configured via `WarningConfig`.
//!
//! Note: This system is separate from errors. Warnings are for code quality,
//! style, and potential issues that don't prevent compilation.

use super::codes::{ErrorCode, Severity};
use super::diagnostic::Diagnostic;
use text_size::TextRange;

/// Warning categories for organizing different types of warnings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WarningCategory {
    /// Unused code warnings
    UnusedCode,
    /// Shadowing warnings
    Shadowing,
    /// Import-related warnings
    Import,
    /// Deprecated features
    Deprecation,
    /// Indentation warnings
    Indentation,
}

/// Warning kinds - only includes non-fatal, code quality issues.
/// Errors that prevent compilation are in ErrorKind instead.
#[derive(Debug, Clone)]
pub enum WarningKind {
    // ===== Import/Shadowing Warnings (W3xxx) =====
    // These are the warnings currently being used in import_resolution.rs
    /// Import never used in the module
    UnusedImport { name: String, span: TextRange },

    /// Import shadows a builtin name
    ShadowsBuiltin { name: String, span: TextRange },

    /// Import shadows a previous import
    ShadowsImport {
        name: String,
        previous_span: TextRange,
        span: TextRange,
    },

    // ===== Additional Unused Code Warnings =====
    /// Variable assigned but never used
    UnusedVariable { name: String, span: TextRange },

    /// Function defined but never called
    UnusedFunction { name: String, span: TextRange },

    /// Parameter never used in function
    UnusedParameter { name: String, span: TextRange },

    // ===== Deprecation Warnings (W2xxx) =====
    /// Using deprecated feature
    DeprecatedFeature {
        feature: String,
        alternative: Option<String>,
        removal_version: Option<String>,
        span: TextRange,
    },

    // ===== Indentation Warnings =====
    /// Mixed tabs and spaces in indentation
    MixedTabsAndSpaces {
        line_content: String,
        span: TextRange,
    },

    /// Inconsistent indentation style
    InconsistentIndentation {
        expected_style: String,
        found_style: String,
        span: TextRange,
    },
}

/// Warning with metadata and location information.
#[derive(Debug, Clone)]
pub struct Warning {
    pub kind: WarningKind,
    pub span: TextRange,
    pub notes: Vec<String>,
}

impl Warning {
    /// Create a new warning.
    pub fn new(kind: WarningKind, span: TextRange) -> Self {
        Self {
            kind,
            span,
            notes: Vec::new(),
        }
    }

    /// Add a note to this warning.
    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }

    /// Get the warning code.
    pub fn code(&self) -> ErrorCode {
        self.kind.code()
    }

    /// Get the warning category.
    pub fn category(&self) -> WarningCategory {
        self.kind.category()
    }

    /// Convert this warning to a diagnostic.
    pub fn to_diagnostic(&self, source: &str) -> Diagnostic {
        let message = self.kind.format_message();
        let context = super::context::ErrorContext::new(source.to_string(), self.span);

        Diagnostic::new(Severity::Warning, message)
            .with_code(self.code())
            .with_error_type(self.kind.error_type().to_string())
            .with_context(context)
    }
}

impl WarningKind {
    /// Get the error code for this warning.
    pub fn code(&self) -> ErrorCode {
        match self {
            // Currently used warnings (from import_resolution.rs)
            WarningKind::UnusedImport { .. } => ErrorCode::W3003,
            WarningKind::ShadowsBuiltin { .. } => ErrorCode::W3006,
            WarningKind::ShadowsImport { .. } => ErrorCode::W3007,

            // Additional unused code warnings
            WarningKind::UnusedVariable { .. } => ErrorCode::W3001,
            WarningKind::UnusedFunction { .. } => ErrorCode::W3002,
            WarningKind::UnusedParameter { .. } => ErrorCode::W3004,

            // Deprecation warnings
            WarningKind::DeprecatedFeature { .. } => ErrorCode::W2001,

            // Indentation warnings
            WarningKind::MixedTabsAndSpaces { .. } => ErrorCode::W1005,
            WarningKind::InconsistentIndentation { .. } => ErrorCode::W1005,
        }
    }

    /// Get the warning category.
    pub fn category(&self) -> WarningCategory {
        match self {
            WarningKind::UnusedVariable { .. }
            | WarningKind::UnusedFunction { .. }
            | WarningKind::UnusedParameter { .. } => WarningCategory::UnusedCode,

            WarningKind::UnusedImport { .. } => WarningCategory::Import,

            WarningKind::ShadowsBuiltin { .. } | WarningKind::ShadowsImport { .. } => {
                WarningCategory::Shadowing
            }

            WarningKind::DeprecatedFeature { .. } => WarningCategory::Deprecation,

            WarningKind::MixedTabsAndSpaces { .. }
            | WarningKind::InconsistentIndentation { .. } => WarningCategory::Indentation,
        }
    }

    /// Get the error type name for this warning.
    pub fn error_type(&self) -> &'static str {
        match self.category() {
            WarningCategory::Deprecation => "DeprecationWarning",
            WarningCategory::Indentation => "IndentationWarning",
            WarningCategory::UnusedCode => "UnusedCodeWarning",
            WarningCategory::Shadowing => "ShadowingWarning",
            WarningCategory::Import => "ImportWarning",
        }
    }

    /// Format a human-readable message for this warning.
    pub fn format_message(&self) -> String {
        match self {
            WarningKind::UnusedImport { name, .. } => {
                format!("Import '{}' is never used", name)
            }
            WarningKind::ShadowsBuiltin { name, .. } => {
                format!("Name '{}' shadows a builtin", name)
            }
            WarningKind::ShadowsImport { name, .. } => {
                format!("Import '{}' shadows a previous import", name)
            }
            WarningKind::UnusedVariable { name, .. } => {
                format!("Variable '{}' is defined but never used", name)
            }
            WarningKind::UnusedFunction { name, .. } => {
                format!("Function '{}' is defined but never called", name)
            }
            WarningKind::UnusedParameter { name, .. } => {
                format!("Parameter '{}' is never used in function body", name)
            }
            WarningKind::DeprecatedFeature {
                feature,
                alternative,
                removal_version,
                ..
            } => {
                let mut msg = format!("Feature '{}' is deprecated", feature);
                if let Some(alt) = alternative {
                    msg.push_str(&format!("; use '{}' instead", alt));
                }
                if let Some(ver) = removal_version {
                    msg.push_str(&format!(" (will be removed in version {})", ver));
                }
                msg
            }

            WarningKind::MixedTabsAndSpaces { line_content, .. } => {
                format!("Mixed tabs and spaces in indentation: '{}'", line_content)
            }

            WarningKind::InconsistentIndentation {
                expected_style,
                found_style,
                ..
            } => {
                format!(
                    "Inconsistent indentation: expected {}, found {}",
                    expected_style, found_style
                )
            }
        }
    }
}

/// Warning collector for batch reporting.
#[derive(Debug, Clone, Default)]
pub struct WarningCollector {
    warnings: Vec<Warning>,
}

impl WarningCollector {
    /// Create a new warning collector.
    pub fn new() -> Self {
        Self {
            warnings: Vec::new(),
        }
    }

    /// Add a warning.
    pub fn add(&mut self, warning: Warning) {
        self.warnings.push(warning);
    }

    /// Add a warning with just the kind and span.
    pub fn add_warning(&mut self, kind: WarningKind, span: TextRange) {
        self.add(Warning::new(kind, span));
    }

    /// Get all warnings.
    pub fn warnings(&self) -> &[Warning] {
        &self.warnings
    }

    /// Get a mutable reference to all warnings.
    pub fn warnings_mut(&mut self) -> &mut Vec<Warning> {
        &mut self.warnings
    }

    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    /// Get the number of warnings.
    pub fn count(&self) -> usize {
        self.warnings.len()
    }

    /// Clear all warnings.
    pub fn clear(&mut self) {
        self.warnings.clear();
    }

    /// Get warnings by category.
    pub fn by_category(&self, category: WarningCategory) -> Vec<&Warning> {
        self.warnings
            .iter()
            .filter(|w| w.category() == category)
            .collect()
    }

    /// Convert all warnings to diagnostics.
    pub fn to_diagnostics(&self, source: &str) -> Vec<Diagnostic> {
        self.warnings
            .iter()
            .map(|w| w.to_diagnostic(source))
            .collect()
    }

    /// Take all warnings, leaving the collector empty.
    pub fn take(&mut self) -> Vec<Warning> {
        std::mem::take(&mut self.warnings)
    }
}
