//! Error reporting configuration.

use super::codes::{ErrorCode, Severity};
use super::warnings::{Warning, WarningCategory};
use std::collections::HashSet;

/// Configuration for error reporting behavior.
#[derive(Debug, Clone)]
pub struct ErrorConfig {
    /// Maximum number of errors before stopping
    pub max_errors: usize,
    /// Whether to show error codes
    pub show_codes: bool,
    /// Whether to show source context
    pub show_context: bool,
    /// Number of context lines to show
    pub context_lines: usize,
    /// Whether to use colored output
    pub use_colors: bool,
    /// Minimum severity level to report
    pub min_severity: Severity,
    /// Error codes to suppress
    pub suppressed_codes: HashSet<String>,
}

impl ErrorConfig {
    /// Create a new error configuration with defaults.
    pub fn new() -> Self {
        ErrorConfig {
            max_errors: 100,
            show_codes: true,
            show_context: true,
            context_lines: 2,
            use_colors: true,
            min_severity: Severity::Warning,
            suppressed_codes: HashSet::new(),
        }
    }

    /// Create a minimal error configuration (codes only, no context).
    pub fn minimal() -> Self {
        ErrorConfig {
            max_errors: 100,
            show_codes: true,
            show_context: false,
            context_lines: 0,
            use_colors: false,
            min_severity: Severity::Error,
            suppressed_codes: HashSet::new(),
        }
    }

    /// Create a verbose error configuration (full context).
    pub fn verbose() -> Self {
        ErrorConfig {
            max_errors: 1000,
            show_codes: true,
            show_context: true,
            context_lines: 5,
            use_colors: true,
            min_severity: Severity::Info,
            suppressed_codes: HashSet::new(),
        }
    }

    /// Suppress specific error codes.
    pub fn suppress_code(&mut self, code: String) {
        self.suppressed_codes.insert(code);
    }

    /// Check if an error code is suppressed.
    pub fn is_suppressed(&self, code: &str) -> bool {
        self.suppressed_codes.contains(code)
    }
}

impl Default for ErrorConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Configuration for warning reporting behavior.
#[derive(Debug, Clone)]
pub struct WarningConfig {
    /// Enable all warnings
    pub all: bool,

    /// Treat warnings as errors
    pub error: bool,

    /// Specific warning categories to enable
    pub enabled_categories: HashSet<WarningCategory>,

    /// Specific warning codes to disable
    pub disabled_codes: HashSet<ErrorCode>,

    /// Maximum cyclomatic complexity before warning
    pub max_complexity: usize,

    /// Maximum number of returns before warning
    pub max_returns: usize,
}

impl Default for WarningConfig {
    fn default() -> Self {
        Self {
            all: false,
            error: false,
            enabled_categories: HashSet::new(),
            disabled_codes: HashSet::new(),
            max_complexity: 10,
            max_returns: 6,
        }
    }
}

impl WarningConfig {
    /// Create a configuration with all warnings enabled
    pub fn all_warnings() -> Self {
        Self {
            all: true,
            ..Default::default()
        }
    }

    /// Enable a specific warning category
    pub fn enable_category(&mut self, category: WarningCategory) {
        self.enabled_categories.insert(category);
    }

    /// Disable a specific warning code
    pub fn disable_code(&mut self, code: ErrorCode) {
        self.disabled_codes.insert(code);
    }

    /// Check if a warning should be reported
    pub fn should_report(&self, warning: &Warning) -> bool {
        if self.disabled_codes.contains(&warning.code()) {
            return false;
        }

        if self.all {
            return true;
        }

        self.enabled_categories.contains(&warning.category())
    }
}
