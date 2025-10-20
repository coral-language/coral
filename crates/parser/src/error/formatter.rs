//! Error formatting and display.

use super::codes::Severity;
use super::config::ErrorConfig;
use super::diagnostic::Diagnostic;
use owo_colors::OwoColorize;

/// Formatter for diagnostic messages.
pub struct DiagnosticFormatter {
    config: ErrorConfig,
}

impl DiagnosticFormatter {
    /// Create a new formatter with the given configuration.
    pub fn new(config: ErrorConfig) -> Self {
        DiagnosticFormatter { config }
    }

    /// Format a diagnostic for display with beautiful colors.
    pub fn format(&self, diagnostic: &Diagnostic) -> String {
        let mut output = String::new();

        // Severity header with error type and code (Error: SyntaxError [E2001])
        match diagnostic.severity {
            Severity::Error | Severity::Fatal => {
                if let Some(error_type) = &diagnostic.error_type {
                    if let Some(code) = &diagnostic.code {
                        output.push_str(&format!(
                            "{}: {} [{}]\n",
                            "Error".bold().red(),
                            error_type.bold(),
                            code.to_string().dimmed()
                        ));
                    } else {
                        output.push_str(&format!(
                            "{}: {}\n",
                            "Error".bold().red(),
                            error_type.bold()
                        ));
                    }
                } else {
                    output.push_str(&format!("{}\n", "Error".bold().red()));
                }
            }
            Severity::Warning => {
                if let Some(error_type) = &diagnostic.error_type {
                    if let Some(code) = &diagnostic.code {
                        output.push_str(&format!(
                            "{}: {} [{}]\n",
                            "Warning".bold().yellow(),
                            error_type.bold(),
                            code.to_string().dimmed()
                        ));
                    } else {
                        output.push_str(&format!(
                            "{}: {}\n",
                            "Warning".bold().yellow(),
                            error_type.bold()
                        ));
                    }
                } else {
                    output.push_str(&format!("{}\n", "Warning".bold().yellow()));
                }
            }
            Severity::Info => {
                if let Some(error_type) = &diagnostic.error_type {
                    if let Some(code) = &diagnostic.code {
                        output.push_str(&format!(
                            "{}: {} [{}]\n",
                            "Info".bold().cyan(),
                            error_type.bold(),
                            code.to_string().dimmed()
                        ));
                    } else {
                        output.push_str(&format!(
                            "{}: {}\n",
                            "Info".bold().cyan(),
                            error_type.bold()
                        ));
                    }
                } else {
                    output.push_str(&format!("{}\n", "Info".bold().cyan()));
                }
            }
        }

        // Separator
        output.push_str(&format!("  {}\n", "|".blue()));

        // Message section - use diagnostic message (specific error),
        // only fall back to context description for generic errors
        let message = &diagnostic.message;

        output.push_str(&format!(
            "  {} {}: {}\n",
            "|".blue(),
            "Message".white().bold(),
            message
        ));

        // Context
        if self.config.show_context
            && let Some(context) = &diagnostic.context
        {
            let (line, col) = context.line_and_column();
            output.push_str(&format!("  {}\n", "|".blue()));

            // Location line with cyan
            let location_str = if let Some(filename) = &context.filename {
                format!("{}:{}:{}", filename, line, col)
            } else {
                format!("line {}:{}", line, col)
            };

            output.push_str(&format!(
                "  +--> {}: {}\n",
                "Location".cyan().bold(),
                location_str
            ));
            output.push_str(&format!("  {}\n", "|".blue()));

            // Context lines
            if self.config.context_lines > 0 {
                let context_lines = context.context_lines(self.config.context_lines);
                let start: usize = context.span.start().into();
                let end: usize = context.span.end().into();
                let line_start = context.source[..start]
                    .rfind('\n')
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let col_offset = start - line_start;
                let length = (end - start).max(1);

                for (line_num, line_text) in context_lines {
                    let is_error_line = line_num == line;
                    if is_error_line {
                        // Error line
                        output.push_str(&format!("  |{:5} | {}\n", line_num, line_text));

                        // Error indicator with yellow carets
                        output.push_str(&format!(
                            "  |      | {}{}\n",
                            " ".repeat(col_offset),
                            "^".repeat(length).bright_yellow().bold()
                        ));

                        // "Here" pointer - show the actual error text location
                        let error_text = if end <= context.source.len() {
                            let text = &context.source[start..end.min(start + 40)];
                            if !text.trim().is_empty() {
                                format!("Error at '{}'", text.trim())
                            } else {
                                "Error at this location".to_string()
                            }
                        } else {
                            "Error at this location".to_string()
                        };

                        output.push_str(&format!(
                            "  |      +-- {}: {}\n",
                            "Here".bright_yellow().bold(),
                            error_text
                        ));
                    } else {
                        // Context line dimmed
                        output.push_str(&format!("  |{:5} | {}\n", line_num, line_text.dimmed()));
                    }
                }
            } else {
                // Just show the error line
                output.push_str(&format!("  |{:5} | {}\n", line, context.error_line()));
            }

            output.push_str(&format!("  {}\n", "|".blue()));

            // Related spans as notes
            for (_, note) in &context.related_spans {
                output.push_str(&format!("  = {}: {}\n", "note".cyan().bold(), note));
            }

            // Suggestion section (separate from context message)
            if let Some(suggestion) = &context.suggestion {
                output.push_str(&format!(
                    "  +--> {}: {}\n",
                    "Suggestion".green().bold(),
                    suggestion
                ));
                output.push('\n');
            }
        }

        output
    }

    /// Format multiple diagnostics.
    pub fn format_all(&self, diagnostics: &[Diagnostic]) -> String {
        diagnostics
            .iter()
            .filter(|d| d.severity >= self.config.min_severity)
            .filter(|d| {
                if let Some(code) = &d.code {
                    !self.config.is_suppressed(&code.to_string())
                } else {
                    true
                }
            })
            .map(|d| self.format(d))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Default for DiagnosticFormatter {
    fn default() -> Self {
        Self::new(ErrorConfig::default())
    }
}
