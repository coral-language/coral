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

        let error_type = diagnostic
            .error_type
            .as_deref()
            .unwrap_or(match diagnostic.severity {
                Severity::Error | Severity::Fatal => "Error",
                Severity::Warning => "Warning",
                Severity::Info => "Info",
            });

        let title = diagnostic.title.as_deref().unwrap_or("Unknown error");

        let header_text = if let Some(code) = &diagnostic.code {
            format!(" {}: {} [{}] ", error_type, title, code)
        } else {
            format!(" {}: {} ", error_type, title)
        };

        let header = match diagnostic.severity {
            Severity::Fatal | Severity::Error => {
                format!("{}\n", header_text.black().on_red())
            }
            Severity::Warning => {
                format!("{}\n", header_text.black().on_yellow())
            }
            Severity::Info => {
                format!("{}\n", header_text.black().on_blue())
            }
        };

        output.push_str(&header);

        output.push_str(&format!("\n{}\n\n", diagnostic.message));

        if self.config.show_context
            && let Some(context) = &diagnostic.context
        {
            let (line, col) = context.line_and_column();

            let location_str = if let Some(filename) = &context.filename {
                format!("{}:{}:{}", filename, line, col)
            } else {
                format!("line {}:{}", line, col)
            };

            output.push_str(&format!(
                "   {} {}\n",
                "-->".blue(),
                location_str.underline()
            ));

            output.push_str(&format!("    {}\n", "|".cyan()));

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
                        let before_error = if col_offset < line_text.len() {
                            &line_text[..col_offset]
                        } else {
                            &line_text[..]
                        };
                        let error_part = if col_offset < line_text.len() {
                            let end_offset = (col_offset + length).min(line_text.len());
                            &line_text[col_offset..end_offset]
                        } else {
                            ""
                        };
                        let after_error = if col_offset + length < line_text.len() {
                            &line_text[(col_offset + length)..]
                        } else {
                            ""
                        };

                        output.push_str(&format!(
                            " {} {} {}{}{}",
                            format!("{:>2}", line_num).cyan(),
                            "|".cyan(),
                            before_error,
                            error_part.red(),
                            after_error
                        ));
                        output.push('\n');

                        output.push_str(&format!(
                            "    {} {}{}\n",
                            "|".cyan(),
                            " ".repeat(col_offset),
                            "-".repeat(length).red()
                        ));
                    } else {
                        output.push_str(&format!(
                            " {} {} {}\n",
                            format!("{:>2}", line_num).cyan(),
                            "|".cyan(),
                            line_text
                        ));
                    }
                }
            } else {
                output.push_str(&format!(
                    " {} {} {}\n",
                    format!("{:>2}", line).cyan(),
                    "|".cyan(),
                    context.error_line()
                ));
            }

            if !context.related_spans.is_empty() {
                for (_, note) in &context.related_spans {
                    output.push_str(&format!("    {} {}\n", "= note".cyan(), note));
                }
                output.push('\n');
            }
        }

        output.push('\n');

        let suggestion = if let Some(ctx) = &diagnostic.context {
            diagnostic.suggestion.as_ref().or(ctx.suggestion.as_ref())
        } else {
            diagnostic.suggestion.as_ref()
        };

        if let Some(suggestion) = suggestion {
            output.push_str(&format!("{}:\n", "Suggestions".green().bold()));

            for line in suggestion.lines() {
                if !line.trim().is_empty() {
                    output.push_str(&format!("  - {}\n", line.trim()));
                }
            }
            output.push('\n');
        }

        if let Some(code) = &diagnostic.code {
            output.push_str(&format!(
                "{}\n{}\n",
                "For a detailed explanation of this error, visit:".dimmed(),
                format!("https://coral-lang.org/docs/diagnostics/{}", code).underline()
            ));
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
