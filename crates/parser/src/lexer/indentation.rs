//! Indentation tracking for indentation-based syntax.

use super::token::{Token, TokenKind};
use crate::error::{
    UnifiedError as Error, UnifiedErrorKind as ErrorKind, error,
    warnings::{Warning, WarningKind},
};
use text_size::{TextRange, TextSize};

/// Indentation style used in the file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndentStyle {
    /// Uses only spaces for indentation
    Spaces,
    /// Uses only tabs for indentation
    Tabs,
    /// Mixed use of tabs and spaces
    Mixed,
}

/// Analysis result of indentation in a line.
#[derive(Debug, Clone)]
pub struct IndentationAnalysis {
    /// The indentation level (in spaces, with tabs expanded to 8 spaces)
    pub level: usize,
    /// The indentation style detected
    pub style: IndentStyle,
    /// Whether this line has mixed tabs and spaces
    pub has_mixed: bool,
    /// The raw indentation string
    pub raw_indent: String,
}

/// Tracks indentation levels for indentation-based syntax.
pub struct IndentationTracker {
    /// Stack of indentation levels (in spaces)
    indent_stack: Vec<usize>,
    /// The indentation style established for this file
    indentation_style: Option<IndentStyle>,
}

impl IndentationTracker {
    /// Create a new indentation tracker starting at level 0.
    pub fn new() -> Self {
        IndentationTracker {
            indent_stack: vec![0],
            indentation_style: None,
        }
    }

    /// Process indentation change and generate INDENT/DEDENT tokens.
    ///
    /// Returns a vector of tokens and any indentation errors encountered.
    pub fn process_indentation(
        &mut self,
        indent_level: usize,
        position: TextSize,
    ) -> (Vec<Token>, Vec<Error>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let current_indent = *self.indent_stack.last().unwrap();

        if indent_level > current_indent {
            self.indent_stack.push(indent_level);
            tokens.push(Token::new(
                TokenKind::Indent,
                TextRange::new(position, position),
            ));
        } else if indent_level < current_indent {
            let mut is_valid_unindent = false;
            for &level in &self.indent_stack {
                if level == indent_level {
                    is_valid_unindent = true;
                    break;
                }
            }

            if is_valid_unindent {
                while let Some(&level) = self.indent_stack.last() {
                    if level <= indent_level {
                        break;
                    }
                    self.indent_stack.pop();
                    tokens.push(Token::new(
                        TokenKind::Dedent,
                        TextRange::new(position, position),
                    ));
                }
            } else {
                let error = error(
                    ErrorKind::UnindentMismatch,
                    TextRange::new(position, position),
                );
                errors.push(*error);
            }
        }

        (tokens, errors)
    }

    /// Emit remaining DEDENT tokens at end of file.
    pub fn finalize(&mut self, eof_position: TextSize) -> Vec<Token> {
        let mut tokens = Vec::new();

        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(Token::new(
                TokenKind::Dedent,
                TextRange::new(eof_position, eof_position),
            ));
        }

        tokens
    }

    /// Analyze indentation in a line and return detailed information.
    ///
    /// This method detects tabs vs spaces, mixed usage, and calculates the indentation level
    /// (expanding tabs to 8 spaces for consistency).
    pub fn analyze_indent_level(line: &str) -> IndentationAnalysis {
        let raw_indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
        let mut has_spaces = false;
        let mut has_tabs = false;
        let mut level = 0;

        for ch in raw_indent.chars() {
            match ch {
                ' ' => {
                    has_spaces = true;
                    level += 1;
                }
                '\t' => {
                    has_tabs = true;

                    level += 8;
                }
                _ => break,
            }
        }

        let style = match (has_spaces, has_tabs) {
            (true, false) => IndentStyle::Spaces,
            (false, true) => IndentStyle::Tabs,
            (true, true) => IndentStyle::Mixed,
            (false, false) => IndentStyle::Spaces, // No indentation defaults to spaces
        };

        IndentationAnalysis {
            level,
            style,
            has_mixed: has_spaces && has_tabs,
            raw_indent,
        }
    }

    /// Check indentation consistency and generate warnings if needed.
    ///
    /// This method establishes the indentation style for the file and warns about
    /// inconsistencies or mixed usage.
    pub fn check_indentation_consistency(
        &mut self,
        analysis: &IndentationAnalysis,
        line_span: TextRange,
    ) -> Vec<Warning> {
        let mut warnings = Vec::new();

        if analysis.has_mixed {
            warnings.push(Warning::new(
                WarningKind::MixedTabsAndSpaces {
                    line_content: analysis.raw_indent.clone(),
                    span: line_span,
                },
                line_span,
            ));
        }

        if analysis.level > 0 {
            match (&self.indentation_style, analysis.style) {
                (None, IndentStyle::Spaces) => {
                    self.indentation_style = Some(IndentStyle::Spaces);
                }
                (None, IndentStyle::Tabs) => {
                    self.indentation_style = Some(IndentStyle::Tabs);
                }
                (None, IndentStyle::Mixed) => {}
                (Some(IndentStyle::Spaces), IndentStyle::Tabs) => {
                    warnings.push(Warning::new(
                        WarningKind::InconsistentIndentation {
                            expected_style: IndentStyle::Spaces.as_str().to_string(),
                            found_style: IndentStyle::Tabs.as_str().to_string(),
                            span: line_span,
                        },
                        line_span,
                    ));
                }
                (Some(IndentStyle::Tabs), IndentStyle::Spaces) => {
                    warnings.push(Warning::new(
                        WarningKind::InconsistentIndentation {
                            expected_style: IndentStyle::Tabs.as_str().to_string(),
                            found_style: IndentStyle::Spaces.as_str().to_string(),
                            span: line_span,
                        },
                        line_span,
                    ));
                }
                _ => {}
            }
        }

        warnings
    }

    /// Calculate indentation level (count leading spaces/tabs).
    /// DEPRECATED: Use analyze_indent_level for more detailed analysis.
    #[allow(dead_code)]
    pub fn calculate_indent_level(line: &str) -> usize {
        Self::analyze_indent_level(line).level
    }
}

impl Default for IndentationTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl IndentStyle {
    /// Get a string representation of this indentation style.
    pub fn as_str(&self) -> &'static str {
        match self {
            IndentStyle::Spaces => "spaces",
            IndentStyle::Tabs => "tabs",
            IndentStyle::Mixed => "mixed",
        }
    }
}
