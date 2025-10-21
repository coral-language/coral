//! Indentation tracking for indentation-based syntax.

use super::token::{Token, TokenKind};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use text_size::{TextRange, TextSize};

/// Tracks indentation levels for indentation-based syntax.
pub struct IndentationTracker {
    /// Stack of indentation levels (in spaces)
    indent_stack: Vec<usize>,
}

impl IndentationTracker {
    /// Create a new indentation tracker starting at level 0.
    pub fn new() -> Self {
        IndentationTracker {
            indent_stack: vec![0],
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
            // Increased indentation
            self.indent_stack.push(indent_level);
            tokens.push(Token::new(
                TokenKind::Indent,
                TextRange::new(position, position),
            ));
        } else if indent_level < current_indent {
            // Decreased indentation - may need multiple DEDENTs
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

            // Check for indentation error (indent level doesn't match any level in stack)
            if self.indent_stack.last() != Some(&indent_level) {
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

    /// Calculate indentation level (count leading spaces/tabs).
    pub fn calculate_indent_level(line: &str) -> usize {
        line.len() - line.trim_start().len()
    }
}

impl Default for IndentationTracker {
    fn default() -> Self {
        Self::new()
    }
}
