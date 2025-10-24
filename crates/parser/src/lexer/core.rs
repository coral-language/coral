//! Main lexer implementation for source code.

use super::cursor::LineCursor;
use super::indentation::IndentationTracker;
use super::token::{Token, TokenKind};
use crate::error::{UnifiedError as Error, warnings::Warning};
use text_size::{TextRange, TextSize};

/// Lexer that tokenizes source code with indentation tracking.
///
/// This lexer implements implicit line joining rules:
/// - When inside (), [], or {}, newlines are ignored and INDENT/DEDENT are not generated
/// - This allows multiline expressions like function calls, list literals, etc.
/// - The bracket_depth field tracks nesting depth to implement this behavior
pub struct Lexer {
    input: String,
    indentation: IndentationTracker,
    bracket_depth: usize,
}

impl Lexer {
    /// Create a new lexer for the given source.
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
            indentation: IndentationTracker::new(),
            bracket_depth: 0,
        }
    }

    /// Get the source text.
    pub fn source(&self) -> &str {
        &self.input
    }

    /// Tokenize the entire input with indentation handling.
    /// Returns tokens, lexical errors, and warnings encountered.
    pub fn tokenize(&mut self) -> (Vec<Token>, Vec<Error>, Vec<Warning>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let lines = self.input.lines().collect::<Vec<_>>();
        let mut line_start_pos = 0;
        let mut line_idx = 0;

        while line_idx < lines.len() {
            let mut line = lines[line_idx].to_string();
            let mut line_len = line.len();

            let mut continuation_lines = vec![line_idx];
            while line.trim_end().ends_with('\\')
                && !line.trim_end().ends_with("\\\\")
                && line_idx + 1 < lines.len()
            {
                line = line
                    .trim_end()
                    .strip_suffix('\\')
                    .unwrap_or(&line)
                    .to_string();
                line_idx += 1;
                continuation_lines.push(line_idx);

                if line_idx < lines.len() {
                    line.push(' ');
                    line.push_str(lines[line_idx]);
                    line_len = line.len();
                }
            }

            if line.trim().is_empty() {
                if line_idx < lines.len() - 1 && self.bracket_depth == 0 {
                    let newline_pos = TextSize::from((line_start_pos + line_len) as u32);
                    tokens.push(Token::new(
                        TokenKind::Newline,
                        TextRange::new(newline_pos, newline_pos),
                    ));
                }
                line_start_pos += lines.get(line_idx).map(|l| l.len() + 1).unwrap_or(0);
                line_idx += 1;
                continue;
            }

            let line_content = line.trim_start();
            let is_comment_only_line = line_content.starts_with('#') || line_content.is_empty();

            if self.bracket_depth == 0 && !is_comment_only_line {
                let indent_analysis = IndentationTracker::analyze_indent_level(&line);
                let indent_pos = TextSize::from(line_start_pos as u32);

                let line_span = TextRange::new(
                    indent_pos,
                    indent_pos + TextSize::from(indent_analysis.raw_indent.len() as u32),
                );
                let indent_warnings = self
                    .indentation
                    .check_indentation_consistency(&indent_analysis, line_span);
                warnings.extend(indent_warnings);

                let (indent_tokens, indent_errors) = self
                    .indentation
                    .process_indentation(indent_analysis.level, indent_pos);
                tokens.extend(indent_tokens);
                errors.extend(indent_errors);
            }

            let cursor = LineCursor::new(line_start_pos);
            let (line_tokens, line_errors) = cursor.tokenize_line(&line);
            errors.extend(line_errors);

            for token in &line_tokens {
                match token.kind {
                    TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::LeftBrace => {
                        self.bracket_depth += 1;
                    }
                    TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace => {
                        if self.bracket_depth > 0 {
                            self.bracket_depth -= 1;
                        }
                    }
                    _ => {}
                }
            }

            tokens.extend(line_tokens);

            if line_idx < lines.len() - 1 && self.bracket_depth == 0 {
                let newline_pos = TextSize::from((line_start_pos + line_len) as u32);
                tokens.push(Token::new(
                    TokenKind::Newline,
                    TextRange::new(newline_pos, newline_pos),
                ));
            }

            for idx in continuation_lines {
                line_start_pos += lines.get(idx).map(|l| l.len() + 1).unwrap_or(0);
            }

            line_idx += 1;
        }

        let eof_pos = TextSize::from(self.input.len() as u32);
        tokens.extend(self.indentation.finalize(eof_pos));

        tokens.push(Token::new(TokenKind::Eof, TextRange::new(eof_pos, eof_pos)));
        (tokens, errors, warnings)
    }
}
