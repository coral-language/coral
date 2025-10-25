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
/// - When inside triple-quoted strings (""" or '''), newlines are also ignored
/// - This allows multiline expressions like function calls, list literals, and multiline strings
/// - The bracket_depth and triple_quote_depth fields track nesting to implement this behavior
pub struct Lexer {
    input: String,
    indentation: IndentationTracker,
    bracket_depth: usize,
    /// Track if we're inside a triple-quoted string: Some(quote_char) if inside, None otherwise
    triple_quote_depth: Option<char>,
}

impl Lexer {
    /// Create a new lexer for the given source.
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.to_string(),
            indentation: IndentationTracker::new(),
            bracket_depth: 0,
            triple_quote_depth: None,
        }
    }

    /// Get the source text.
    pub fn source(&self) -> &str {
        &self.input
    }

    /// Check if we're currently inside a triple-quoted string
    fn is_in_triple_quotes(&self) -> bool {
        self.triple_quote_depth.is_some()
    }

    /// Count triple-quote occurrences in a string (""" or ''')
    fn count_triple_quotes(text: &str, quote_char: char) -> usize {
        let triple = match quote_char {
            '"' => r#"""""#,
            '\'' => "'''",
            _ => return 0,
        };
        text.matches(triple).count()
    }

    /// Check if a token text contains triple quotes and toggle state if needed
    fn process_triple_quotes(&mut self, token_text: &str) {
        let has_triple_double = token_text.contains(r#"""""#);
        let has_triple_single = token_text.contains("'''");

        if has_triple_double {
            let count = Self::count_triple_quotes(token_text, '"');
            if count % 2 == 1 {
                if let Some(char) = self.triple_quote_depth {
                    if char == '"' {
                        self.triple_quote_depth = None;
                    } else {
                        self.triple_quote_depth = Some('"');
                    }
                } else {
                    self.triple_quote_depth = Some('"');
                }
            }
        }

        if has_triple_single {
            let count = Self::count_triple_quotes(token_text, '\'');
            if count % 2 == 1 {
                if let Some(char) = self.triple_quote_depth {
                    if char == '\'' {
                        self.triple_quote_depth = None;
                    } else {
                        self.triple_quote_depth = Some('\'');
                    }
                } else {
                    self.triple_quote_depth = Some('\'');
                }
            }
        }
    }

    /// Preprocess lines to merge multiline strings into single logical lines
    /// This allows the line-by-line tokenizer to handle multiline strings correctly
    fn preprocess_multiline_strings(lines: &[String]) -> Vec<String> {
        let mut result = Vec::new();
        let mut i = 0;

        while i < lines.len() {
            let line = &lines[i];

            let has_triple_double = line.contains(r#"""""#);
            let has_triple_single = line.contains("'''");

            if has_triple_double || has_triple_single {
                let double_count = Self::count_triple_quotes(line, '"');
                let single_count = Self::count_triple_quotes(line, '\'');

                let mut accumulated = line.clone();
                i += 1;

                let mut in_double_triple = double_count % 2 == 1;
                let mut in_single_triple = single_count % 2 == 1;

                while i < lines.len() && (in_double_triple || in_single_triple) {
                    accumulated.push(' ');
                    accumulated.push_str(&lines[i]);

                    let line_double = Self::count_triple_quotes(&lines[i], '"');
                    let line_single = Self::count_triple_quotes(&lines[i], '\'');

                    if in_double_triple && line_double % 2 == 1 {
                        in_double_triple = false;
                    }
                    if in_single_triple && line_single % 2 == 1 {
                        in_single_triple = false;
                    }

                    i += 1;
                }

                result.push(accumulated);
            } else {
                result.push(line.clone());
                i += 1;
            }
        }

        result
    }

    /// Tokenize the entire input with indentation handling.
    /// Returns tokens, lexical errors, and warnings encountered.
    pub fn tokenize(&mut self) -> (Vec<Token>, Vec<Error>, Vec<Warning>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        let lines_raw: Vec<String> = self.input.lines().map(|s| s.to_string()).collect();

        let lines = Self::preprocess_multiline_strings(&lines_raw);
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
                    line.push_str(&lines[line_idx]);
                    line_len = line.len();
                }
            }

            if self.is_in_triple_quotes() {
                let token_text = line.clone();
                self.process_triple_quotes(&token_text);

                for idx in continuation_lines {
                    line_start_pos += lines.get(idx).map(|l| l.len() + 1).unwrap_or(0);
                }
                line_idx += 1;
                continue;
            }

            if line.trim().is_empty() {
                if line_idx < lines.len() - 1
                    && self.bracket_depth == 0
                    && !self.is_in_triple_quotes()
                {
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

            if self.bracket_depth == 0 && !is_comment_only_line && !self.is_in_triple_quotes() {
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

            let mut string_tokens_to_process = Vec::new();
            let mut bracket_changes = 0i32;
            for token in &line_tokens {
                match token.kind {
                    TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::LeftBrace => {
                        bracket_changes += 1;
                    }
                    TokenKind::RightParen | TokenKind::RightBracket | TokenKind::RightBrace => {
                        bracket_changes -= 1;
                    }
                    TokenKind::String
                    | TokenKind::RawString
                    | TokenKind::FString
                    | TokenKind::RawFString
                    | TokenKind::TString
                    | TokenKind::RawTString => {
                        string_tokens_to_process.push(token.span);
                    }
                    _ => {}
                }
            }

            self.bracket_depth = ((self.bracket_depth as i32) + bracket_changes).max(0) as usize;

            let mut token_texts_to_process = Vec::new();
            for span in string_tokens_to_process {
                let token_start: usize = span.start().into();
                let token_end: usize = span.end().into();
                let line_end = line_start_pos + line.len();
                if token_start >= line_start_pos && token_end <= line_end {
                    let relative_start = token_start - line_start_pos;
                    let relative_end = token_end - line_start_pos;
                    if relative_end <= line.len() {
                        let token_text = line[relative_start..relative_end].to_string();
                        token_texts_to_process.push(token_text);
                    }
                }
            }

            for token_text in token_texts_to_process {
                self.process_triple_quotes(&token_text);
            }

            tokens.extend(line_tokens);

            if line_idx < lines.len() - 1 && self.bracket_depth == 0 && !self.is_in_triple_quotes()
            {
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
