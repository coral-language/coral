//! Low-level cursor for tokenizing individual lines.

use super::token::{LogosToken, Token, TokenKind};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use logos::Logos;
use text_size::{TextRange, TextSize};

/// A cursor for tokenizing a single line of source code.
pub struct LineCursor {
    line_start_offset: usize,
}

impl LineCursor {
    /// Create a new line cursor with the given offset into the source.
    pub fn new(line_start_offset: usize) -> Self {
        LineCursor { line_start_offset }
    }

    /// Tokenize a single line of code (without indentation handling).
    /// Returns tokens and any lexical errors encountered.
    pub fn tokenize_line(&self, line: &str) -> (Vec<Token>, Vec<Error>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut lexer = LogosToken::lexer(line);

        while let Some(tok_result) = lexer.next() {
            let local_start = lexer.span().start;
            let local_end = lexer.span().end;

            let start = TextSize::from((self.line_start_offset + local_start) as u32);
            let end = TextSize::from((self.line_start_offset + local_end) as u32);
            let span = TextRange::new(start, end);

            match tok_result {
                Ok(tok) => {
                    let kind = self.convert_logos_token(tok);

                    if kind != TokenKind::Newline {
                        tokens.push(Token::new(kind, span));
                    }
                }
                Err(()) => {
                    let error = error(ErrorKind::InvalidCharacter, span);
                    errors.push(*error);
                }
            }
        }

        (tokens, errors)
    }

    /// Convert a LogosToken to TokenKind.
    fn convert_logos_token(&self, tok: LogosToken) -> TokenKind {
        match tok {
            LogosToken::None => TokenKind::None,
            LogosToken::True => TokenKind::True,
            LogosToken::False => TokenKind::False,
            LogosToken::If => TokenKind::If,
            LogosToken::Elif => TokenKind::Elif,
            LogosToken::Else => TokenKind::Else,
            LogosToken::While => TokenKind::While,
            LogosToken::For => TokenKind::For,
            LogosToken::In => TokenKind::In,
            LogosToken::Def => TokenKind::Def,
            LogosToken::Class => TokenKind::Class,
            LogosToken::Protocol => TokenKind::Protocol,
            LogosToken::Implements => TokenKind::Implements,
            LogosToken::Owned => TokenKind::Owned,
            LogosToken::Return => TokenKind::Return,
            LogosToken::Pass => TokenKind::Pass,
            LogosToken::Break => TokenKind::Break,
            LogosToken::Continue => TokenKind::Continue,
            LogosToken::Import => TokenKind::Import,
            LogosToken::From => TokenKind::From,
            LogosToken::As => TokenKind::As,
            LogosToken::Export => TokenKind::Export,
            LogosToken::With => TokenKind::With,
            LogosToken::Try => TokenKind::Try,
            LogosToken::Except => TokenKind::Except,
            LogosToken::Finally => TokenKind::Finally,
            LogosToken::Raise => TokenKind::Raise,
            LogosToken::Assert => TokenKind::Assert,
            LogosToken::Del => TokenKind::Del,
            LogosToken::Yield => TokenKind::Yield,
            LogosToken::Lambda => TokenKind::Lambda,
            LogosToken::Global => TokenKind::Global,
            LogosToken::Nonlocal => TokenKind::Nonlocal,
            LogosToken::And => TokenKind::And,
            LogosToken::Or => TokenKind::Or,
            LogosToken::Not => TokenKind::Not,
            LogosToken::Is => TokenKind::Is,
            LogosToken::Async => TokenKind::Async,
            LogosToken::Await => TokenKind::Await,
            LogosToken::Constructor => TokenKind::Constructor,

            LogosToken::Ident => TokenKind::Ident,
            LogosToken::Number => TokenKind::Number,
            LogosToken::Complex => TokenKind::Complex,
            LogosToken::String => TokenKind::String,
            LogosToken::RawString => TokenKind::RawString,
            LogosToken::Bytes => TokenKind::Bytes,
            LogosToken::RawBytes => TokenKind::RawBytes,
            LogosToken::FString => TokenKind::FString,
            LogosToken::RawFString => TokenKind::RawFString,
            LogosToken::TString => TokenKind::TString,
            LogosToken::RawTString => TokenKind::RawTString,
            LogosToken::Plus => TokenKind::Plus,
            LogosToken::Minus => TokenKind::Minus,
            LogosToken::Star => TokenKind::Star,
            LogosToken::Slash => TokenKind::Slash,
            LogosToken::DoubleSlash => TokenKind::DoubleSlash,
            LogosToken::Percent => TokenKind::Percent,
            LogosToken::DoubleStar => TokenKind::DoubleStar,
            LogosToken::Equal => TokenKind::Equal,
            LogosToken::EqualEqual => TokenKind::EqualEqual,
            LogosToken::NotEqual => TokenKind::NotEqual,
            LogosToken::Less => TokenKind::Less,
            LogosToken::LessEqual => TokenKind::LessEqual,
            LogosToken::Greater => TokenKind::Greater,
            LogosToken::GreaterEqual => TokenKind::GreaterEqual,
            LogosToken::Ampersand => TokenKind::Ampersand,
            LogosToken::Pipe => TokenKind::Pipe,
            LogosToken::Caret => TokenKind::Caret,
            LogosToken::Tilde => TokenKind::Tilde,
            LogosToken::LeftShift => TokenKind::LeftShift,
            LogosToken::RightShift => TokenKind::RightShift,
            LogosToken::PlusEqual => TokenKind::PlusEqual,
            LogosToken::MinusEqual => TokenKind::MinusEqual,
            LogosToken::StarEqual => TokenKind::StarEqual,
            LogosToken::SlashEqual => TokenKind::SlashEqual,
            LogosToken::DoubleSlashEqual => TokenKind::DoubleSlashEqual,
            LogosToken::PercentEqual => TokenKind::PercentEqual,
            LogosToken::DoubleStarEqual => TokenKind::DoubleStarEqual,
            LogosToken::AmpersandEqual => TokenKind::AmpersandEqual,
            LogosToken::PipeEqual => TokenKind::PipeEqual,
            LogosToken::CaretEqual => TokenKind::CaretEqual,
            LogosToken::RightShiftEqual => TokenKind::RightShiftEqual,
            LogosToken::LeftShiftEqual => TokenKind::LeftShiftEqual,
            LogosToken::ColonEqual => TokenKind::ColonEqual,
            LogosToken::AtEqual => TokenKind::AtEqual,
            LogosToken::LeftParen => TokenKind::LeftParen,
            LogosToken::RightParen => TokenKind::RightParen,
            LogosToken::LeftBracket => TokenKind::LeftBracket,
            LogosToken::RightBracket => TokenKind::RightBracket,
            LogosToken::LeftBrace => TokenKind::LeftBrace,
            LogosToken::RightBrace => TokenKind::RightBrace,
            LogosToken::Comma => TokenKind::Comma,
            LogosToken::Colon => TokenKind::Colon,
            LogosToken::Semicolon => TokenKind::Semicolon,
            LogosToken::Dot => TokenKind::Dot,
            LogosToken::Ellipsis => TokenKind::Ellipsis,
            LogosToken::Arrow => TokenKind::Arrow,
            LogosToken::At => TokenKind::At,
            LogosToken::Newline => TokenKind::Newline,
            LogosToken::Comment => TokenKind::Comment,
        }
    }
}
