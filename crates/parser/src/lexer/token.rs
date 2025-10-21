//! Token definition and types.

use logos::Logos;
use text_size::{TextRange, TextSize};

/// A positioned token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextRange,
}

impl Token {
    pub fn new(kind: TokenKind, span: TextRange) -> Self {
        Token { kind, span }
    }

    pub fn start(&self) -> TextSize {
        self.span.start()
    }

    pub fn end(&self) -> TextSize {
        self.span.end()
    }
}

/// Lexical token kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum TokenKind {
    None,
    True,
    False,
    If,
    Elif,
    Else,
    While,
    For,
    In,
    Def,
    Class,
    Return,
    Pass,
    Break,
    Continue,
    Import,
    From,
    As,
    Export,
    With,
    Try,
    Except,
    Finally,
    Raise,
    Assert,
    Del,
    Yield,
    Lambda,
    Global,
    Nonlocal,
    And,
    Or,
    Not,
    Is,
    Async,
    Await,
    Match,
    Case,
    Type,
    Ident,
    Number,
    Complex,
    String,
    RawString,
    Bytes,
    RawBytes,
    FString,
    RawFString,
    TString,
    RawTString,
    Plus,
    Minus,
    Star,
    Slash,
    DoubleSlash,
    Percent,
    DoubleStar,
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    LeftShift,
    RightShift,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    DoubleSlashEqual,
    PercentEqual,
    DoubleStarEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    RightShiftEqual,
    LeftShiftEqual,
    ColonEqual,
    AtEqual,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Semicolon,
    Dot,
    Ellipsis,
    Arrow,
    At,
    Newline,
    Indent,
    Dedent,
    Comment,
    Error,
    Eof,
}

/// Logos-based lexer token enum.
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip r"[ \t]+")]
pub enum LogosToken {
    #[token("None")]
    None,
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("if")]
    If,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("def")]
    Def,
    #[token("class")]
    Class,
    #[token("return")]
    Return,
    #[token("pass")]
    Pass,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("import")]
    Import,
    #[token("from")]
    From,
    #[token("as")]
    As,
    #[token("export")]
    Export,
    #[token("with")]
    With,
    #[token("try")]
    Try,
    #[token("except")]
    Except,
    #[token("finally")]
    Finally,
    #[token("raise")]
    Raise,
    #[token("assert")]
    Assert,
    #[token("del")]
    Del,
    #[token("yield")]
    Yield,
    #[token("lambda")]
    Lambda,
    #[token("global")]
    Global,
    #[token("nonlocal")]
    Nonlocal,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("is")]
    Is,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    // Note: match, case, and type are soft keywords - handled as identifiers in lexer,
    // then checked in parser based on context

    // #[token("match")]
    // Match,
    // #[token("case")]
    // Case,
    // #[token("type")]
    // Type,

    // Identifiers support Unicode
    // XID_Start for first char (or underscore), XID_Continue for rest
    // This regex is broad to catch candidates, validation happens in parser if needed
    #[regex(r"[\p{XID_Start}_][\p{XID_Continue}]*")]
    Ident, // Complex numbers must be checked before regular numbers
    // Supports underscores and all number formats
    #[regex(r"(0[bB][01](_?[01])*|0[oO][0-7](_?[0-7])*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])*|[0-9](_?[0-9])*(\.[0-9](_?[0-9])*)?([eE][+-]?[0-9](_?[0-9])*)?)[jJ]")]
    Complex,

    // Binary, octal, hex, decimal with underscores and scientific notation
    #[regex(r"0[bB][01](_?[01])*|0[oO][0-7](_?[0-7])*|0[xX][0-9a-fA-F](_?[0-9a-fA-F])*|[0-9](_?[0-9])*(\.[0-9](_?[0-9])*)?([eE][+-]?[0-9](_?[0-9])*)?")]
    Number,

    // Bytes literals must be checked before regular strings
    #[regex(r#"[bB]"(?:[^"\n\\]|\\.)*"|[bB]'(?:[^'\n\\]|\\.)*'"#)]
    Bytes,

    // Raw bytes literals - backslashes are literal, no escape processing
    #[regex(r#"[rR][bB]"[^"\n]*"|[rR][bB]'[^'\n]*'|[bB][rR]"[^"\n]*"|[bB][rR]'[^'\n]*'"#)]
    RawBytes,

    // Raw strings - must come before regular strings to match first
    // Backslashes are literal, no escape processing
    #[regex(r#"[rR]"[^"\n]*"|[rR]'[^'\n]*'"#)]
    RawString,

    // Regular strings (single and double quoted)
    #[regex(r#""(?:[^"\n\\]|\\.)*"|'(?:[^'\n\\]|\\.)*'"#)]
    String,

    // F-strings (single and double quoted)
    #[regex(r#"[fF]"(?:[^"\n\\]|\\.|(\{[^}]*\}))*"|[fF]'(?:[^'\n\\]|\\.|(\{[^}]*\}))*'"#)]
    FString,

    // Raw F-strings (rf"...", fr"...", RF"...", etc.)
    // In raw f-strings, backslashes are literal but braces still work for interpolation
    #[regex(r#"[rR][fF]"(?:[^"\n]|(\{[^}]*\}))*"|[rR][fF]'(?:[^'\n]|(\{[^}]*\}))*'|[fF][rR]"(?:[^"\n]|(\{[^}]*\}))*"|[fF][rR]'(?:[^'\n]|(\{[^}]*\}))*'"#)]
    RawFString,

    // T-strings (template strings) - single and double quoted
    #[regex(r#"[tT]"(?:[^"\n\\]|\\.|(\{[^}]*\}))*"|[tT]'(?:[^'\n\\]|\\.|(\{[^}]*\}))*'"#)]
    TString,

    // Raw t-strings
    #[regex(r#"[rR][tT]"[^"\n]*"|[rR][tT]'[^'\n]*'|[tT][rR]"[^"\n]*"|[tT][rR]'[^'\n]*'"#)]
    RawTString,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("//")]
    DoubleSlash,
    #[token("%")]
    Percent,
    #[token("**")]
    DoubleStar,
    #[token("=")]
    Equal,
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("<<")]
    LeftShift,
    #[token(">>")]
    RightShift,
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("//=")]
    DoubleSlashEqual,
    #[token("%=")]
    PercentEqual,
    #[token("**=")]
    DoubleStarEqual,
    #[token("&=")]
    AmpersandEqual,
    #[token("|=")]
    PipeEqual,
    #[token("^=")]
    CaretEqual,
    #[token(">>=")]
    RightShiftEqual,
    #[token("<<=")]
    LeftShiftEqual,
    #[token(":=")]
    ColonEqual,
    #[token("@=")]
    AtEqual,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token("...")]
    Ellipsis,
    #[token("->")]
    Arrow,
    #[token("@")]
    At,

    #[token("\n")]
    Newline,

    #[regex(r"#[^\n]*")]
    Comment,
}
