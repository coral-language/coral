//! Pattern matching AST nodes.

use super::expr::Expr;
use text_size::TextRange;

/// Match statement.
#[derive(Debug, Clone)]
pub struct MatchStmt<'a> {
    /// The subject expression being matched
    pub subject: Expr<'a>,
    /// The match cases
    pub cases: &'a [MatchCase<'a>],
    /// Span of the entire match statement
    pub span: TextRange,
}

/// A single case in a match statement.
#[derive(Debug, Clone)]
pub struct MatchCase<'a> {
    /// The pattern to match
    pub pattern: Pattern<'a>,
    /// Optional guard condition
    pub guard: Option<Expr<'a>>,
    /// Body statements
    pub body: &'a [super::nodes::Stmt<'a>],
    /// Span of this case
    pub span: TextRange,
}

/// Pattern types for pattern matching.
#[derive(Debug, Clone)]
pub enum Pattern<'a> {
    /// Match any value and bind to a name
    MatchAs(MatchAsPattern<'a>),
    /// Match a specific value
    MatchValue(MatchValuePattern<'a>),
    /// Match multiple patterns (or pattern)
    MatchOr(MatchOrPattern<'a>),
    /// Match a sequence pattern
    MatchSequence(MatchSequencePattern<'a>),
    /// Match a mapping pattern
    MatchMapping(MatchMappingPattern<'a>),
    /// Match a class pattern
    MatchClass(MatchClassPattern<'a>),
    /// Match a literal
    MatchSingleton(MatchSingletonPattern),
}

impl<'a> Pattern<'a> {
    pub fn span(&self) -> TextRange {
        match self {
            Pattern::MatchAs(p) => p.span,
            Pattern::MatchValue(p) => p.span,
            Pattern::MatchOr(p) => p.span,
            Pattern::MatchSequence(p) => p.span,
            Pattern::MatchMapping(p) => p.span,
            Pattern::MatchClass(p) => p.span,
            Pattern::MatchSingleton(p) => p.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchAsPattern<'a> {
    pub pattern: Option<Box<Pattern<'a>>>,
    pub name: Option<&'a str>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchValuePattern<'a> {
    pub value: Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchOrPattern<'a> {
    pub patterns: &'a [Pattern<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchSequencePattern<'a> {
    pub patterns: &'a [Pattern<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchMappingPattern<'a> {
    pub keys: &'a [Expr<'a>],
    pub patterns: &'a [Pattern<'a>],
    pub rest: Option<&'a str>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchClassPattern<'a> {
    pub cls: Expr<'a>,
    pub patterns: &'a [Pattern<'a>],
    pub kwd_attrs: &'a [&'a str],
    pub kwd_patterns: &'a [Pattern<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct MatchSingletonPattern {
    pub value: MatchSingleton,
    pub span: TextRange,
}

#[derive(Debug, Clone, Copy)]
pub enum MatchSingleton {
    True,
    False,
    None,
}
