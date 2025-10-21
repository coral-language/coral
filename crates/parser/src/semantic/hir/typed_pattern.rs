//! Typed pattern nodes for HIR

use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// Typed pattern nodes with resolved names and type information
#[derive(Debug, Clone)]
pub enum TypedPattern<'a> {
    /// Match any value and bind to a name
    MatchAs(TypedMatchAsPattern<'a>),
    /// Match a specific value
    MatchValue(TypedMatchValuePattern<'a>),
    /// Match multiple patterns (or pattern)
    MatchOr(TypedMatchOrPattern<'a>),
    /// Match a sequence pattern
    MatchSequence(TypedMatchSequencePattern<'a>),
    /// Match a mapping pattern
    MatchMapping(TypedMatchMappingPattern<'a>),
    /// Match a class pattern
    MatchClass(TypedMatchClassPattern<'a>),
    /// Match a literal singleton
    MatchSingleton(TypedMatchSingletonPattern),
}

impl<'a> TypedPattern<'a> {
    /// Get the type of this pattern
    pub fn ty(&self) -> &Type {
        match self {
            TypedPattern::MatchAs(p) => &p.ty,
            TypedPattern::MatchValue(p) => &p.ty,
            TypedPattern::MatchOr(p) => &p.ty,
            TypedPattern::MatchSequence(p) => &p.ty,
            TypedPattern::MatchMapping(p) => &p.ty,
            TypedPattern::MatchClass(p) => &p.ty,
            TypedPattern::MatchSingleton(p) => &p.ty,
        }
    }

    /// Get the source span of this pattern
    pub fn span(&self) -> TextRange {
        match self {
            TypedPattern::MatchAs(p) => p.span,
            TypedPattern::MatchValue(p) => p.span,
            TypedPattern::MatchOr(p) => p.span,
            TypedPattern::MatchSequence(p) => p.span,
            TypedPattern::MatchMapping(p) => p.span,
            TypedPattern::MatchClass(p) => p.span,
            TypedPattern::MatchSingleton(p) => p.span,
        }
    }

    /// Get all symbols bound by this pattern
    pub fn bound_symbols(&self) -> Vec<Symbol> {
        match self {
            TypedPattern::MatchAs(p) => {
                let mut symbols = Vec::new();
                if let Some(symbol) = p.name {
                    symbols.push(symbol);
                }
                if let Some(pattern) = p.pattern {
                    symbols.extend(pattern.bound_symbols());
                }
                symbols
            }
            TypedPattern::MatchValue(_) => Vec::new(),
            TypedPattern::MatchOr(p) => {
                let mut symbols = Vec::new();
                for pattern in p.patterns {
                    symbols.extend(pattern.bound_symbols());
                }
                symbols
            }
            TypedPattern::MatchSequence(p) => {
                let mut symbols = Vec::new();
                for pattern in p.patterns {
                    symbols.extend(pattern.bound_symbols());
                }
                symbols
            }
            TypedPattern::MatchMapping(p) => {
                let mut symbols = Vec::new();
                for pattern in p.patterns {
                    symbols.extend(pattern.bound_symbols());
                }
                symbols
            }
            TypedPattern::MatchClass(p) => {
                let mut symbols = Vec::new();
                for pattern in p.patterns {
                    symbols.extend(pattern.bound_symbols());
                }
                for pattern in p.kwd_patterns {
                    symbols.extend(pattern.bound_symbols());
                }
                symbols
            }
            TypedPattern::MatchSingleton(_) => Vec::new(),
        }
    }
}

/// Typed match as pattern
#[derive(Debug, Clone)]
pub struct TypedMatchAsPattern<'a> {
    pub pattern: Option<&'a TypedPattern<'a>>,
    pub name: Option<Symbol>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match value pattern
#[derive(Debug, Clone)]
pub struct TypedMatchValuePattern<'a> {
    pub value: super::typed_expr::TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match or pattern
#[derive(Debug, Clone)]
pub struct TypedMatchOrPattern<'a> {
    pub patterns: &'a [TypedPattern<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match sequence pattern
#[derive(Debug, Clone)]
pub struct TypedMatchSequencePattern<'a> {
    pub patterns: &'a [TypedPattern<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match mapping pattern
#[derive(Debug, Clone)]
pub struct TypedMatchMappingPattern<'a> {
    pub keys: &'a [super::typed_expr::TypedExpr<'a>],
    pub patterns: &'a [TypedPattern<'a>],
    pub rest: Option<Symbol>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match class pattern
#[derive(Debug, Clone)]
pub struct TypedMatchClassPattern<'a> {
    pub cls: super::typed_expr::TypedExpr<'a>,
    pub patterns: &'a [TypedPattern<'a>],
    pub kwd_attrs: &'a [Symbol],
    pub kwd_patterns: &'a [TypedPattern<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match singleton pattern
#[derive(Debug, Clone)]
pub struct TypedMatchSingletonPattern {
    pub value: TypedMatchSingleton,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed match singleton values
#[derive(Debug, Clone, Copy)]
pub enum TypedMatchSingleton {
    True,
    False,
    None,
}
