//! Typed item nodes for HIR

use super::typed_expr::TypedExpr;
use super::typed_stmt::TypedStmt;
use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// Typed item nodes for top-level declarations
#[derive(Debug, Clone)]
pub enum TypedItem<'a> {
    /// Typed function definition
    Function(TypedFunction<'a>),
    /// Typed class definition
    Class(TypedClass<'a>),
    /// Typed module
    Module(TypedModule<'a>),
    /// Typed type alias
    TypeAlias(TypedTypeAlias<'a>),
}

impl<'a> TypedItem<'a> {
    /// Get the source span of this item
    pub fn span(&self) -> TextRange {
        match self {
            TypedItem::Function(f) => f.span,
            TypedItem::Class(c) => c.span,
            TypedItem::Module(m) => m.span,
            TypedItem::TypeAlias(t) => t.span,
        }
    }
}

/// Typed function definition with resolved types
#[derive(Debug, Clone)]
pub struct TypedFunction<'a> {
    pub name: Symbol,
    pub type_params: &'a [TypedTypeParam<'a>],
    pub args: super::typed_expr::TypedArguments<'a>,
    pub body: &'a [TypedStmt<'a>],
    pub decorators: &'a [TypedExpr<'a>],
    pub returns: Option<&'a TypedExpr<'a>>,
    pub is_async: bool,
    pub ty: Type, // Function type
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

/// Typed class definition with computed MRO and attribute tables
#[derive(Debug, Clone)]
pub struct TypedClass<'a> {
    pub name: Symbol,
    pub type_params: &'a [TypedTypeParam<'a>],
    pub mro: &'a [Symbol],                // Method Resolution Order
    pub attributes: &'a [(Symbol, Type)], // Attribute table
    pub methods: &'a [(Symbol, Type)],    // Method table
    pub bases: &'a [TypedExpr<'a>],
    pub keywords: &'a [super::typed_expr::TypedKeyword<'a>],
    pub body: &'a [TypedStmt<'a>],
    pub decorators: &'a [TypedExpr<'a>],
    pub ty: Type, // Class type
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

/// Typed module with resolved imports and exports
#[derive(Debug, Clone)]
pub struct TypedModule<'a> {
    pub name: Symbol,
    pub body: &'a [TypedStmt<'a>],
    pub imports: &'a [TypedImport<'a>],
    pub exports: &'a [TypedExport<'a>],
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

/// Typed type alias with resolved type
#[derive(Debug, Clone)]
pub struct TypedTypeAlias<'a> {
    pub name: Symbol,
    pub type_params: &'a [TypedTypeParam<'a>],
    pub value: TypedExpr<'a>,
    pub ty: Type, // Resolved type
    pub span: TextRange,
}

/// Typed type parameter
#[derive(Debug, Clone)]
pub struct TypedTypeParam<'a> {
    pub name: Symbol,
    pub bound: Option<&'a TypedExpr<'a>>,
    pub default: Option<&'a TypedExpr<'a>>,
    pub ty: Type, // Resolved type
    pub span: TextRange,
}

/// Typed import
#[derive(Debug, Clone)]
pub struct TypedImport<'a> {
    pub module: Option<Symbol>,
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub level: u32,                            // For relative imports
    pub span: TextRange,
}

/// Typed export
#[derive(Debug, Clone)]
pub struct TypedExport<'a> {
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub module: Option<Symbol>,                // For re-exports
    pub span: TextRange,
}
