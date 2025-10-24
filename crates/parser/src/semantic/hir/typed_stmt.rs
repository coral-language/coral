//! Typed statement nodes for HIR

use super::typed_expr::TypedExpr;
use super::typed_pattern::TypedPattern;
use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// Typed statement nodes with resolved names and type information
#[derive(Debug, Clone)]
pub enum TypedStmt<'a> {
    /// Expression statement
    Expr(TypedExprStmt<'a>),
    /// Assignment statement
    Assign(TypedAssignStmt<'a>),
    /// Annotated assignment statement
    AnnAssign(TypedAnnAssignStmt<'a>),
    /// Augmented assignment statement (may be desugared)
    AugAssign(TypedAugAssignStmt<'a>),
    /// Return statement
    Return(TypedReturnStmt<'a>),
    /// If statement
    If(TypedIfStmt<'a>),
    /// While statement
    While(TypedWhileStmt<'a>),
    /// For statement with explicit iterator protocol
    For(Box<TypedForStmt<'a>>),
    /// Function definition
    FuncDef(TypedFuncDefStmt<'a>),
    /// Class definition with computed MRO
    ClassDef(TypedClassDefStmt<'a>),
    /// Pass statement
    Pass(TextRange),
    /// Break statement
    Break(TextRange),
    /// Continue statement
    Continue(TextRange),
    /// Import statement
    Import(TypedImportStmt<'a>),
    /// From import statement
    From(TypedFromStmt<'a>),
    /// Export statement
    Export(TypedExportStmt<'a>),
    /// Raise statement
    Raise(TypedRaiseStmt<'a>),
    /// Try statement
    Try(TypedTryStmt<'a>),
    /// With statement with explicit protocol
    With(TypedWithStmt<'a>),
    /// Assert statement
    Assert(TypedAssertStmt<'a>),
    /// Delete statement
    Delete(TypedDeleteStmt<'a>),
    /// Global statement
    Global(TypedGlobalStmt<'a>),
    /// Nonlocal statement
    Nonlocal(TypedNonlocalStmt<'a>),
    /// Match statement with typed patterns
    Match(TypedMatchStmt<'a>),
    /// Yield statement
    Yield(TypedYieldStmt<'a>),
    /// Type alias statement
    TypeAlias(TypedTypeAliasStmt<'a>),
}

impl<'a> TypedStmt<'a> {
    /// Get the source span of this statement
    pub fn span(&self) -> TextRange {
        match self {
            TypedStmt::Expr(s) => s.span,
            TypedStmt::Assign(s) => s.span,
            TypedStmt::AnnAssign(s) => s.span,
            TypedStmt::AugAssign(s) => s.span,
            TypedStmt::Return(s) => s.span,
            TypedStmt::If(s) => s.span,
            TypedStmt::While(s) => s.span,
            TypedStmt::For(s) => s.span,
            TypedStmt::FuncDef(s) => s.span,
            TypedStmt::ClassDef(s) => s.span,
            TypedStmt::Pass(s) | TypedStmt::Break(s) | TypedStmt::Continue(s) => *s,
            TypedStmt::Import(s) => s.span,
            TypedStmt::From(s) => s.span,
            TypedStmt::Export(s) => s.span,
            TypedStmt::Raise(s) => s.span,
            TypedStmt::Try(s) => s.span,
            TypedStmt::With(s) => s.span,
            TypedStmt::Assert(s) => s.span,
            TypedStmt::Delete(s) => s.span,
            TypedStmt::Global(s) => s.span,
            TypedStmt::Nonlocal(s) => s.span,
            TypedStmt::Match(s) => s.span,
            TypedStmt::Yield(s) => s.span,
            TypedStmt::TypeAlias(s) => s.span,
        }
    }
}

/// Typed expression statement
#[derive(Debug, Clone)]
pub struct TypedExprStmt<'a> {
    pub value: TypedExpr<'a>,
    pub span: TextRange,
}

/// Typed assignment statement
#[derive(Debug, Clone)]
pub struct TypedAssignStmt<'a> {
    pub targets: &'a [TypedExpr<'a>],
    pub value: TypedExpr<'a>,
    pub span: TextRange,
}

/// Typed annotated assignment statement
#[derive(Debug, Clone)]
pub struct TypedAnnAssignStmt<'a> {
    pub target: TypedExpr<'a>,
    pub annotation: TypedExpr<'a>,
    pub value: Option<TypedExpr<'a>>,
    pub span: TextRange,
}

/// Typed augmented assignment statement
#[derive(Debug, Clone)]
pub struct TypedAugAssignStmt<'a> {
    pub target: TypedExpr<'a>,
    pub op: &'a str,
    pub value: TypedExpr<'a>,
    pub span: TextRange,
}

/// Typed return statement
#[derive(Debug, Clone)]
pub struct TypedReturnStmt<'a> {
    pub value: Option<TypedExpr<'a>>,
    pub span: TextRange,
}

/// Typed if statement
#[derive(Debug, Clone)]
pub struct TypedIfStmt<'a> {
    pub test: TypedExpr<'a>,
    pub body: &'a [TypedStmt<'a>],
    pub orelse: &'a [TypedStmt<'a>],
    pub span: TextRange,
}

/// Typed while statement
#[derive(Debug, Clone)]
pub struct TypedWhileStmt<'a> {
    pub test: TypedExpr<'a>,
    pub body: &'a [TypedStmt<'a>],
    pub orelse: &'a [TypedStmt<'a>],
    pub span: TextRange,
}

/// Typed for statement with explicit iterator protocol
#[derive(Debug, Clone)]
pub struct TypedForStmt<'a> {
    pub target: TypedExpr<'a>,
    pub iter: TypedExpr<'a>,
    pub iter_call: TypedExpr<'a>, // Explicit __iter__() call
    pub next_call: TypedExpr<'a>, // Explicit __next__() call
    pub body: &'a [TypedStmt<'a>],
    pub orelse: &'a [TypedStmt<'a>],
    pub is_async: bool,
    pub span: TextRange,
}

/// Typed function definition
#[derive(Debug, Clone)]
pub struct TypedFuncDefStmt<'a> {
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

/// Typed class definition with computed MRO
#[derive(Debug, Clone)]
pub struct TypedClassDefStmt<'a> {
    pub name: Symbol,
    pub type_params: &'a [TypedTypeParam<'a>],
    pub bases: &'a [TypedExpr<'a>],
    pub keywords: &'a [super::typed_expr::TypedKeyword<'a>],
    pub body: &'a [TypedStmt<'a>],
    pub decorators: &'a [TypedExpr<'a>],
    pub is_protocol: bool,
    pub mro: &'a [Symbol],                // Method Resolution Order
    pub attributes: &'a [(Symbol, Type)], // Attribute table
    pub methods: &'a [(Symbol, Type)],    // Method table
    pub ty: Type,                         // Class type
    pub span: TextRange,
    pub docstring: Option<&'a str>,
}

/// Typed import statement
#[derive(Debug, Clone)]
pub struct TypedImportStmt<'a> {
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub span: TextRange,
}

/// Typed from import statement
#[derive(Debug, Clone)]
pub struct TypedFromStmt<'a> {
    pub level: u32,
    pub module: Option<Symbol>,
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub span: TextRange,
}

/// Typed export statement
#[derive(Debug, Clone)]
pub struct TypedExportStmt<'a> {
    pub names: &'a [(Symbol, Option<Symbol>)], // (name, alias)
    pub module: Option<Symbol>,                // For re-exports
    pub span: TextRange,
}

/// Typed raise statement
#[derive(Debug, Clone)]
pub struct TypedRaiseStmt<'a> {
    pub exc: Option<TypedExpr<'a>>,
    pub cause: Option<TypedExpr<'a>>,
    pub span: TextRange,
}

/// Typed try statement
#[derive(Debug, Clone)]
pub struct TypedTryStmt<'a> {
    pub body: &'a [TypedStmt<'a>],
    pub handlers: &'a [TypedExceptHandler<'a>],
    pub orelse: &'a [TypedStmt<'a>],
    pub finalbody: &'a [TypedStmt<'a>],
    pub span: TextRange,
}

/// Typed exception handler
#[derive(Debug, Clone)]
pub struct TypedExceptHandler<'a> {
    pub typ: Option<TypedExpr<'a>>,
    pub name: Option<Symbol>,
    pub body: &'a [TypedStmt<'a>],
    pub is_exception_group: bool,
    pub span: TextRange,
}

/// Typed with statement with explicit protocol
#[derive(Debug, Clone)]
pub struct TypedWithStmt<'a> {
    pub items: &'a [TypedWithItem<'a>],
    pub body: &'a [TypedStmt<'a>],
    pub is_async: bool,
    pub span: TextRange,
}

/// Typed with item
#[derive(Debug, Clone)]
pub struct TypedWithItem<'a> {
    pub context_expr: TypedExpr<'a>,
    pub enter_call: TypedExpr<'a>, // Explicit __enter__() call
    pub exit_call: TypedExpr<'a>,  // Explicit __exit__() call
    pub optional_vars: Option<TypedExpr<'a>>,
}

/// Typed assert statement
#[derive(Debug, Clone)]
pub struct TypedAssertStmt<'a> {
    pub test: TypedExpr<'a>,
    pub msg: Option<TypedExpr<'a>>,
    pub span: TextRange,
}

/// Typed delete statement
#[derive(Debug, Clone)]
pub struct TypedDeleteStmt<'a> {
    pub targets: &'a [TypedExpr<'a>],
    pub span: TextRange,
}

/// Typed global statement
#[derive(Debug, Clone)]
pub struct TypedGlobalStmt<'a> {
    pub names: &'a [Symbol],
    pub span: TextRange,
}

/// Typed nonlocal statement
#[derive(Debug, Clone)]
pub struct TypedNonlocalStmt<'a> {
    pub names: &'a [Symbol],
    pub span: TextRange,
}

/// Typed match statement
#[derive(Debug, Clone)]
pub struct TypedMatchStmt<'a> {
    pub subject: TypedExpr<'a>,
    pub cases: &'a [TypedMatchCase<'a>],
    pub span: TextRange,
}

/// Typed match case
#[derive(Debug, Clone)]
pub struct TypedMatchCase<'a> {
    pub pattern: TypedPattern<'a>,
    pub guard: Option<TypedExpr<'a>>,
    pub body: &'a [TypedStmt<'a>],
    pub span: TextRange,
}

/// Typed yield statement
#[derive(Debug, Clone)]
pub struct TypedYieldStmt<'a> {
    pub value: Option<&'a TypedExpr<'a>>,
    pub span: TextRange,
}

/// Typed type alias statement
#[derive(Debug, Clone)]
pub struct TypedTypeAliasStmt<'a> {
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
