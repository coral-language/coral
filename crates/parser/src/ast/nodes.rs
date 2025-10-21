//! Core AST node definitions (Module and statements).

use super::expr::Expr;
use text_size::TextRange;

/// A module (root AST node).
#[derive(Debug, Clone)]
pub struct Module<'a> {
    pub body: &'a [Stmt<'a>],
    pub span: TextRange,
    /// The module's docstring (first string literal in the module, if any).
    pub docstring: Option<&'a str>,
}

/// Statement types.
#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expr(ExprStmt<'a>),
    Assign(AssignStmt<'a>),
    AnnAssign(AnnAssignStmt<'a>),
    AugAssign(AugAssignStmt<'a>),
    Return(ReturnStmt<'a>),
    If(IfStmt<'a>),
    While(WhileStmt<'a>),
    For(ForStmt<'a>),
    FuncDef(FuncDefStmt<'a>),
    ClassDef(ClassDefStmt<'a>),
    Pass(TextRange),
    Break(TextRange),
    Continue(TextRange),
    Import(ImportStmt<'a>),
    From(FromStmt<'a>),
    Export(ExportStmt<'a>),
    Raise(RaiseStmt<'a>),
    Try(TryStmt<'a>),
    With(WithStmt<'a>),
    Assert(AssertStmt<'a>),
    Delete(DeleteStmt<'a>),
    Global(GlobalStmt<'a>),
    Nonlocal(NonlocalStmt<'a>),
    Match(super::patterns::MatchStmt<'a>),
    Yield(YieldStmt<'a>),
    TypeAlias(TypeAliasStmt<'a>),
}

impl<'a> Stmt<'a> {
    pub fn span(&self) -> TextRange {
        match self {
            Stmt::Expr(s) => s.span,
            Stmt::Assign(s) => s.span,
            Stmt::AnnAssign(s) => s.span,
            Stmt::AugAssign(s) => s.span,
            Stmt::Return(s) => s.span,
            Stmt::If(s) => s.span,
            Stmt::While(s) => s.span,
            Stmt::For(s) => s.span,
            Stmt::FuncDef(s) => s.span,
            Stmt::ClassDef(s) => s.span,
            Stmt::Pass(s) | Stmt::Break(s) | Stmt::Continue(s) => *s,
            Stmt::Import(s) => s.span,
            Stmt::From(s) => s.span,
            Stmt::Export(s) => s.span,
            Stmt::Raise(s) => s.span,
            Stmt::Try(s) => s.span,
            Stmt::With(s) => s.span,
            Stmt::Assert(s) => s.span,
            Stmt::Delete(s) => s.span,
            Stmt::Global(s) => s.span,
            Stmt::Nonlocal(s) => s.span,
            Stmt::Match(s) => s.span,
            Stmt::Yield(s) => s.span,
            Stmt::TypeAlias(s) => s.span,
        }
    }
}

/// Statement types with spans.
#[derive(Debug, Clone)]
pub struct ExprStmt<'a> {
    pub value: Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct AssignStmt<'a> {
    pub targets: &'a [Expr<'a>],
    pub value: Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct AnnAssignStmt<'a> {
    pub target: Expr<'a>,
    pub annotation: Expr<'a>,
    pub value: Option<Expr<'a>>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct AugAssignStmt<'a> {
    pub target: Expr<'a>,
    pub op: &'a str,
    pub value: Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<'a> {
    pub value: Option<Expr<'a>>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct IfStmt<'a> {
    pub test: Expr<'a>,
    pub body: &'a [Stmt<'a>],
    pub orelse: &'a [Stmt<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct WhileStmt<'a> {
    pub test: Expr<'a>,
    pub body: &'a [Stmt<'a>],
    pub orelse: &'a [Stmt<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ForStmt<'a> {
    pub target: Expr<'a>,
    pub iter: Expr<'a>,
    pub body: &'a [Stmt<'a>],
    pub orelse: &'a [Stmt<'a>],
    pub is_async: bool,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct FuncDefStmt<'a> {
    pub name: &'a str,
    pub type_params: &'a [TypeParam<'a>],
    pub args: Arguments<'a>,
    pub body: &'a [Stmt<'a>],
    pub decorators: &'a [Expr<'a>],
    pub returns: Option<Box<Expr<'a>>>,
    pub is_async: bool,
    pub span: TextRange,
    /// The function's docstring (first string literal in the function body, if any).
    pub docstring: Option<&'a str>,
}

#[derive(Debug, Clone)]
pub struct ClassDefStmt<'a> {
    pub name: &'a str,
    pub type_params: &'a [TypeParam<'a>],
    pub bases: &'a [Expr<'a>],
    pub keywords: &'a [super::expr::Keyword<'a>],
    pub body: &'a [Stmt<'a>],
    pub decorators: &'a [Expr<'a>],
    pub span: TextRange,
    /// The class's docstring (first string literal in the class body, if any).
    pub docstring: Option<&'a str>,
}

#[derive(Debug, Clone)]
pub struct ImportStmt<'a> {
    pub names: &'a [(&'a str, Option<&'a str>)],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct FromStmt<'a> {
    pub level: u32, // Number of leading dots for relative imports (0 = absolute)
    pub module: Option<&'a str>,
    pub names: &'a [(&'a str, Option<&'a str>)],
    pub span: TextRange,
}

/// Export statement variants:
/// 1. `export name` - export a single name
/// 2. `export name, other` - export multiple names
/// 3. `export name as alias` - export with alias
/// 4. `export name, other from module` - re-export from another module
#[derive(Debug, Clone)]
pub struct ExportStmt<'a> {
    pub names: &'a [(&'a str, Option<&'a str>)], // (name, optional alias)
    pub module: Option<&'a str>,                 // For re-exports: `export name from module`
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct RaiseStmt<'a> {
    pub exc: Option<Expr<'a>>,
    pub cause: Option<Expr<'a>>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct TryStmt<'a> {
    pub body: &'a [Stmt<'a>],
    pub handlers: &'a [ExceptHandler<'a>],
    pub orelse: &'a [Stmt<'a>],
    pub finalbody: &'a [Stmt<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ExceptHandler<'a> {
    pub typ: Option<Expr<'a>>,
    pub name: Option<&'a str>,
    pub body: &'a [Stmt<'a>],
    pub is_exception_group: bool, // true for except*, false for except
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct WithStmt<'a> {
    pub items: &'a [WithItem<'a>],
    pub body: &'a [Stmt<'a>],
    pub is_async: bool,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct WithItem<'a> {
    pub context_expr: Expr<'a>,
    pub optional_vars: Option<Expr<'a>>,
}

#[derive(Debug, Clone)]
pub struct AssertStmt<'a> {
    pub test: Expr<'a>,
    pub msg: Option<Expr<'a>>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct DeleteStmt<'a> {
    pub targets: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct GlobalStmt<'a> {
    pub names: &'a [&'a str],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct NonlocalStmt<'a> {
    pub names: &'a [&'a str],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct YieldStmt<'a> {
    pub value: Option<&'a super::expr::Expr<'a>>,
    pub span: TextRange,
}

/// Type alias statement: type MyType = int
#[derive(Debug, Clone)]
pub struct TypeAliasStmt<'a> {
    pub name: &'a str,
    pub type_params: &'a [TypeParam<'a>],
    pub value: Expr<'a>,
    pub span: TextRange,
}

/// Function arguments.
#[derive(Debug, Clone)]
pub struct Arguments<'a> {
    pub posonlyargs: &'a [Arg<'a>],
    pub args: &'a [Arg<'a>],
    pub vararg: Option<Box<Arg<'a>>>,
    pub kwonlyargs: &'a [Arg<'a>],
    pub kw_defaults: &'a [Option<Expr<'a>>],
    pub kwarg: Option<Box<Arg<'a>>>,
    pub defaults: &'a [Expr<'a>],
}

#[derive(Debug, Clone)]
pub struct Arg<'a> {
    pub arg: &'a str,
    pub annotation: Option<Box<Expr<'a>>>,
}

/// Type parameter for generic functions and classes
#[derive(Debug, Clone)]
pub struct TypeParam<'a> {
    pub name: &'a str,
    /// Constraint bound: T: (int, str) becomes Some(Tuple([int, str]))
    pub bound: Option<Expr<'a>>,
    /// Default value: T = int becomes Some(int)
    pub default: Option<Expr<'a>>,
    pub span: TextRange,
}

/// Comprehension clause (for x in iter [if condition])
#[derive(Debug, Clone)]
pub struct Comprehension<'a> {
    pub target: Expr<'a>,
    pub iter: Expr<'a>,
    pub ifs: &'a [Expr<'a>],
    pub is_async: bool,
}
