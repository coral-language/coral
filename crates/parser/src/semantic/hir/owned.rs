//! Owned HIR types without lifetime dependencies
//! These are used for passing HIR data across crate boundaries

use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// Owned typed module without lifetime dependencies
#[derive(Debug, Clone)]
pub struct OwnedTypedModule {
    pub name: Symbol,
    pub body: Vec<OwnedTypedStmt>,
    pub imports: Vec<OwnedTypedImport>,
    pub exports: Vec<OwnedTypedExport>,
    pub span: TextRange,
    pub docstring: Option<String>,
}

/// Owned typed statement without lifetime dependencies
#[derive(Debug, Clone)]
pub enum OwnedTypedStmt {
    Expr(OwnedExprStmt),
    Assign(OwnedAssignStmt),
    AnnAssign(OwnedAnnAssignStmt),
    AugAssign(OwnedAugAssignStmt),
    Return(OwnedReturnStmt),
    If(OwnedIfStmt),
    While(OwnedWhileStmt),
    For(OwnedForStmt),
    FuncDef(OwnedFuncDefStmt),
    ClassDef(OwnedClassDefStmt),
    Pass(TextRange),
    Break(TextRange),
    Continue(TextRange),
    Import(OwnedImportStmt),
    From(OwnedFromStmt),
    Export(OwnedExportStmt),
    Raise(OwnedRaiseStmt),
    Try(OwnedTryStmt),
    With(OwnedWithStmt),
    Assert(OwnedAssertStmt),
    Delete(OwnedDeleteStmt),
    Global(OwnedGlobalStmt),
    Nonlocal(OwnedNonlocalStmt),
    Match(OwnedMatchStmt),
    Yield(OwnedYieldStmt),
    TypeAlias(OwnedTypeAliasStmt),
}

impl OwnedTypedStmt {
    pub fn span(&self) -> TextRange {
        match self {
            OwnedTypedStmt::Expr(s) => s.span,
            OwnedTypedStmt::Assign(s) => s.span,
            OwnedTypedStmt::AnnAssign(s) => s.span,
            OwnedTypedStmt::AugAssign(s) => s.span,
            OwnedTypedStmt::Return(s) => s.span,
            OwnedTypedStmt::If(s) => s.span,
            OwnedTypedStmt::While(s) => s.span,
            OwnedTypedStmt::For(s) => s.span,
            OwnedTypedStmt::FuncDef(s) => s.span,
            OwnedTypedStmt::ClassDef(s) => s.span,
            OwnedTypedStmt::Pass(s) | OwnedTypedStmt::Break(s) | OwnedTypedStmt::Continue(s) => *s,
            OwnedTypedStmt::Import(s) => s.span,
            OwnedTypedStmt::From(s) => s.span,
            OwnedTypedStmt::Export(s) => s.span,
            OwnedTypedStmt::Raise(s) => s.span,
            OwnedTypedStmt::Try(s) => s.span,
            OwnedTypedStmt::With(s) => s.span,
            OwnedTypedStmt::Assert(s) => s.span,
            OwnedTypedStmt::Delete(s) => s.span,
            OwnedTypedStmt::Global(s) => s.span,
            OwnedTypedStmt::Nonlocal(s) => s.span,
            OwnedTypedStmt::Match(s) => s.span,
            OwnedTypedStmt::Yield(s) => s.span,
            OwnedTypedStmt::TypeAlias(s) => s.span,
        }
    }
}

/// Owned typed expression without lifetime dependencies
#[derive(Debug, Clone)]
pub enum OwnedTypedExpr {
    Constant(OwnedConstantExpr),
    Name(OwnedNameExpr),
    BinOp(OwnedBinOpExpr),
    UnaryOp(OwnedUnaryOpExpr),
    Compare(OwnedCompareExpr),
    Call(OwnedCallExpr),
    Attribute(OwnedAttributeExpr),
    Subscript(OwnedSubscriptExpr),
    Slice(OwnedSliceExpr),
    List(OwnedListExpr),
    Tuple(OwnedTupleExpr),
    Set(OwnedSetExpr),
    Dict(OwnedDictExpr),
    Lambda(OwnedLambdaExpr),
    IfExp(OwnedIfExpExpr),
    BoolOp(OwnedBoolOpExpr),
    Await(OwnedAwaitExpr),
    NamedExpr(OwnedNamedExpr),
    JoinedStr(OwnedJoinedStrExpr),
    FormattedValue(OwnedFormattedValueExpr),
    TString(OwnedTStringExpr),
    Starred(OwnedStarredExpr),
    Yield(OwnedYieldExpr),
    YieldFrom(OwnedYieldFromExpr),
}

impl OwnedTypedExpr {
    pub fn ty(&self) -> &Type {
        match self {
            OwnedTypedExpr::Constant(e) => &e.ty,
            OwnedTypedExpr::Name(e) => &e.ty,
            OwnedTypedExpr::BinOp(e) => &e.ty,
            OwnedTypedExpr::UnaryOp(e) => &e.ty,
            OwnedTypedExpr::Compare(e) => &e.ty,
            OwnedTypedExpr::Call(e) => &e.ty,
            OwnedTypedExpr::Attribute(e) => &e.ty,
            OwnedTypedExpr::Subscript(e) => &e.ty,
            OwnedTypedExpr::Slice(e) => &e.ty,
            OwnedTypedExpr::List(e) => &e.ty,
            OwnedTypedExpr::Tuple(e) => &e.ty,
            OwnedTypedExpr::Set(e) => &e.ty,
            OwnedTypedExpr::Dict(e) => &e.ty,
            OwnedTypedExpr::Lambda(e) => &e.ty,
            OwnedTypedExpr::IfExp(e) => &e.ty,
            OwnedTypedExpr::BoolOp(e) => &e.ty,
            OwnedTypedExpr::Await(e) => &e.ty,
            OwnedTypedExpr::NamedExpr(e) => &e.ty,
            OwnedTypedExpr::JoinedStr(e) => &e.ty,
            OwnedTypedExpr::FormattedValue(e) => &e.ty,
            OwnedTypedExpr::TString(e) => &e.ty,
            OwnedTypedExpr::Starred(e) => &e.ty,
            OwnedTypedExpr::Yield(e) => &e.ty,
            OwnedTypedExpr::YieldFrom(e) => &e.ty,
        }
    }

    pub fn span(&self) -> TextRange {
        match self {
            OwnedTypedExpr::Constant(e) => e.span,
            OwnedTypedExpr::Name(e) => e.span,
            OwnedTypedExpr::BinOp(e) => e.span,
            OwnedTypedExpr::UnaryOp(e) => e.span,
            OwnedTypedExpr::Compare(e) => e.span,
            OwnedTypedExpr::Call(e) => e.span,
            OwnedTypedExpr::Attribute(e) => e.span,
            OwnedTypedExpr::Subscript(e) => e.span,
            OwnedTypedExpr::Slice(e) => e.span,
            OwnedTypedExpr::List(e) => e.span,
            OwnedTypedExpr::Tuple(e) => e.span,
            OwnedTypedExpr::Set(e) => e.span,
            OwnedTypedExpr::Dict(e) => e.span,
            OwnedTypedExpr::Lambda(e) => e.span,
            OwnedTypedExpr::IfExp(e) => e.span,
            OwnedTypedExpr::BoolOp(e) => e.span,
            OwnedTypedExpr::Await(e) => e.span,
            OwnedTypedExpr::NamedExpr(e) => e.span,
            OwnedTypedExpr::JoinedStr(e) => e.span,
            OwnedTypedExpr::FormattedValue(e) => e.span,
            OwnedTypedExpr::TString(e) => e.span,
            OwnedTypedExpr::Starred(e) => e.span,
            OwnedTypedExpr::Yield(e) => e.span,
            OwnedTypedExpr::YieldFrom(e) => e.span,
        }
    }
}

// Simplified statement types (bare minimum for compilation)
#[derive(Debug, Clone)]
pub struct OwnedExprStmt {
    pub value: OwnedTypedExpr,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedAssignStmt {
    pub targets: Vec<OwnedTypedExpr>,
    pub value: OwnedTypedExpr,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedAnnAssignStmt {
    pub target: OwnedTypedExpr,
    pub annotation: OwnedTypedExpr,
    pub value: Option<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedAugAssignStmt {
    pub target: OwnedTypedExpr,
    pub op: String,
    pub value: OwnedTypedExpr,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedReturnStmt {
    pub value: Option<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedIfStmt {
    pub test: OwnedTypedExpr,
    pub body: Vec<OwnedTypedStmt>,
    pub orelse: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedWhileStmt {
    pub test: OwnedTypedExpr,
    pub body: Vec<OwnedTypedStmt>,
    pub orelse: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedForStmt {
    pub target: OwnedTypedExpr,
    pub iter: OwnedTypedExpr,
    pub body: Vec<OwnedTypedStmt>,
    pub orelse: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedFuncDefStmt {
    pub name: Symbol,
    pub args: Vec<OwnedArg>,
    pub body: Vec<OwnedTypedStmt>,
    pub returns: Option<OwnedTypedExpr>,
    pub decorators: Vec<OwnedTypedExpr>,
    pub is_async: bool,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedClassDefStmt {
    pub name: Symbol,
    pub bases: Vec<OwnedTypedExpr>,
    pub body: Vec<OwnedTypedStmt>,
    pub decorators: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedImportStmt {
    pub names: Vec<(String, Option<String>)>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedFromStmt {
    pub module: Option<String>,
    pub names: Vec<(String, Option<String>)>,
    pub level: u32,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedExportStmt {
    pub names: Vec<(String, Option<String>)>,
    pub module: Option<String>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedRaiseStmt {
    pub exc: Option<OwnedTypedExpr>,
    pub cause: Option<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedTryStmt {
    pub body: Vec<OwnedTypedStmt>,
    pub handlers: Vec<OwnedExceptHandler>,
    pub orelse: Vec<OwnedTypedStmt>,
    pub finalbody: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedExceptHandler {
    pub typ: Option<OwnedTypedExpr>,
    pub name: Option<Symbol>,
    pub body: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedWithStmt {
    pub items: Vec<OwnedWithItem>,
    pub body: Vec<OwnedTypedStmt>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedWithItem {
    pub context_expr: OwnedTypedExpr,
    pub optional_vars: Option<OwnedTypedExpr>,
}

#[derive(Debug, Clone)]
pub struct OwnedAssertStmt {
    pub test: OwnedTypedExpr,
    pub msg: Option<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedDeleteStmt {
    pub targets: Vec<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedGlobalStmt {
    pub names: Vec<String>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedNonlocalStmt {
    pub names: Vec<String>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedMatchStmt {
    pub subject: OwnedTypedExpr,
    pub cases: Vec<OwnedMatchCase>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedMatchCase {
    pub pattern: String, // Simplified
    pub guard: Option<OwnedTypedExpr>,
    pub body: Vec<OwnedTypedStmt>,
}

#[derive(Debug, Clone)]
pub struct OwnedYieldStmt {
    pub value: Option<OwnedTypedExpr>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedTypeAliasStmt {
    pub name: Symbol,
    pub value: OwnedTypedExpr,
    pub ty: Type,
    pub span: TextRange,
}

// Expression types
#[derive(Debug, Clone)]
pub struct OwnedConstantExpr {
    pub value: String,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedNameExpr {
    pub id: String,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedBinOpExpr {
    pub left: Box<OwnedTypedExpr>,
    pub op: String,
    pub right: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedUnaryOpExpr {
    pub op: String,
    pub operand: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedCompareExpr {
    pub left: Box<OwnedTypedExpr>,
    pub ops: Vec<String>,
    pub comparators: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedCallExpr {
    pub func: Box<OwnedTypedExpr>,
    pub args: Vec<OwnedTypedExpr>,
    pub keywords: Vec<OwnedKeyword>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedKeyword {
    pub arg: Option<String>,
    pub value: OwnedTypedExpr,
}

#[derive(Debug, Clone)]
pub struct OwnedAttributeExpr {
    pub value: Box<OwnedTypedExpr>,
    pub attr: String,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedSubscriptExpr {
    pub value: Box<OwnedTypedExpr>,
    pub slice: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedSliceExpr {
    pub lower: Option<Box<OwnedTypedExpr>>,
    pub upper: Option<Box<OwnedTypedExpr>>,
    pub step: Option<Box<OwnedTypedExpr>>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedListExpr {
    pub elts: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedTupleExpr {
    pub elts: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedSetExpr {
    pub elts: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedDictExpr {
    pub keys: Vec<Option<OwnedTypedExpr>>,
    pub values: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedLambdaExpr {
    pub args: Vec<OwnedArg>,
    pub body: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedIfExpExpr {
    pub test: Box<OwnedTypedExpr>,
    pub body: Box<OwnedTypedExpr>,
    pub orelse: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedBoolOpExpr {
    pub op: String,
    pub values: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedAwaitExpr {
    pub value: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedNamedExpr {
    pub target: Box<OwnedTypedExpr>,
    pub value: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedJoinedStrExpr {
    pub values: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedFormattedValueExpr {
    pub value: Box<OwnedTypedExpr>,
    pub conversion: Option<char>,
    pub format_spec: Option<Vec<OwnedTypedExpr>>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedTStringExpr {
    pub values: Vec<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedStarredExpr {
    pub value: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedYieldExpr {
    pub value: Option<Box<OwnedTypedExpr>>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedYieldFromExpr {
    pub value: Box<OwnedTypedExpr>,
    pub ty: Type,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedArg {
    pub arg: String,
    pub annotation: Option<OwnedTypedExpr>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct OwnedTypedImport {
    pub module: Option<String>,
    pub names: Vec<(String, Option<String>)>,
    pub level: u32,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct OwnedTypedExport {
    pub names: Vec<(String, Option<String>)>,
    pub module: Option<String>,
    pub span: TextRange,
}

/// Convert lifetime-dependent HIR to owned types for export to codegen
pub fn convert_to_owned(
    hir_module: &super::typed_item::TypedModule,
    interner: &crate::arena::interner::Interner,
) -> OwnedTypedModule {
    OwnedTypedModule {
        name: hir_module.name,
        body: hir_module
            .body
            .iter()
            .map(|s| convert_stmt(s, interner))
            .collect(),
        imports: hir_module
            .imports
            .iter()
            .map(|i| convert_import(i, interner))
            .collect(),
        exports: hir_module
            .exports
            .iter()
            .map(|e| convert_export(e, interner))
            .collect(),
        span: hir_module.span,
        docstring: hir_module.docstring.map(String::from),
    }
}

fn convert_stmt(
    stmt: &super::typed_stmt::TypedStmt,
    interner: &crate::arena::interner::Interner,
) -> OwnedTypedStmt {
    match stmt {
        super::typed_stmt::TypedStmt::Expr(e) => OwnedTypedStmt::Expr(OwnedExprStmt {
            value: convert_expr(&e.value, interner),
            span: e.span,
        }),
        super::typed_stmt::TypedStmt::Assign(a) => OwnedTypedStmt::Assign(OwnedAssignStmt {
            targets: a
                .targets
                .iter()
                .map(|t| convert_expr(t, interner))
                .collect(),
            value: convert_expr(&a.value, interner),
            span: a.span,
        }),
        super::typed_stmt::TypedStmt::AnnAssign(a) => {
            OwnedTypedStmt::AnnAssign(OwnedAnnAssignStmt {
                target: convert_expr(&a.target, interner),
                annotation: convert_expr(&a.annotation, interner),
                value: a.value.as_ref().map(|v| convert_expr(v, interner)),
                span: a.span,
            })
        }
        super::typed_stmt::TypedStmt::AugAssign(a) => {
            OwnedTypedStmt::AugAssign(OwnedAugAssignStmt {
                target: convert_expr(&a.target, interner),
                op: format!("{:?}", a.op),
                value: convert_expr(&a.value, interner),
                span: a.span,
            })
        }
        super::typed_stmt::TypedStmt::Return(r) => OwnedTypedStmt::Return(OwnedReturnStmt {
            value: r.value.as_ref().map(|v| convert_expr(v, interner)),
            span: r.span,
        }),
        super::typed_stmt::TypedStmt::If(i) => OwnedTypedStmt::If(OwnedIfStmt {
            test: convert_expr(&i.test, interner),
            body: i.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            orelse: i.orelse.iter().map(|s| convert_stmt(s, interner)).collect(),
            span: i.span,
        }),
        super::typed_stmt::TypedStmt::While(w) => OwnedTypedStmt::While(OwnedWhileStmt {
            test: convert_expr(&w.test, interner),
            body: w.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            orelse: w.orelse.iter().map(|s| convert_stmt(s, interner)).collect(),
            span: w.span,
        }),
        super::typed_stmt::TypedStmt::For(f) => OwnedTypedStmt::For(OwnedForStmt {
            target: convert_expr(&f.target, interner),
            iter: convert_expr(&f.iter, interner),
            body: f.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            orelse: f.orelse.iter().map(|s| convert_stmt(s, interner)).collect(),
            span: f.span,
        }),
        super::typed_stmt::TypedStmt::FuncDef(f) => OwnedTypedStmt::FuncDef(OwnedFuncDefStmt {
            name: f.name,
            args: f
                .args
                .args
                .iter()
                .map(|arg| OwnedArg {
                    arg: interner
                        .resolve(arg.symbol)
                        .unwrap_or("<unknown>")
                        .to_string(),
                    annotation: arg.annotation.as_ref().map(|a| convert_expr(a, interner)),
                    ty: arg.ty.clone(),
                })
                .collect(),
            body: f.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            returns: f.returns.as_ref().map(|r| convert_expr(r, interner)),
            decorators: f
                .decorators
                .iter()
                .map(|d| convert_expr(d, interner))
                .collect(),
            is_async: f.is_async,
            ty: f.ty.clone(),
            span: f.span,
        }),
        super::typed_stmt::TypedStmt::ClassDef(c) => OwnedTypedStmt::ClassDef(OwnedClassDefStmt {
            name: c.name,
            bases: c.bases.iter().map(|b| convert_expr(b, interner)).collect(),
            body: c.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            decorators: c
                .decorators
                .iter()
                .map(|d| convert_expr(d, interner))
                .collect(),
            ty: c.ty.clone(),
            span: c.span,
        }),
        super::typed_stmt::TypedStmt::Pass(s) => OwnedTypedStmt::Pass(*s),
        super::typed_stmt::TypedStmt::Break(s) => OwnedTypedStmt::Break(*s),
        super::typed_stmt::TypedStmt::Continue(s) => OwnedTypedStmt::Continue(*s),
        super::typed_stmt::TypedStmt::Import(i) => OwnedTypedStmt::Import(OwnedImportStmt {
            names: i
                .names
                .iter()
                .map(|(name, alias)| {
                    (
                        interner.resolve(*name).unwrap_or("<unknown>").to_string(),
                        alias
                            .as_ref()
                            .map(|a| interner.resolve(*a).unwrap_or("<unknown>").to_string()),
                    )
                })
                .collect(),
            span: i.span,
        }),
        super::typed_stmt::TypedStmt::From(f) => OwnedTypedStmt::From(OwnedFromStmt {
            module: f
                .module
                .as_ref()
                .map(|m| interner.resolve(*m).unwrap_or("<unknown>").to_string()),
            names: f
                .names
                .iter()
                .map(|(name, alias)| {
                    (
                        interner.resolve(*name).unwrap_or("<unknown>").to_string(),
                        alias
                            .as_ref()
                            .map(|a| interner.resolve(*a).unwrap_or("<unknown>").to_string()),
                    )
                })
                .collect(),
            level: f.level,
            span: f.span,
        }),
        super::typed_stmt::TypedStmt::Export(e) => OwnedTypedStmt::Export(OwnedExportStmt {
            names: e
                .names
                .iter()
                .map(|(name, alias)| {
                    (
                        interner.resolve(*name).unwrap_or("<unknown>").to_string(),
                        alias
                            .as_ref()
                            .map(|a| interner.resolve(*a).unwrap_or("<unknown>").to_string()),
                    )
                })
                .collect(),
            module: e
                .module
                .as_ref()
                .map(|m| interner.resolve(*m).unwrap_or("<unknown>").to_string()),
            span: e.span,
        }),
        super::typed_stmt::TypedStmt::Raise(r) => OwnedTypedStmt::Raise(OwnedRaiseStmt {
            exc: r.exc.as_ref().map(|e| convert_expr(e, interner)),
            cause: r.cause.as_ref().map(|c| convert_expr(c, interner)),
            span: r.span,
        }),
        super::typed_stmt::TypedStmt::Try(t) => OwnedTypedStmt::Try(OwnedTryStmt {
            body: t.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            handlers: t
                .handlers
                .iter()
                .map(|h| OwnedExceptHandler {
                    typ: h.typ.as_ref().map(|ty| convert_expr(ty, interner)),
                    name: h.name,
                    body: h.body.iter().map(|s| convert_stmt(s, interner)).collect(),
                    span: h.span,
                })
                .collect(),
            orelse: t.orelse.iter().map(|s| convert_stmt(s, interner)).collect(),
            finalbody: t
                .finalbody
                .iter()
                .map(|s| convert_stmt(s, interner))
                .collect(),
            span: t.span,
        }),
        super::typed_stmt::TypedStmt::With(w) => OwnedTypedStmt::With(OwnedWithStmt {
            items: w
                .items
                .iter()
                .map(|item| OwnedWithItem {
                    context_expr: convert_expr(&item.context_expr, interner),
                    optional_vars: item
                        .optional_vars
                        .as_ref()
                        .map(|v| convert_expr(v, interner)),
                })
                .collect(),
            body: w.body.iter().map(|s| convert_stmt(s, interner)).collect(),
            span: w.span,
        }),
        super::typed_stmt::TypedStmt::Assert(a) => OwnedTypedStmt::Assert(OwnedAssertStmt {
            test: convert_expr(&a.test, interner),
            msg: a.msg.as_ref().map(|m| convert_expr(m, interner)),
            span: a.span,
        }),
        super::typed_stmt::TypedStmt::Delete(d) => OwnedTypedStmt::Delete(OwnedDeleteStmt {
            targets: d
                .targets
                .iter()
                .map(|t| convert_expr(t, interner))
                .collect(),
            span: d.span,
        }),
        super::typed_stmt::TypedStmt::Global(g) => OwnedTypedStmt::Global(OwnedGlobalStmt {
            names: g
                .names
                .iter()
                .map(|n| interner.resolve(*n).unwrap_or("<unknown>").to_string())
                .collect(),
            span: g.span,
        }),
        super::typed_stmt::TypedStmt::Nonlocal(n) => OwnedTypedStmt::Nonlocal(OwnedNonlocalStmt {
            names: n
                .names
                .iter()
                .map(|n| interner.resolve(*n).unwrap_or("<unknown>").to_string())
                .collect(),
            span: n.span,
        }),
        super::typed_stmt::TypedStmt::Match(m) => OwnedTypedStmt::Match(OwnedMatchStmt {
            subject: convert_expr(&m.subject, interner),
            cases: m
                .cases
                .iter()
                .map(|case| OwnedMatchCase {
                    pattern: format!("{:?}", case.pattern),
                    guard: case.guard.as_ref().map(|g| convert_expr(g, interner)),
                    body: case
                        .body
                        .iter()
                        .map(|s| convert_stmt(s, interner))
                        .collect(),
                })
                .collect(),
            span: m.span,
        }),
        super::typed_stmt::TypedStmt::Yield(y) => OwnedTypedStmt::Yield(OwnedYieldStmt {
            value: y.value.as_ref().map(|v| convert_expr(v, interner)),
            span: y.span,
        }),
        super::typed_stmt::TypedStmt::TypeAlias(t) => {
            OwnedTypedStmt::TypeAlias(OwnedTypeAliasStmt {
                name: t.name,
                value: convert_expr(&t.value, interner),
                ty: t.ty.clone(),
                span: t.span,
            })
        }
    }
}

fn convert_expr(
    expr: &super::typed_expr::TypedExpr,
    interner: &crate::arena::interner::Interner,
) -> OwnedTypedExpr {
    match expr {
        super::typed_expr::TypedExpr::Constant(e) => OwnedTypedExpr::Constant(OwnedConstantExpr {
            value: e.value.to_string(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Name(e) => OwnedTypedExpr::Name(OwnedNameExpr {
            id: interner
                .resolve(e.symbol)
                .unwrap_or("<unknown>")
                .to_string(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::BinOp(e) => OwnedTypedExpr::BinOp(OwnedBinOpExpr {
            left: Box::new(convert_expr(e.left, interner)),
            op: format!("{:?}", e.op),
            right: Box::new(convert_expr(e.right, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::UnaryOp(e) => OwnedTypedExpr::UnaryOp(OwnedUnaryOpExpr {
            op: format!("{:?}", e.op),
            operand: Box::new(convert_expr(e.operand, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Compare(e) => OwnedTypedExpr::Compare(OwnedCompareExpr {
            left: Box::new(convert_expr(e.left, interner)),
            ops: e.ops.iter().map(|op| format!("{:?}", op)).collect(),
            comparators: e
                .comparators
                .iter()
                .map(|c| convert_expr(c, interner))
                .collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Call(e) => OwnedTypedExpr::Call(OwnedCallExpr {
            func: Box::new(convert_expr(e.func, interner)),
            args: e.args.iter().map(|a| convert_expr(a, interner)).collect(),
            keywords: e
                .keywords
                .iter()
                .map(|kw| OwnedKeyword {
                    arg: kw.arg.map(|a| a.to_string()),
                    value: convert_expr(kw.value, interner),
                })
                .collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Attribute(e) => {
            OwnedTypedExpr::Attribute(OwnedAttributeExpr {
                value: Box::new(convert_expr(e.value, interner)),
                attr: interner.resolve(e.attr).unwrap_or("<unknown>").to_string(),
                ty: e.ty.clone(),
                span: e.span,
            })
        }
        super::typed_expr::TypedExpr::Subscript(e) => {
            OwnedTypedExpr::Subscript(OwnedSubscriptExpr {
                value: Box::new(convert_expr(e.value, interner)),
                slice: Box::new(convert_expr(e.slice, interner)),
                ty: e.ty.clone(),
                span: e.span,
            })
        }
        super::typed_expr::TypedExpr::Slice(e) => OwnedTypedExpr::Slice(OwnedSliceExpr {
            lower: e
                .lower
                .as_ref()
                .map(|l| Box::new(convert_expr(l, interner))),
            upper: e
                .upper
                .as_ref()
                .map(|u| Box::new(convert_expr(u, interner))),
            step: e.step.as_ref().map(|s| Box::new(convert_expr(s, interner))),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::List(e) => OwnedTypedExpr::List(OwnedListExpr {
            elts: e.elts.iter().map(|el| convert_expr(el, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Tuple(e) => OwnedTypedExpr::Tuple(OwnedTupleExpr {
            elts: e.elts.iter().map(|el| convert_expr(el, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Set(e) => OwnedTypedExpr::Set(OwnedSetExpr {
            elts: e.elts.iter().map(|el| convert_expr(el, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Dict(e) => OwnedTypedExpr::Dict(OwnedDictExpr {
            keys: e
                .keys
                .iter()
                .map(|k| k.as_ref().map(|key| convert_expr(key, interner)))
                .collect(),
            values: e.values.iter().map(|v| convert_expr(v, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Lambda(e) => OwnedTypedExpr::Lambda(OwnedLambdaExpr {
            args: e
                .args
                .args
                .iter()
                .map(|arg| OwnedArg {
                    arg: interner
                        .resolve(arg.symbol)
                        .unwrap_or("<unknown>")
                        .to_string(),
                    annotation: arg.annotation.as_ref().map(|a| convert_expr(a, interner)),
                    ty: arg.ty.clone(),
                })
                .collect(),
            body: Box::new(convert_expr(e.body, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::IfExp(e) => OwnedTypedExpr::IfExp(OwnedIfExpExpr {
            test: Box::new(convert_expr(e.test, interner)),
            body: Box::new(convert_expr(e.body, interner)),
            orelse: Box::new(convert_expr(e.orelse, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::BoolOp(e) => OwnedTypedExpr::BoolOp(OwnedBoolOpExpr {
            op: format!("{:?}", e.op),
            values: e.values.iter().map(|v| convert_expr(v, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Await(e) => OwnedTypedExpr::Await(OwnedAwaitExpr {
            value: Box::new(convert_expr(e.value, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::NamedExpr(e) => OwnedTypedExpr::NamedExpr(OwnedNamedExpr {
            target: Box::new(convert_expr(e.target, interner)),
            value: Box::new(convert_expr(e.value, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::JoinedStr(e) => {
            OwnedTypedExpr::JoinedStr(OwnedJoinedStrExpr {
                values: e.values.iter().map(|v| convert_expr(v, interner)).collect(),
                ty: e.ty.clone(),
                span: e.span,
            })
        }
        super::typed_expr::TypedExpr::FormattedValue(e) => {
            OwnedTypedExpr::FormattedValue(OwnedFormattedValueExpr {
                value: Box::new(convert_expr(e.value, interner)),
                conversion: e.conversion,
                format_spec: e.format_spec.as_ref().map(|fs| {
                    fs.values
                        .iter()
                        .map(|f| convert_expr(f, interner))
                        .collect()
                }),
                ty: e.ty.clone(),
                span: e.span,
            })
        }
        super::typed_expr::TypedExpr::TString(e) => OwnedTypedExpr::TString(OwnedTStringExpr {
            values: e.values.iter().map(|v| convert_expr(v, interner)).collect(),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Starred(e) => OwnedTypedExpr::Starred(OwnedStarredExpr {
            value: Box::new(convert_expr(e.value, interner)),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::Yield(e) => OwnedTypedExpr::Yield(OwnedYieldExpr {
            value: e
                .value
                .as_ref()
                .map(|v| Box::new(convert_expr(v, interner))),
            ty: e.ty.clone(),
            span: e.span,
        }),
        super::typed_expr::TypedExpr::YieldFrom(e) => {
            OwnedTypedExpr::YieldFrom(OwnedYieldFromExpr {
                value: Box::new(convert_expr(e.value, interner)),
                ty: e.ty.clone(),
                span: e.span,
            })
        }
        _ => OwnedTypedExpr::Constant(OwnedConstantExpr {
            value: "unknown".to_string(),
            ty: crate::semantic::types::Type::Unknown,
            span: text_size::TextRange::default(),
        }),
    }
}

fn convert_import(
    import: &super::typed_item::TypedImport,
    interner: &crate::arena::interner::Interner,
) -> OwnedTypedImport {
    OwnedTypedImport {
        module: import
            .module
            .map(|m| interner.resolve(m).unwrap_or("<unknown>").to_string()),
        names: import
            .names
            .iter()
            .map(|(name, alias)| {
                (
                    interner.resolve(*name).unwrap_or("<unknown>").to_string(),
                    alias
                        .as_ref()
                        .map(|a| interner.resolve(*a).unwrap_or("<unknown>").to_string()),
                )
            })
            .collect(),
        level: import.level,
        span: import.span,
    }
}

fn convert_export(
    export: &super::typed_item::TypedExport,
    interner: &crate::arena::interner::Interner,
) -> OwnedTypedExport {
    OwnedTypedExport {
        names: export
            .names
            .iter()
            .map(|(name, alias)| {
                (
                    interner.resolve(*name).unwrap_or("<unknown>").to_string(),
                    alias
                        .as_ref()
                        .map(|a| interner.resolve(*a).unwrap_or("<unknown>").to_string()),
                )
            })
            .collect(),
        module: export
            .module
            .map(|m| interner.resolve(m).unwrap_or("<unknown>").to_string()),
        span: export.span,
    }
}
