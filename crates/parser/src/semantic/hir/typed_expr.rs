//! Typed expression nodes for HIR

use crate::arena::symbol::Symbol;
use crate::semantic::types::Type;
use text_size::TextRange;

/// Typed expression nodes with resolved names and type information
#[derive(Debug, Clone)]
pub enum TypedExpr<'a> {
    /// Literal constant with type annotation
    Constant(TypedConstantExpr<'a>),
    /// Complex number literal
    Complex(TypedComplexExpr<'a>),
    /// Bytes literal
    Bytes(TypedBytesExpr<'a>),
    /// Name reference with resolved symbol
    Name(TypedNameExpr),
    /// Binary operation with typed operands
    BinOp(TypedBinOpExpr<'a>),
    /// Unary operation with typed operand
    UnaryOp(TypedUnaryOpExpr<'a>),
    /// Comparison with typed operands
    Compare(TypedCompareExpr<'a>),
    /// Function call with typed arguments
    Call(TypedCallExpr<'a>),
    /// Method call (desugared from attribute + call)
    MethodCall(TypedMethodCallExpr<'a>),
    /// Attribute access with typed object
    Attribute(TypedAttributeExpr<'a>),
    /// Subscript access with typed object and index
    Subscript(TypedSubscriptExpr<'a>),
    /// Slice operation with typed bounds
    Slice(TypedSliceExpr<'a>),
    /// List literal with typed elements
    List(TypedListExpr<'a>),
    /// Tuple literal with typed elements
    Tuple(TypedTupleExpr<'a>),
    /// Set literal with typed elements
    Set(TypedSetExpr<'a>),
    /// Dictionary literal with typed keys and values
    Dict(TypedDictExpr<'a>),
    /// Lambda expression with typed parameters and body
    Lambda(TypedLambdaExpr<'a>),
    /// Conditional expression with typed branches
    IfExp(TypedIfExpExpr<'a>),
    /// Boolean operation with typed operands
    BoolOp(TypedBoolOpExpr<'a>),
    /// Await expression (only in async context)
    Await(TypedAwaitExpr<'a>),
    /// Named expression (walrus operator)
    NamedExpr(TypedNamedExpr<'a>),
    /// Joined string (f-string) with typed parts
    JoinedStr(TypedJoinedStrExpr<'a>),
    /// Formatted value in f-string
    FormattedValue(TypedFormattedValueExpr<'a>),
    /// Template string literal
    TString(TypedTStringExpr<'a>),
    /// Starred expression for unpacking
    Starred(TypedStarredExpr<'a>),
    /// Yield expression
    Yield(TypedYieldExpr<'a>),
    /// Yield from expression
    YieldFrom(TypedYieldFromExpr<'a>),
    /// Module introspection expression
    ModuleIntrospection(TypedModuleIntrospectionExpr<'a>),
}

impl<'a> TypedExpr<'a> {
    /// Get the type of this expression
    pub fn ty(&self) -> &Type {
        match self {
            TypedExpr::Constant(e) => &e.ty,
            TypedExpr::Complex(e) => &e.ty,
            TypedExpr::Bytes(e) => &e.ty,
            TypedExpr::Name(e) => &e.ty,
            TypedExpr::BinOp(e) => &e.ty,
            TypedExpr::UnaryOp(e) => &e.ty,
            TypedExpr::Compare(e) => &e.ty,
            TypedExpr::Call(e) => &e.ty,
            TypedExpr::MethodCall(e) => &e.ty,
            TypedExpr::Attribute(e) => &e.ty,
            TypedExpr::Subscript(e) => &e.ty,
            TypedExpr::Slice(e) => &e.ty,
            TypedExpr::List(e) => &e.ty,
            TypedExpr::Tuple(e) => &e.ty,
            TypedExpr::Set(e) => &e.ty,
            TypedExpr::Dict(e) => &e.ty,
            TypedExpr::Lambda(e) => &e.ty,
            TypedExpr::IfExp(e) => &e.ty,
            TypedExpr::BoolOp(e) => &e.ty,
            TypedExpr::Await(e) => &e.ty,
            TypedExpr::NamedExpr(e) => &e.ty,
            TypedExpr::JoinedStr(e) => &e.ty,
            TypedExpr::FormattedValue(e) => &e.ty,
            TypedExpr::TString(e) => &e.ty,
            TypedExpr::Starred(e) => &e.ty,
            TypedExpr::Yield(e) => &e.ty,
            TypedExpr::YieldFrom(e) => &e.ty,
            TypedExpr::ModuleIntrospection(e) => &e.ty,
        }
    }

    /// Get the source span of this expression
    pub fn span(&self) -> TextRange {
        match self {
            TypedExpr::Constant(e) => e.span,
            TypedExpr::Complex(e) => e.span,
            TypedExpr::Bytes(e) => e.span,
            TypedExpr::Name(e) => e.span,
            TypedExpr::BinOp(e) => e.span,
            TypedExpr::UnaryOp(e) => e.span,
            TypedExpr::Compare(e) => e.span,
            TypedExpr::Call(e) => e.span,
            TypedExpr::MethodCall(e) => e.span,
            TypedExpr::Attribute(e) => e.span,
            TypedExpr::Subscript(e) => e.span,
            TypedExpr::Slice(e) => e.span,
            TypedExpr::List(e) => e.span,
            TypedExpr::Tuple(e) => e.span,
            TypedExpr::Set(e) => e.span,
            TypedExpr::Dict(e) => e.span,
            TypedExpr::Lambda(e) => e.span,
            TypedExpr::IfExp(e) => e.span,
            TypedExpr::BoolOp(e) => e.span,
            TypedExpr::Await(e) => e.span,
            TypedExpr::NamedExpr(e) => e.span,
            TypedExpr::JoinedStr(e) => e.span,
            TypedExpr::FormattedValue(e) => e.span,
            TypedExpr::TString(e) => e.span,
            TypedExpr::Starred(e) => e.span,
            TypedExpr::Yield(e) => e.span,
            TypedExpr::YieldFrom(e) => e.span,
            TypedExpr::ModuleIntrospection(e) => e.span,
        }
    }
}

/// Typed constant expression
#[derive(Debug, Clone)]
pub struct TypedConstantExpr<'a> {
    pub value: &'a str,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed complex number expression
#[derive(Debug, Clone)]
pub struct TypedComplexExpr<'a> {
    pub value: &'a str,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed bytes expression
#[derive(Debug, Clone)]
pub struct TypedBytesExpr<'a> {
    pub value: &'a str,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed name expression with resolved symbol
#[derive(Debug, Clone)]
pub struct TypedNameExpr {
    pub symbol: Symbol,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed binary operation
#[derive(Debug, Clone)]
pub struct TypedBinOpExpr<'a> {
    pub left: &'a TypedExpr<'a>,
    pub op: &'a str,
    pub right: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed unary operation
#[derive(Debug, Clone)]
pub struct TypedUnaryOpExpr<'a> {
    pub op: &'a str,
    pub operand: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed comparison expression
#[derive(Debug, Clone)]
pub struct TypedCompareExpr<'a> {
    pub left: &'a TypedExpr<'a>,
    pub ops: &'a [&'a str],
    pub comparators: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed function call
#[derive(Debug, Clone)]
pub struct TypedCallExpr<'a> {
    pub func: &'a TypedExpr<'a>,
    pub args: &'a [TypedExpr<'a>],
    pub keywords: &'a [TypedKeyword<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed method call (desugared from attribute + call)
#[derive(Debug, Clone)]
pub struct TypedMethodCallExpr<'a> {
    pub object: &'a TypedExpr<'a>,
    pub method: Symbol,
    pub args: &'a [TypedExpr<'a>],
    pub keywords: &'a [TypedKeyword<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed keyword argument
#[derive(Debug, Clone)]
pub struct TypedKeyword<'a> {
    pub arg: Option<&'a str>,
    pub value: &'a TypedExpr<'a>,
}

/// Typed attribute access
#[derive(Debug, Clone)]
pub struct TypedAttributeExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub attr: Symbol,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed subscript access
#[derive(Debug, Clone)]
pub struct TypedSubscriptExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub slice: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed slice expression
#[derive(Debug, Clone)]
pub struct TypedSliceExpr<'a> {
    pub lower: Option<&'a TypedExpr<'a>>,
    pub upper: Option<&'a TypedExpr<'a>>,
    pub step: Option<&'a TypedExpr<'a>>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed list expression
#[derive(Debug, Clone)]
pub struct TypedListExpr<'a> {
    pub elts: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed tuple expression
#[derive(Debug, Clone)]
pub struct TypedTupleExpr<'a> {
    pub elts: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed set expression
#[derive(Debug, Clone)]
pub struct TypedSetExpr<'a> {
    pub elts: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed dictionary expression
#[derive(Debug, Clone)]
pub struct TypedDictExpr<'a> {
    pub keys: &'a [Option<TypedExpr<'a>>],
    pub values: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed lambda expression
#[derive(Debug, Clone)]
pub struct TypedLambdaExpr<'a> {
    pub args: TypedArguments<'a>,
    pub body: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed conditional expression
#[derive(Debug, Clone)]
pub struct TypedIfExpExpr<'a> {
    pub test: &'a TypedExpr<'a>,
    pub body: &'a TypedExpr<'a>,
    pub orelse: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed boolean operation
#[derive(Debug, Clone)]
pub struct TypedBoolOpExpr<'a> {
    pub op: &'a str,
    pub values: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed await expression
#[derive(Debug, Clone)]
pub struct TypedAwaitExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed named expression (walrus operator)
#[derive(Debug, Clone)]
pub struct TypedNamedExpr<'a> {
    pub target: &'a TypedExpr<'a>,
    pub value: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed joined string (f-string)
#[derive(Debug, Clone)]
pub struct TypedJoinedStrExpr<'a> {
    pub values: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed formatted value in f-string
#[derive(Debug, Clone)]
pub struct TypedFormattedValueExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub conversion: Option<char>,
    pub format_spec: Option<&'a TypedJoinedStrExpr<'a>>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed template string
#[derive(Debug, Clone)]
pub struct TypedTStringExpr<'a> {
    pub values: &'a [TypedExpr<'a>],
    pub ty: Type,
    pub span: TextRange,
}

/// Typed starred expression
#[derive(Debug, Clone)]
pub struct TypedStarredExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed yield expression
#[derive(Debug, Clone)]
pub struct TypedYieldExpr<'a> {
    pub value: Option<&'a TypedExpr<'a>>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed yield from expression
#[derive(Debug, Clone)]
pub struct TypedYieldFromExpr<'a> {
    pub value: &'a TypedExpr<'a>,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed module introspection expression
#[derive(Debug, Clone)]
pub struct TypedModuleIntrospectionExpr<'a> {
    pub function: &'a str,
    pub ty: Type,
    pub span: TextRange,
}

/// Typed function arguments
#[derive(Debug, Clone)]
pub struct TypedArguments<'a> {
    pub posonlyargs: &'a [TypedArg<'a>],
    pub args: &'a [TypedArg<'a>],
    pub vararg: Option<&'a TypedArg<'a>>,
    pub kwonlyargs: &'a [TypedArg<'a>],
    pub kw_defaults: &'a [Option<TypedExpr<'a>>],
    pub kwarg: Option<&'a TypedArg<'a>>,
    pub defaults: &'a [TypedExpr<'a>],
}

/// Typed function argument
#[derive(Debug, Clone)]
pub struct TypedArg<'a> {
    pub symbol: Symbol,
    pub annotation: Option<&'a TypedExpr<'a>>,
    pub ty: Type,
}
