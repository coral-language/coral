//! Expression AST nodes.

use super::nodes::{Arguments, Comprehension};
use text_size::TextRange;

/// Expression types.
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Constant(ConstantExpr<'a>),
    Complex(ComplexExpr<'a>),
    Bytes(BytesExpr<'a>),
    Name(NameExpr<'a>),
    BinOp(BinOpExpr<'a>),
    UnaryOp(UnaryOpExpr<'a>),
    Compare(CompareExpr<'a>),
    Call(CallExpr<'a>),
    Attribute(AttributeExpr<'a>),
    Subscript(SubscriptExpr<'a>),
    Slice(SliceExpr<'a>),
    List(ListExpr<'a>),
    Tuple(TupleExpr<'a>),
    Set(SetExpr<'a>),
    Dict(DictExpr<'a>),
    Lambda(LambdaExpr<'a>),
    IfExp(IfExpExpr<'a>),
    BoolOp(BoolOpExpr<'a>),
    ListComp(ListCompExpr<'a>),
    DictComp(DictCompExpr<'a>),
    SetComp(SetCompExpr<'a>),
    GeneratorExp(GeneratorExpExpr<'a>),
    Await(AwaitExpr<'a>),
    NamedExpr(NamedExprNode<'a>),
    JoinedStr(JoinedStrExpr<'a>),
    FormattedValue(FormattedValueExpr<'a>),
    TString(TStringExpr<'a>),
    Starred(StarredExpr<'a>),
    Yield(YieldExpr<'a>),
    YieldFrom(YieldFromExpr<'a>),
    ModuleIntrospection(ModuleIntrospectionExpr<'a>),
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> TextRange {
        match self {
            Expr::Constant(e) => e.span,
            Expr::Complex(e) => e.span,
            Expr::Bytes(e) => e.span,
            Expr::Name(e) => e.span,
            Expr::BinOp(e) => e.span,
            Expr::UnaryOp(e) => e.span,
            Expr::Compare(e) => e.span,
            Expr::Call(e) => e.span,
            Expr::Attribute(e) => e.span,
            Expr::Subscript(e) => e.span,
            Expr::Slice(e) => e.span,
            Expr::List(e) => e.span,
            Expr::Tuple(e) => e.span,
            Expr::Set(e) => e.span,
            Expr::Dict(e) => e.span,
            Expr::Lambda(e) => e.span,
            Expr::IfExp(e) => e.span,
            Expr::BoolOp(e) => e.span,
            Expr::ListComp(e) => e.span,
            Expr::DictComp(e) => e.span,
            Expr::SetComp(e) => e.span,
            Expr::GeneratorExp(e) => e.span,
            Expr::Await(e) => e.span,
            Expr::NamedExpr(e) => e.span,
            Expr::JoinedStr(e) => e.span,
            Expr::FormattedValue(e) => e.span,
            Expr::TString(e) => e.span,
            Expr::Starred(e) => e.span,
            Expr::Yield(e) => e.span,
            Expr::YieldFrom(e) => e.span,
            Expr::ModuleIntrospection(e) => e.span,
        }
    }
}

/// Expression structures.
#[derive(Debug, Clone)]
pub struct ConstantExpr<'a> {
    pub value: &'a str,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ComplexExpr<'a> {
    pub value: &'a str, // The full complex literal like "3.5j"
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct BytesExpr<'a> {
    pub value: &'a str, // The full bytes literal like b"hello"
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct NameExpr<'a> {
    pub id: &'a str,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ListExpr<'a> {
    pub elts: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct TupleExpr<'a> {
    pub elts: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct SetExpr<'a> {
    pub elts: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct BinOpExpr<'a> {
    pub left: &'a Expr<'a>,
    pub op: &'a str,
    pub right: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct UnaryOpExpr<'a> {
    pub op: &'a str,
    pub operand: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct CompareExpr<'a> {
    pub left: &'a Expr<'a>,
    pub ops: &'a [&'a str],
    pub comparators: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct CallExpr<'a> {
    pub func: &'a Expr<'a>,
    pub args: &'a [Expr<'a>],
    pub keywords: &'a [Keyword<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct Keyword<'a> {
    pub arg: Option<&'a str>,
    pub value: Expr<'a>,
}

#[derive(Debug, Clone)]
pub struct AttributeExpr<'a> {
    pub value: &'a Expr<'a>,
    pub attr: &'a str,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct SubscriptExpr<'a> {
    pub value: &'a Expr<'a>,
    pub slice: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct SliceExpr<'a> {
    pub lower: Option<&'a Expr<'a>>,
    pub upper: Option<&'a Expr<'a>>,
    pub step: Option<&'a Expr<'a>>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct DictExpr<'a> {
    pub keys: &'a [Option<Expr<'a>>],
    pub values: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct LambdaExpr<'a> {
    pub args: Arguments<'a>,
    pub body: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct IfExpExpr<'a> {
    pub test: &'a Expr<'a>,
    pub body: &'a Expr<'a>,
    pub orelse: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct BoolOpExpr<'a> {
    pub op: &'a str,
    pub values: &'a [Expr<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct ListCompExpr<'a> {
    pub elt: &'a Expr<'a>,
    pub generators: &'a [Comprehension<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct DictCompExpr<'a> {
    pub key: &'a Expr<'a>,
    pub value: &'a Expr<'a>,
    pub generators: &'a [Comprehension<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct SetCompExpr<'a> {
    pub elt: &'a Expr<'a>,
    pub generators: &'a [Comprehension<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct GeneratorExpExpr<'a> {
    pub elt: &'a Expr<'a>,
    pub generators: &'a [Comprehension<'a>],
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct AwaitExpr<'a> {
    pub value: &'a Expr<'a>,
    pub span: TextRange,
}

#[derive(Debug, Clone)]
pub struct NamedExprNode<'a> {
    pub target: &'a Expr<'a>,
    pub value: &'a Expr<'a>,
    pub span: TextRange,
}

/// F-string: a joined string with literal and expression parts.
/// Example: f"Hello {name}" becomes JoinedStr with Constant and FormattedValue nodes
#[derive(Debug, Clone)]
pub struct JoinedStrExpr<'a> {
    pub values: &'a [Expr<'a>],
    pub span: TextRange,
}

/// FormattedValue: an expression inside an f-string with optional conversion and format_spec.
/// Example: {x!r:.2f} has value=x, conversion='r', format_spec=".2f"
#[derive(Debug, Clone)]
pub struct FormattedValueExpr<'a> {
    pub value: &'a Expr<'a>,
    pub conversion: Option<char>, // 's', 'r', or 'a'
    pub format_spec: Option<&'a JoinedStrExpr<'a>>,
    pub span: TextRange,
}

/// T-String: Template string with delayed interpolation
///
/// Unlike f-strings which eagerly interpolate at expression evaluation time,
/// t-strings are compiled to template functions by codegen.
///
/// This design enables:
///
/// - SQL-safe escaping: Template functions can apply parameterized query escaping
/// - HTML/XML templating: Automatic context-aware escaping for web output
/// - Custom escaping: Applications can define custom escaping strategies
///
/// Syntax:
///
/// - t"Hello {name}" - template string with interpolation
/// - rt"raw {template}" - raw template (escape sequences are literal)
///
/// Example: t"SELECT * FROM users WHERE id = {user_id}"
/// This compiles to a template function that safely escapes user_id for SQL,
/// rather than eagerly substituting it as a string.
///
/// The values field contains the template parts:
///
/// - Constant strings (Expr::Constant with Str)
/// - Interpolation expressions (any expression type)
/// - These are ordered as they appear in the original template string.
#[derive(Debug, Clone)]
pub struct TStringExpr<'a> {
    pub values: &'a [Expr<'a>],
    pub span: TextRange,
}

/// Starred: unpacking expression like *args in function calls
#[derive(Debug, Clone)]
pub struct StarredExpr<'a> {
    pub value: &'a Expr<'a>,
    pub span: TextRange,
}

/// Yield: yield expression
#[derive(Debug, Clone)]
pub struct YieldExpr<'a> {
    pub value: Option<&'a Expr<'a>>,
    pub span: TextRange,
}

/// YieldFrom: yield from expression
#[derive(Debug, Clone)]
pub struct YieldFromExpr<'a> {
    pub value: &'a Expr<'a>,
    pub span: TextRange,
}

/// Module introspection: compile-time module queries
/// Syntax: `module::function_name()`
/// Examples:
///   - `module::is_main()` - returns true if current module is entry point
///   - `module::name()` - returns the current module's fully qualified name
///   - `module::path()` - returns the file path of the current module
#[derive(Debug, Clone)]
pub struct ModuleIntrospectionExpr<'a> {
    pub function: &'a str, // "is_main", "name", "path", etc.
    pub span: TextRange,
}
