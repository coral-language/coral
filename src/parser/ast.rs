#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
  // Literals
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  Ident(String),

  // Binary operations
  Binary {
    left: Box<Expr>,
    op: BinaryOp,
    right: Box<Expr>,
  },

  // Unary operations
  Unary {
    op: UnaryOp,
    expr: Box<Expr>,
  },

  // Function call
  Call {
    callee: Box<Expr>,
    args: Vec<Expr>,
  },

  // Method call (pipeline)
  MethodCall {
    receiver: Box<Expr>,
    method: String,
    args: Vec<Expr>,
  },

  // If expression
  If {
    condition: Box<Expr>,
    then_branch: Vec<Stmt>,
    else_branch: Option<Vec<Stmt>>,
  },

  // Match expression
  Match {
    expr: Box<Expr>,
    arms: Vec<MatchArm>,
  },

  // List literal
  List(Vec<Expr>),

  // Map/Dict literal
  Map(Vec<(Expr, Expr)>),

  // Assignment
  Assign {
    name: String,
    value: Box<Expr>,
  },

  // Index access
  Index {
    object: Box<Expr>,
    index: Box<Expr>,
  },

  // Lambda/Closure
  Lambda {
    params: Vec<String>,
    body: Vec<Stmt>,
  },

  // Parenthesized expression
  Grouping(Box<Expr>),

  // String interpolation
  StringInterpolation {
    parts: Vec<StringPart>,
  },

  // Range expression
  Range {
    start: Box<Expr>,
    end: Box<Expr>,
    inclusive: bool,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
  Text(String),
  Expr(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
  // Arithmetic
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  // Comparison
  Equal,
  NotEqual,
  Less,
  Greater,
  LessEqual,
  GreaterEqual,

  // Logical
  And,
  Or,

  // Pipeline
  #[allow(dead_code)]
  Pipeline,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
  Not,
  Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
  pub pattern: Pattern,
  pub guard: Option<Box<Expr>>,
  pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
  Wildcard,
  Ident(String),
  Int(i64),
  String(String),
  Bool(bool),
  Variant { name: String, args: Vec<Pattern> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
  // Variable declaration
  Let {
    name: String,
    mutable: bool,
    type_annotation: Option<Type>,
    initializer: Expr,
  },

  // Expression statement
  Expr(Expr),

  // Return statement
  Return(Option<Expr>),

  // Function declaration
  Function {
    name: String,
    params: Vec<Param>,
    return_type: Option<Type>,
    body: Vec<Stmt>,
  },

  // Type definition
  TypeDef {
    name: String,
    type_def: TypeDefinition,
  },

  // Export statement
  Export(Vec<String>),

  // Import statement
  Import {
    items: Vec<ImportItem>,
    from: String,
  },

  // Module declaration
  Module(String),

  // For loop
  For {
    var: String,
    iter: Expr,
    body: Vec<Stmt>,
  },

  // While loop
  While {
    condition: Expr,
    body: Vec<Stmt>,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
  pub name: String,
  pub type_annotation: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  Simple(String),
  Generic {
    name: String,
    args: Vec<Type>,
  },
  Function {
    params: Vec<Type>,
    return_type: Box<Type>,
  },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
  Struct { fields: Vec<(String, Type)> },
  Enum { variants: Vec<(String, Vec<Type>)> },
  Alias(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ImportItem {
  Item(String),
  ItemAs { name: String, alias: String },
  Module(String),
  Type(String),
  TypeAs { name: String, alias: String },
}

#[derive(Debug, Clone)]
pub struct Program {
  pub statements: Vec<Stmt>,
}
