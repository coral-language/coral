#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
  // Literals
  Int,
  Float,
  String,
  True,
  False,

  // Identifiers and keywords
  Ident,

  // Keywords
  Fn,
  Do,
  End,
  Return,
  If,
  Else,
  Match,
  When,
  Mut,
  Module,
  Import,
  Export,
  From,
  As,
  Type,
  Async,
  Await,
  For,
  In,
  While,
  Trait,
  Impl,
  Spawn,

  // Operators
  Plus,    // +
  Minus,   // -
  Star,    // *
  Slash,   // /
  Percent, // %

  Eq,    // =
  EqEq,  // ==
  NotEq, // !=
  Lt,    //
  Gt,    // >
  LtEq,  // <=
  GtEq,  // >=

  And, // and
  Or,  // or
  Not, // not

  Dot,           // .
  DotDot,        // ..
  DotDotEq,      // ..=
  Question,      // ?
  QuestionDot,   // ?.
  QuestionColon, // ?:
  Arrow,         // ->
  Pipe,          // |

  // Delimiters
  LParen,   // (
  RParen,   // )
  LBrace,   // {
  RBrace,   // }
  LBracket, // [
  RBracket, // ]
  Comma,    // ,
  Colon,    // :

  // Special
  Newline,
  Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
  pub token_type: TokenType,
  pub lexeme: String,
  pub line: usize,
  pub column: usize,
}

impl Token {
  pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
    Self {
      token_type,
      lexeme,
      line,
      column,
    }
  }
}

pub fn keyword_or_ident(word: &str) -> TokenType {
  match word {
    "fn" | "func" | "function" => TokenType::Fn,
    "do" => TokenType::Do,
    "end" => TokenType::End,
    "return" => TokenType::Return,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "match" => TokenType::Match,
    "when" => TokenType::When,
    "mut" => TokenType::Mut,
    "module" => TokenType::Module,
    "import" => TokenType::Import,
    "export" => TokenType::Export,
    "from" => TokenType::From,
    "as" => TokenType::As,
    "type" => TokenType::Type,
    "async" => TokenType::Async,
    "await" => TokenType::Await,
    "for" => TokenType::For,
    "in" => TokenType::In,
    "while" => TokenType::While,
    "trait" => TokenType::Trait,
    "impl" => TokenType::Impl,
    "spawn" => TokenType::Spawn,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "and" => TokenType::And,
    "or" => TokenType::Or,
    "not" => TokenType::Not,
    _ => TokenType::Ident,
  }
}
