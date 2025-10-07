#[derive(Debug, Clone)]
pub struct CoralError {
  pub kind: ErrorKind,
  pub message: String,
  pub line: usize,
  pub column: usize,
  pub file: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
  Lex,
  Parse,
  Type,
  Runtime,
}

impl CoralError {
  pub fn new(kind: ErrorKind, message: String, line: usize, column: usize) -> Self {
    Self {
      kind,
      message,
      line,
      column,
      file: None,
    }
  }

  pub fn with_file(mut self, file: String) -> Self {
    self.file = Some(file);
    self
  }

  pub fn format(&self) -> String {
    let kind_str = match self.kind {
      ErrorKind::Lex => "Lexical Error",
      ErrorKind::Parse => "Parse Error",
      ErrorKind::Type => "Type Error",
      ErrorKind::Runtime => "Runtime Error",
    };

    let location = if let Some(ref file) = self.file {
      format!("{}:{}:{}", file, self.line, self.column)
    } else {
      format!("line {}:{}", self.line, self.column)
    };

    format!("❌ {} at {}\n  {}", kind_str, location, self.message)
  }
}

impl std::fmt::Display for CoralError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.format())
  }
}

impl std::error::Error for CoralError {}
