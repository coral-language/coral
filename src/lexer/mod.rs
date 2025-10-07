pub mod token;

use crate::error::{CoralError, ErrorKind};
use token::{Token, TokenType, keyword_or_ident};

pub struct Lexer {
  input: Vec<char>,
  position: usize,
  line: usize,
  column: usize,
}

impl Lexer {
  pub fn new(input: String) -> Self {
    Self {
      input: input.chars().collect(),
      position: 0,
      line: 1,
      column: 1,
    }
  }

  fn current_char(&self) -> Option<char> {
    if self.position >= self.input.len() {
      None
    } else {
      Some(self.input[self.position])
    }
  }

  fn peek_char(&self) -> Option<char> {
    if self.position + 1 >= self.input.len() {
      None
    } else {
      Some(self.input[self.position + 1])
    }
  }

  fn advance(&mut self) -> Option<char> {
    if let Some(ch) = self.current_char() {
      self.position += 1;
      if ch == '\n' {
        self.line += 1;
        self.column = 1;
      } else {
        self.column += 1;
      }
      Some(ch)
    } else {
      None
    }
  }

  fn skip_whitespace(&mut self) {
    while let Some(ch) = self.current_char() {
      if ch.is_whitespace() && ch != '\n' {
        self.advance();
      } else {
        break;
      }
    }
  }

  fn skip_comment(&mut self) {
    // Single-line comment: //
    if self.current_char() == Some('/') && self.peek_char() == Some('/') {
      while self.current_char().is_some() && self.current_char() != Some('\n') {
        self.advance();
      }
    }

    // Multi-line comment: /* */
    if self.current_char() == Some('/') && self.peek_char() == Some('*') {
      self.advance(); // /
      self.advance(); // *

      while self.current_char().is_some() {
        if self.current_char() == Some('*') && self.peek_char() == Some('/') {
          self.advance(); // *
          self.advance(); // /
          break;
        }
        self.advance();
      }
    }
  }

  fn read_number(&mut self) -> Token {
    let start_line = self.line;
    let start_column = self.column;
    let mut num = String::new();
    let mut is_float = false;

    while let Some(ch) = self.current_char() {
      if ch.is_ascii_digit() {
        num.push(ch);
        self.advance();
      } else if ch == '.'
        && self
          .peek_char()
          .map(|c| c.is_ascii_digit())
          .unwrap_or(false)
      {
        is_float = true;
        num.push(ch);
        self.advance();
      } else if ch == '_' {
        // Allow underscores in numbers for readability (e.g., 1_000_000)
        self.advance();
      } else {
        break;
      }
    }

    Token::new(
      if is_float {
        TokenType::Float
      } else {
        TokenType::Int
      },
      num,
      start_line,
      start_column,
    )
  }

  fn read_string(&mut self) -> Result<Token, CoralError> {
    let start_line = self.line;
    let start_column = self.column;
    let mut string = String::new();
    let mut terminated = false;

    self.advance(); // Skip opening quote

    while let Some(ch) = self.current_char() {
      if ch == '"' {
        self.advance(); // Skip closing quote
        terminated = true;
        break;
      } else if ch == '\\' {
        self.advance();
        if let Some(escaped) = self.current_char() {
          match escaped {
            'n' => string.push('\n'),
            't' => string.push('\t'),
            'r' => string.push('\r'),
            '\\' => string.push('\\'),
            '"' => string.push('"'),
            _ => {
              string.push('\\');
              string.push(escaped);
            }
          }
          self.advance();
        } else {
          // EOF after backslash
          break;
        }
      } else {
        string.push(ch);
        self.advance();
      }
    }

    if !terminated {
      Err(CoralError::new(
        ErrorKind::Lex,
        "Unterminated string literal".to_string(),
        start_line,
        start_column,
      ))
    } else {
      Ok(Token::new(
        TokenType::String,
        string,
        start_line,
        start_column,
      ))
    }
  }

  fn read_identifier(&mut self) -> Token {
    let start_line = self.line;
    let start_column = self.column;
    let mut ident = String::new();

    while let Some(ch) = self.current_char() {
      if ch.is_alphanumeric() || ch == '_' {
        ident.push(ch);
        self.advance();
      } else {
        break;
      }
    }

    let token_type = keyword_or_ident(&ident);
    Token::new(token_type, ident, start_line, start_column)
  }

  pub fn next_token(&mut self) -> Result<Token, CoralError> {
    loop {
      self.skip_whitespace();

      if self.current_char() == Some('/')
        && (self.peek_char() == Some('/') || self.peek_char() == Some('*'))
      {
        self.skip_comment();
        continue;
      }

      break;
    }

    let line = self.line;
    let column = self.column;

    let token = match self.current_char() {
      Option::None => Token::new(TokenType::Eof, String::new(), line, column),

      Some('\n') => {
        self.advance();
        Token::new(TokenType::Newline, "\n".to_string(), line, column)
      }

      Some(ch) if ch.is_ascii_digit() => self.read_number(),

      Some('"') => return self.read_string(),

      Some(ch) if ch.is_alphabetic() || ch == '_' => self.read_identifier(),

      Some('+') => {
        self.advance();
        Token::new(TokenType::Plus, "+".to_string(), line, column)
      }

      Some('-') => {
        self.advance();
        if self.current_char() == Some('>') {
          self.advance();
          Token::new(TokenType::Arrow, "->".to_string(), line, column)
        } else {
          Token::new(TokenType::Minus, "-".to_string(), line, column)
        }
      }

      Some('*') => {
        self.advance();
        Token::new(TokenType::Star, "*".to_string(), line, column)
      }

      Some('/') => {
        self.advance();
        Token::new(TokenType::Slash, "/".to_string(), line, column)
      }

      Some('%') => {
        self.advance();
        Token::new(TokenType::Percent, "%".to_string(), line, column)
      }

      Some('=') => {
        self.advance();
        if self.current_char() == Some('=') {
          self.advance();
          Token::new(TokenType::EqEq, "==".to_string(), line, column)
        } else {
          Token::new(TokenType::Eq, "=".to_string(), line, column)
        }
      }

      Some('!') => {
        self.advance();
        if self.current_char() == Some('=') {
          self.advance();
          Token::new(TokenType::NotEq, "!=".to_string(), line, column)
        } else {
          Token::new(TokenType::Not, "!".to_string(), line, column)
        }
      }

      Some('<') => {
        self.advance();
        if self.current_char() == Some('=') {
          self.advance();
          Token::new(TokenType::LtEq, "<=".to_string(), line, column)
        } else {
          Token::new(TokenType::Lt, "<".to_string(), line, column)
        }
      }

      Some('>') => {
        self.advance();
        if self.current_char() == Some('=') {
          self.advance();
          Token::new(TokenType::GtEq, ">=".to_string(), line, column)
        } else {
          Token::new(TokenType::Gt, ">".to_string(), line, column)
        }
      }

      Some('.') => {
        self.advance();
        if self.current_char() == Some('.') {
          self.advance();
          if self.current_char() == Some('=') {
            self.advance();
            Token::new(TokenType::DotDotEq, "..=".to_string(), line, column)
          } else {
            Token::new(TokenType::DotDot, "..".to_string(), line, column)
          }
        } else {
          Token::new(TokenType::Dot, ".".to_string(), line, column)
        }
      }

      Some('?') => {
        self.advance();
        if self.current_char() == Some('.') {
          self.advance();
          Token::new(TokenType::QuestionDot, "?.".to_string(), line, column)
        } else if self.current_char() == Some(':') {
          self.advance();
          Token::new(TokenType::QuestionColon, "?:".to_string(), line, column)
        } else {
          Token::new(TokenType::Question, "?".to_string(), line, column)
        }
      }

      Some('|') => {
        self.advance();
        Token::new(TokenType::Pipe, "|".to_string(), line, column)
      }

      Some('(') => {
        self.advance();
        Token::new(TokenType::LParen, "(".to_string(), line, column)
      }

      Some(')') => {
        self.advance();
        Token::new(TokenType::RParen, ")".to_string(), line, column)
      }

      Some('{') => {
        self.advance();
        Token::new(TokenType::LBrace, "{".to_string(), line, column)
      }

      Some('}') => {
        self.advance();
        Token::new(TokenType::RBrace, "}".to_string(), line, column)
      }

      Some('[') => {
        self.advance();
        Token::new(TokenType::LBracket, "[".to_string(), line, column)
      }

      Some(']') => {
        self.advance();
        Token::new(TokenType::RBracket, "]".to_string(), line, column)
      }

      Some(',') => {
        self.advance();
        Token::new(TokenType::Comma, ",".to_string(), line, column)
      }

      Some(':') => {
        self.advance();
        Token::new(TokenType::Colon, ":".to_string(), line, column)
      }

      Some(ch) => {
        return Err(CoralError::new(
          ErrorKind::Lex,
          format!("Unexpected character '{}'", ch),
          line,
          column,
        ));
      }
    };

    Ok(token)
  }

  pub fn tokenize(&mut self) -> Result<Vec<Token>, CoralError> {
    let mut tokens = Vec::new();

    loop {
      let token = self.next_token()?;
      let is_eof = token.token_type == TokenType::Eof;

      if token.token_type != TokenType::Newline {
        tokens.push(token);
      }

      if is_eof {
        break;
      }
    }

    Ok(tokens)
  }
}
