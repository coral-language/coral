use coral_lang::error::ErrorKind;
use coral_lang::lexer::{Lexer, token::TokenType};

fn tokenize_types(source: &str) -> Vec<TokenType> {
  let mut lexer = Lexer::new(source.to_string());
  lexer
    .tokenize()
    .expect("lexer should succeed")
    .into_iter()
    .map(|token| token.token_type)
    .collect()
}

#[test]
fn lexes_basic_function_signature() {
  let tokens = tokenize_types("fn add(x, y) do return x + y end");

  assert_eq!(
    tokens,
    vec![
      TokenType::Fn,
      TokenType::Ident,
      TokenType::LParen,
      TokenType::Ident,
      TokenType::Comma,
      TokenType::Ident,
      TokenType::RParen,
      TokenType::Do,
      TokenType::Return,
      TokenType::Ident,
      TokenType::Plus,
      TokenType::Ident,
      TokenType::End,
      TokenType::Eof,
    ]
  );
}

#[test]
fn lexes_numbers_and_allows_underscores() {
  let mut lexer = Lexer::new("42 3.14 1_000_000".to_string());
  let tokens = lexer.tokenize().unwrap();

  assert_eq!(tokens[0].token_type, TokenType::Int);
  assert_eq!(tokens[0].lexeme, "42");
  assert_eq!(tokens[1].token_type, TokenType::Float);
  assert_eq!(tokens[1].lexeme, "3.14");
  assert_eq!(tokens[2].token_type, TokenType::Int);
  assert_eq!(tokens[2].lexeme, "1000000");
}

#[test]
fn skips_single_and_multiline_comments() {
  let source = r#"
fn main() do
  // ignore me
  return 1 /* also ignored */
end
"#;
  let tokens = tokenize_types(source);

  assert!(tokens.contains(&TokenType::Fn));
  assert!(tokens.contains(&TokenType::Return));
  assert!(!tokens.iter().any(|t| matches!(t, TokenType::Slash)));
}

#[test]
fn lexes_range_tokens() {
  let tokens = tokenize_types("1..10 1..=10");

  assert_eq!(
    tokens,
    vec![
      TokenType::Int,
      TokenType::DotDot,
      TokenType::Int,
      TokenType::Int,
      TokenType::DotDotEq,
      TokenType::Int,
      TokenType::Eof,
    ]
  );
}

#[test]
fn reports_unterminated_strings() {
  let mut lexer = Lexer::new("\"hello".to_string());
  let err = lexer.tokenize().unwrap_err();

  assert_eq!(err.kind, ErrorKind::Lex);
  assert!(err.message.contains("Unterminated string literal"));
}

#[test]
fn reports_unexpected_characters() {
  let mut lexer = Lexer::new("@".to_string());
  let err = lexer.tokenize().unwrap_err();

  assert_eq!(err.kind, ErrorKind::Lex);
  assert!(err.message.contains("Unexpected character '@'"));
}
