use coral_lang::lexer::Lexer;
use coral_lang::parser::{
  Parser,
  ast::{Expr, Stmt, StringPart},
};

fn parse_expression(source: &str) -> Expr {
  let mut lexer = Lexer::new(source.to_string());
  let tokens = lexer.tokenize().expect("lexing should succeed");
  let mut parser = Parser::new(tokens);
  let program = parser.parse().expect("parsing should succeed");

  match program.statements.first() {
    Some(Stmt::Expr(expr)) => expr.clone(),
    other => panic!("expected expression statement, found {:?}", other),
  }
}

#[test]
fn parses_map_literal_with_string_keys() {
  let expr = parse_expression("{\"foo\": 1, \"bar\": 2}");

  match expr {
    Expr::Map(pairs) => {
      assert_eq!(pairs.len(), 2);
      assert!(matches!(pairs[0].0, Expr::String(_)));
      assert!(matches!(pairs[0].1, Expr::Int(1)));
      assert!(matches!(pairs[1].0, Expr::String(_)));
      assert!(matches!(pairs[1].1, Expr::Int(2)));
    }
    other => panic!("expected map literal, found {:?}", other),
  }
}

#[test]
fn parses_string_interpolation_into_parts() {
  let expr = parse_expression("\"Hello, {name}!\"");

  match expr {
    Expr::StringInterpolation { parts } => {
      assert_eq!(parts.len(), 3);

      match &parts[0] {
        StringPart::Text(text) => assert_eq!(text, "Hello, "),
        other => panic!("expected leading text part, found {:?}", other),
      }

      match &parts[1] {
        StringPart::Expr(inner) => match inner.as_ref() {
          Expr::Ident(name) => assert_eq!(name, "name"),
          other => panic!("expected identifier interpolation, found {:?}", other),
        },
        other => panic!("expected interpolation expression, found {:?}", other),
      }

      match &parts[2] {
        StringPart::Text(text) => assert_eq!(text, "!"),
        other => panic!("expected trailing text part, found {:?}", other),
      }
    }
    other => panic!("expected string interpolation, found {:?}", other),
  }
}

#[test]
fn parses_if_expression_with_branches() {
  let expr = parse_expression("if condition do 1 else 2 end");

  match expr {
    Expr::If {
      condition,
      then_branch,
      else_branch,
    } => {
      assert!(matches!(*condition, Expr::Ident(ref name) if name == "condition"));
      assert_eq!(then_branch.len(), 1);
      assert_eq!(else_branch.as_ref().map(|b| b.len()), Some(1));
    }
    other => panic!("expected if expression, found {:?}", other),
  }
}

#[test]
fn parses_lambda_expression() {
  let expr = parse_expression("|x, y| x + y");

  match expr {
    Expr::Lambda { params, body } => {
      assert_eq!(params, vec!["x".to_string(), "y".to_string()]);
      assert_eq!(body.len(), 1);
    }
    other => panic!("expected lambda expression, found {:?}", other),
  }
}

#[test]
fn reports_error_for_unclosed_list() {
  let mut lexer = Lexer::new("[1, 2".to_string());
  let tokens = lexer.tokenize().unwrap();
  let mut parser = Parser::new(tokens);
  let result = parser.parse();

  let err = result.expect_err("parsing should fail for unclosed list");
  assert!(err.contains("]"));
}
