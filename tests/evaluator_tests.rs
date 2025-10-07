use coral_lang::evaluator::{Evaluator, value::Value};
use coral_lang::lexer::Lexer;
use coral_lang::parser::Parser;

fn eval_source(source: &str) -> Result<Value, String> {
  let mut lexer = Lexer::new(source.to_string());
  let tokens = lexer.tokenize().map_err(|err| err.to_string())?;
  let mut parser = Parser::new(tokens);
  let program = parser.parse()?;
  let mut evaluator = Evaluator::new();
  evaluator.eval_program(&program)
}

#[test]
fn evaluates_basic_arithmetic() {
  assert_eq!(eval_source("2 + 3").unwrap(), Value::Int(5));
  assert_eq!(eval_source("10 - 4").unwrap(), Value::Int(6));
  assert_eq!(eval_source("3 * 4").unwrap(), Value::Int(12));
  assert_eq!(eval_source("10 / 2").unwrap(), Value::Int(5));
}

#[test]
fn evaluates_variable_reassignment() {
  let result = eval_source(
    r#"
nums = [1, 2]
nums = nums.push(3)
nums.len()
"#,
  )
  .unwrap();

  assert_eq!(result, Value::Int(3));
}

#[test]
fn evaluates_function_definition_and_call() {
  let result = eval_source(
    r#"
fn add(x: Int, y: Int) -> Int do
  return x + y
end

add(5, 7)
"#,
  )
  .unwrap();

  assert_eq!(result, Value::Int(12));
}

#[test]
fn evaluates_string_interpolation_and_methods() {
  let result = eval_source(
    r#"
text = "  Hello, World!  "
text.trim().uppercase()
"#,
  )
  .unwrap();

  assert_eq!(result, Value::String("HELLO, WORLD!".to_string()));
}

#[test]
fn evaluates_inclusive_range_literal() {
  let result = eval_source("1..=5").unwrap();
  assert_eq!(
    result,
    Value::Range {
      start: 1,
      end: 5,
      inclusive: true
    }
  );
}

#[test]
fn evaluates_option_construction() {
  let result = eval_source("Some(42)").unwrap();
  match result {
    Value::Option(inner) => {
      let value = *inner.expect("expected Some value");
      assert_eq!(value, Value::Int(42));
    }
    other => panic!("expected Option value, found {:?}", other),
  }
}

#[test]
fn evaluates_split_and_join_builtins() {
  let result = eval_source(r#"join(split("a,b,c", ","), "-")"#).unwrap();
  assert_eq!(result, Value::String("a-b-c".to_string()));
}
