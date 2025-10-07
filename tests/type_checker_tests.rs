use coral_lang::lexer::Lexer;
use coral_lang::parser::Parser;
use coral_lang::types::checker::TypeChecker;

fn type_check(source: &str) -> Result<(), String> {
  let mut lexer = Lexer::new(source.to_string());
  let tokens = lexer.tokenize().map_err(|err| err.to_string())?;
  let mut parser = Parser::new(tokens);
  let program = parser.parse()?;
  let mut checker = TypeChecker::new();
  checker.check_program(&program)
}

#[test]
fn accepts_homogeneous_map_literal() {
  let result = type_check("data = {\"x\": 1, \"y\": 2}");
  assert!(result.is_ok());
}

#[test]
fn rejects_list_with_mismatched_element_types() {
  let result = type_check("values = [1, \"two\"]");
  let err = result.expect_err("list type mismatch should fail");
  assert!(err.contains("List elements must have same type"));
}

#[test]
fn rejects_variable_annotation_mismatch() {
  let result = type_check("value: Int = 3.14");
  let err = result.expect_err("annotation mismatch should fail");
  assert!(err.contains("Type mismatch"));
}

#[test]
fn rejects_if_condition_that_is_not_boolean() {
  let result = type_check("if 42 do 1 else 2 end");
  let err = result.expect_err("if condition type should fail");
  assert!(err.contains("if condition must be Bool"));
}

#[test]
fn rejects_function_return_type_mismatch() {
  let result = type_check(
    r#"
fn identity() -> Int do
  return "nope"
end
"#,
  );
  let err = result.expect_err("function return mismatch should fail");
  assert!(err.contains("declared to return"));
}
