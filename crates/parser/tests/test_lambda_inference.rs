//! Comprehensive tests for lambda type inference functionality

use coral_parser::semantic::passes::type_inference::{TypeInference, TypeInferenceContext};
use coral_parser::semantic::symbol::SymbolTable;
use coral_parser::{Arena, Lexer, Parser};

fn infer_types(source: &str) {
    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);
    let module = parser.parse_module().expect("Parse failed");

    let symbol_table = SymbolTable::new();
    let mut context = TypeInferenceContext::new(symbol_table);
    let mut inference = TypeInference::new(&mut context);
    inference.infer_module(module);

    // Lambda inference should run without panicking
    // Full integration with name resolution would be tested separately
}

#[test]
fn test_lambda_identity() {
    infer_types("id = lambda x: x");
}

#[test]
fn test_lambda_arithmetic() {
    infer_types("double = lambda x: x * 2");
}

#[test]
fn test_lambda_with_multiple_params() {
    infer_types("add = lambda x, y: x + y");
}

#[test]
fn test_nested_lambdas() {
    let source = r#"
outer = lambda x: (lambda y: x + y)
"#;
    infer_types(source);
}

#[test]
fn test_lambda_in_call() {
    let source = r#"
def apply_twice(f, x: int) -> int:
    return f(f(x))

result = apply_twice(lambda n: n + 1, 5)
"#;
    infer_types(source);
}

#[test]
fn test_lambda_closure_multi_level() {
    let source = r#"
def outer(a: int):
    def middle(b: int):
        return lambda c: a + b + c
    return middle

f = outer(1)(2)
"#;
    infer_types(source);
}

#[test]
fn test_lambda_with_explicit_annotations() {
    let source = r#"
explicit = lambda x: int, y: str: len(str(x)) + len(y)
"#;
    infer_types(source);
}

#[test]
fn test_lambda_higher_order() {
    let source = r#"
def compose(f, g):
    return lambda x: f(g(x))

add_one = lambda x: x + 1
double = lambda x: x * 2
composed = compose(add_one, double)
"#;
    infer_types(source);
}

#[test]
fn test_lambda_complex_nesting() {
    let source = r#"
def make_counter():
    count = 0
    return lambda: (lambda: count)

counter_factory = make_counter()
inner = counter_factory()
"#;
    infer_types(source);
}

#[test]
fn test_lambda_with_builtin_calls() {
    let source = r#"
mapper = lambda items: [x * 2 for x in items]
filterer = lambda items: [x for x in items if x > 0]
"#;
    infer_types(source);
}

#[test]
fn test_lambda_returning_lambda() {
    let source = r#"
def make_curry(f):
    return lambda x: lambda y: f(x, y)

curry_add = make_curry(lambda a, b: a + b)
partial = curry_add(5)
result = partial(3)
"#;
    infer_types(source);
}
