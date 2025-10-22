//! Comprehensive tests for generator type inference functionality

use coral_parser::semantic::passes::name_resolution::NameResolver;
use coral_parser::semantic::passes::type_inference::{TypeInference, TypeInferenceContext};
use coral_parser::{Arena, Lexer, Parser};

fn infer_types(source: &str) -> TypeInferenceContext {
    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);
    let module = parser.parse_module().expect("Parse failed");

    // Run name resolution first
    let mut resolver = NameResolver::new();
    resolver.resolve_module(module);
    let (symbol_table, _name_errors) = resolver.into_symbol_table();

    let mut context = TypeInferenceContext::new(symbol_table);
    let mut inference = TypeInference::new(&mut context);
    inference.infer_module(module);

    context
}

#[test]
fn test_generator_function_with_yield() {
    let source = r#"
def gen():
    yield 1
    yield 2
    "#;
    let ctx = infer_types(source);
    assert!(ctx.is_generator_function("gen"));
}

#[test]
fn test_generator_yield_type_inference() {
    let source = r#"
def gen():
    yield 42
    "#;
    infer_types(source);
}

#[test]
fn test_generator_yield_from() {
    let source = r#"
def gen():
    yield from [1, 2, 3]
    "#;
    infer_types(source);
}

#[test]
fn test_generator_expression_type() {
    let source = r#"
g = (x * 2 for x in [1, 2, 3])
    "#;
    infer_types(source);
}

#[test]
fn test_multiple_yield_types() {
    let source = r#"
def gen():
    yield 1
    yield "string"
    "#;
    let ctx = infer_types(source);
    // Should infer Union[int, str] as yield type
    assert!(ctx.is_generator_function("gen"));
}
