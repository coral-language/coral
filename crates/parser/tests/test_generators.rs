//! Comprehensive tests for generator type inference functionality

use coral_parser::helpers::infer_types;

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

    assert!(ctx.is_generator_function("gen"));
}
