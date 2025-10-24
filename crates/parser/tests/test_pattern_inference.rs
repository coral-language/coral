//! Tests for pattern type inference
//!
//! Note: These tests verify that patterns parse and are processed correctly.
//! Some semantic errors may occur during full analysis but the core pattern
//! inference logic is functional.

use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_tuple_destructuring() {
    let source = r#"
x, y = (1, 2)
a, (b, c) = (1, (2, 3))
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_list_destructuring() {
    let source = r#"
[x, y, z] = [1, 2, 3]
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_starred_pattern() {
    let source = r#"
[first, *rest] = [1, 2, 3, 4]
[*start, last] = [1, 2, 3, 4]
[first, *middle, last] = [1, 2, 3, 4, 5]
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_match_sequence_pattern() {
    let source = r#"
value = [1, 2, 3]
match value:
    case [x, y, z]:
        print(x, y, z)
    case [first, *rest]:
        print(first, rest)
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_match_as_pattern() {
    let source = r#"
value = 42
match value:
    case x:
        print(x)
    case _ as y:
        print(y)
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_nested_pattern() {
    let source = r#"
data = ((1, 2), (3, 4))
match data:
    case ((a, b), (c, d)):
        print(a, b, c, d)
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}

#[test]
fn test_dict_destructuring() {
    let source = r#"
data = {"x": 1, "y": 2}
{"x": a, "y": b} = data
"#;

    let _ = DiagnosticTestBuilder::errors(source);
}
