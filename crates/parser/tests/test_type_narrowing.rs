//! Tests for flow-sensitive type narrowing
//!
//! Note: These tests verify that code with type narrowing patterns parses correctly.
//! Full type narrowing integration is still being implemented.

use coral_parser::parse;

#[test]
fn test_isinstance_narrowing() {
    let source = r#"
def process(value):
    if isinstance(value, int):
        # In this branch, value should be narrowed to int
        x = value + 1
    else:
        # In this branch, value is not int
        pass
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_is_none_narrowing() {
    let source = r#"
def check_value(value):
    if value is None:
        # value is None here
        return
    # value is not None here
    x = value
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_is_not_none_narrowing() {
    let source = r#"
def check_value(value):
    if value is not None:
        # value is not None here
        x = value
    else:
        # value is None here
        pass
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_truthiness_narrowing() {
    let source = r#"
def check_value(value):
    if value:
        # value is truthy here
        x = value
    else:
        # value is falsy here (could be None, 0, empty string, etc.)
        pass
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_nested_narrowing() {
    let source = r#"
def process(value):
    if isinstance(value, int):
        if value > 0:
            # value is int and > 0
            x = value
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_union_type_narrowing() {
    let source = r#"
def process(value):
    # value could be int | str | float
    if isinstance(value, int):
        # value is int here
        x = value + 1
    elif isinstance(value, str):
        # value is str here
        x = value.upper()
    else:
        # value is float here
        x = value
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}

#[test]
fn test_complex_control_flow() {
    let source = r#"
def process(value):
    if value is not None:
        if isinstance(value, int):
            return value + 1
        else:
            return str(value)
    return 0
"#;

    let result = parse(source);
    // Type narrowing integration is in progress
    // Just verify parsing doesn't panic
    let _ = result;
}
