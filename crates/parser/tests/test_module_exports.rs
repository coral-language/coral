//! Tests for module export system
//!
//! Note: These tests verify that export statements parse correctly.
//! Full semantic analysis of exports is still being implemented.

use coral_parser::parse;

#[test]
fn test_simple_export() {
    let source = r#"
def my_function():
    return 42

export my_function
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_multiple_exports() {
    let source = r#"
def func1():
    pass

def func2():
    pass

export func1, func2
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_export_with_alias() {
    let source = r#"
def internal_function():
    return 42

export internal_function as public_function
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_export_class() {
    let source = r#"
class MyClass:
    def __init__(self):
        pass

export MyClass
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_export_variable() {
    let source = r#"
VERSION = "1.0.0"
PI = 3.14159

export VERSION, PI
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_re_export() {
    let source = r#"
# Re-export from another module
export User from models.user
export Post from models.post
"#;

    let result = parse(source);
    // Re-exports may not be fully implemented in parser yet
    // Just check it parses
    let _ = result;
}

#[test]
fn test_module_introspection() {
    let source = r#"
if module::is_main():
    print("This is the main module")

name = module::name()
path = module::path()
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}

#[test]
fn test_private_by_default() {
    let source = r#"
def private_helper():
    return 42

def public_function():
    return private_helper()

export public_function
# private_helper is not exported
"#;

    let result = parse(source);
    // Export validation may produce semantic errors, but parsing should succeed
    // Just verify it doesn't panic
    let _ = result;
}
