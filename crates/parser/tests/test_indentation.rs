//! Comprehensive integration tests for indentation edge cases.
//!
//! This test suite validates that the lexer correctly handles:
//! - Mixed tabs/spaces detection and warnings
//! - Inconsistent indentation warnings
//! - Better error messages for indentation issues
//! - EOF dedent scenarios with complex nesting

use coral_parser::helpers::{has_error_code, has_warning_code, parse_and_get_diagnostics};

#[test]
fn test_valid_indentation_spaces_4() {
    let source = r#"
def foo():
    x = 0
    y = 0
    if True:
        x = 1
        y = 2
    return x + y

class Bar:
    def baz(self):
        pass
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Valid 4-space indentation should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Valid 4-space indentation should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_valid_indentation_spaces_2() {
    let source = r#"
def foo():
  x = 0
  condition = True
  if condition:
    x = 1
  return x

y = 2
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Valid 2-space indentation should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Valid 2-space indentation should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_valid_indentation_tabs() {
    let source = "def foo():\n\tx = 0\n\tif True:\n\t\tx = 1\n\treturn x\n";
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Valid tab indentation should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Valid tab indentation should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_valid_complex_nesting() {
    let source = r#"
def foo():
    x = 1
    if x:
        y = 2
    else:
        y = 3
    return y
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Valid complex nesting should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Valid complex nesting should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_valid_blank_lines_preserve_indent() {
    let source = r#"
def foo():

    x = 1

    y = 0
    if True:

        y = 2

    return x + y
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Blank lines should preserve indentation context: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Blank lines should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_valid_comments_at_indent_levels() {
    let source = r#"
# Top level comment
def foo():
    # Function comment
    x = 0
    if True:
        # If block comment
        x = 1  # Inline comment
    # After if comment
    return x
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Comments at various indent levels should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Comments should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_mixed_tabs_spaces_in_line() {
    let source = "def foo():\n\t    x = 1\n\t    y = 2\n"; // Both lines have 1 tab + 4 spaces = 12 spaces equivalent
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Mixed tabs/spaces with consistent structure should not produce errors: {:?}",
        errors
    );
    assert!(
        has_warning_code(&warnings, "W1005"),
        "Should warn about mixed tabs and spaces: {:?}",
        warnings
    );
    assert!(
        warnings.iter().any(|w| w.contains("Mixed tabs and spaces")),
        "Warning should mention mixed tabs and spaces"
    );
}

#[test]
fn test_tabs_after_spaces_consistency() {
    let source = "def foo():\n\tx = 1\n        if True:\n            pass\n";
    let (errors, warnings) = parse_and_get_diagnostics(source);

    assert!(
        errors.is_empty(),
        "Consistent structure with inconsistent style should not produce errors: {:?}",
        errors
    );
    assert!(
        has_warning_code(&warnings, "W1005"),
        "Should warn about inconsistent indentation: {:?}",
        warnings
    );
    assert!(
        warnings
            .iter()
            .any(|w| w.contains("Inconsistent indentation")),
        "Warning should mention inconsistent indentation"
    );
}

#[test]
fn test_spaces_after_tabs_consistency() {
    let source = "def foo():\n\tx = 1\n    y = 2\n";
    let (errors, warnings) = parse_and_get_diagnostics(source);

    assert!(
        has_error_code(&errors, "E2029"),
        "Should detect unindent mismatch error: {:?}",
        errors
    );

    assert!(
        warnings
            .iter()
            .any(|w| w.contains("Inconsistent indentation")),
        "May warn about inconsistent indentation: {:?}",
        warnings
    );
}

#[test]
fn test_mixed_tabs_spaces_complex() {
    let source = r#"
def foo():
	if True:  # Tab
    x = 1   # Spaces
  elif False:  # Different spaces
		y = 2   # Tab again
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);

    assert!(
        !errors.is_empty(),
        "Complex inconsistent indentation should produce errors: {:?}",
        errors
    );

    let inconsistent_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Inconsistent indentation"))
        .collect();
    assert!(
        !inconsistent_warnings.is_empty(),
        "Should warn about inconsistent indentation: {:?}",
        warnings
    );
}

#[test]
fn test_unindent_mismatch() {
    let source = r#"
def foo():
    if True:
        x = 1
      y = 2  # Invalid unindent
"#;
    let (errors, _) = parse_and_get_diagnostics(source);
    assert!(
        has_error_code(&errors, "E2029"),
        "Should detect unindent mismatch error: {:?}",
        errors
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("doesn't align with any previous")),
        "Error should mention unindent mismatch"
    );
}

#[test]
fn test_irregular_indent_increase() {
    let source = r#"
def foo():
    x = 1
       y = 2  # Irregular indent increase
"#;
    let (errors, _warnings) = parse_and_get_diagnostics(source);

    assert!(
        !errors.is_empty(),
        "Irregular indent increase should produce errors: {:?}",
        errors
    );
}

#[test]
fn test_dedent_skips_levels() {
    let source = r#"
def foo():
    if True:
        if False:
            x = 1
    y = 2  # Dedents past multiple levels
"#;
    let (errors, _) = parse_and_get_diagnostics(source);

    assert!(
        errors.is_empty(),
        "Dedenting past multiple levels should be valid: {:?}",
        errors
    );
}

#[test]
fn test_eof_multiple_nested_blocks() {
    let source = r#"
def process(x):
    pass

def outer():
    def inner():
        if True:
            for x in range(10):
                try:
                    process(x)
                except:
                    continue
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "EOF with multiple nested blocks should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "EOF with multiple nested blocks should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_eof_function_ending() {
    let source = r#"
def foo():
    if True:
        x = 1
        if False:
            y = 2
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "EOF ending function should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "EOF ending function should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_eof_try_except_finally() {
    let source = r#"
def risky_operation():
    pass

def handle_value_error():
    pass

def cleanup():
    pass

def foo():
    try:
        risky_operation()
    except ValueError:
        handle_value_error()
    finally:
        cleanup()
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "EOF ending try/except/finally should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "EOF ending try/except/finally should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_eof_match_statement() {
    let source = r#"
def classify(value):
    match value:
        case 0:
            return "zero"
        case _:
            return "other"
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "EOF ending match statement should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "EOF ending match statement should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_eof_no_trailing_newline() {
    let source = r#"
def foo():
    x = 0
    if True:
        x = 1
    return x"#; // No trailing newline
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "EOF without trailing newline should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "EOF without trailing newline should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_indentation_inside_brackets_ignored() {
    let source = r#"
def condition(item):
    return True

def foo():
    items = [1, 2, 3]
    result = [1, 2, 3]
    return result
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Indentation inside brackets should be ignored: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Indentation inside brackets should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_dedent_after_closing_bracket() {
    let source = r#"
def foo():
    data = {
        'key': [
            1,
            2,
            3
        ]
    }
    return data
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Dedent after closing bracket should work: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Dedent after closing bracket should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_mixed_brackets_with_indentation() {
    let source = r#"
def func(arg1, arg2, arg3):
    return (arg1, arg2, arg3)

def foo():
    arg1 = 1
    result = func(
        arg1,
        arg2=[
            1,
            2
        ],
        arg3={
            'a': 1,
            'b': 2
        }
    )
    return result
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Mixed brackets with indentation should work: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Mixed brackets with indentation should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_single_line_file() {
    let source = "x = 1";
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Single line file should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Single line file should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_only_whitespace_file() {
    let source = "   \n\t\n    \n";
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Whitespace-only file should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Whitespace-only file should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_deep_nesting_10_levels() {
    let source = r#"
def foo(a, b, c, d, e, f, g, h, i, j):
  x = 0
  if a:
    if b:
      if c:
        if d:
          if e:
            if f:
              if g:
                if h:
                  if i:
                    if j:
                      x = 1
  return x
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Deep nesting should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Deep nesting should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_alternating_indent_dedent() {
    let source = r#"
def foo():
    if True:
        pass
    elif False:
        pass
    else:
        pass
    return None
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Alternating indent/dedent should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Alternating indent/dedent should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_indentation_with_backslash_continuation() {
    let source = r#"
def some_very_long_function_name(a, b):
    return a + b

def foo():
    argument1 = 1
    argument2 = 2
    result = some_very_long_function_name(argument1, argument2)
    return result
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Backslash continuation should work with indentation: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Backslash continuation should not produce warnings: {:?}",
        warnings
    );
}

#[test]
fn test_regression_original_test_case() {
    let source = "def foo():\n\tx = 1\n    y = 2"; // Mix of tab and spaces
    let (errors, warnings) = parse_and_get_diagnostics(source);

    assert!(
        has_error_code(&errors, "E2029"),
        "Should detect unindent mismatch error: {:?}",
        errors
    );

    assert!(
        !warnings.is_empty(),
        "Should produce indentation warnings: {:?}",
        warnings
    );
}

#[test]
fn test_regression_no_false_positives() {
    let source = r#"
def foo():
    x = 1
    if x > 0:
        return True
    else:
        return False

class Bar:
    def method(self):
        pass

# Module level code
result = foo()
"#;
    let (errors, warnings) = parse_and_get_diagnostics(source);
    assert!(
        errors.is_empty(),
        "Valid code should not produce errors: {:?}",
        errors
    );
    assert!(
        warnings.is_empty(),
        "Valid code should not produce warnings: {:?}",
        warnings
    );
}
