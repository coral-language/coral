//! Comprehensive integration tests for indentation edge cases.
//!
//! This test suite validates that the lexer correctly handles:
//! - Mixed tabs/spaces detection and warnings
//! - Inconsistent indentation warnings
//! - Better error messages for indentation issues
//! - EOF dedent scenarios with complex nesting

use coral_parser::{Arena, lexer::Lexer, parser::Parser};

/// Helper function to parse source and collect errors and warnings.
fn parse_and_get_diagnostics(source: &str) -> (Vec<String>, Vec<String>) {
    let arena = Arena::new();
    let lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    let errors = parser
        .errors()
        .iter()
        .map(|err| {
            let diagnostic = err.to_diagnostic(source);
            format!("{}: {}", err.code(), diagnostic.message)
        })
        .collect();

    let warnings = parser
        .warnings()
        .iter()
        .map(|warn| {
            let diagnostic = warn.to_diagnostic(source);
            format!("{}: {}", warn.code(), diagnostic.message)
        })
        .collect();

    (errors, warnings)
}

/// Helper function to tokenize source and show tokens.
fn tokenize_and_show(source: &str) -> Vec<String> {
    let mut lexer = Lexer::new(source);
    let (tokens, _errors, _warnings) = lexer.tokenize();

    tokens.iter().map(|token| format!("{:?}", token)).collect()
}

/// Helper to check if error is present with specific code.
fn has_error_code(errors: &[String], code: &str) -> bool {
    errors.iter().any(|e| e.starts_with(code))
}

/// Helper to check if warning is present with specific code.
fn has_warning_code(warnings: &[String], code: &str) -> bool {
    warnings.iter().any(|w| w.starts_with(code))
}

// ===== Valid Indentation Tests =====

#[test]
fn test_valid_indentation_spaces_4() {
    let source = r#"
def foo():
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
  if True:
    x = 1
  return x

if True:
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
    let source = "def foo():\n\tif True:\n\t\tx = 1\n\treturn x\n";
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
def complex_function():
    if condition1:
        for item in items:
            if condition2:
                try:
                    result = process(item)
                    if result:
                        return result
                except ValueError:
                    continue
                finally:
                    cleanup()
        else:
            handle_empty()
    elif condition3:
        match value:
            case 1:
                return "one"
            case 2:
                return "two"
            case _:
                return "other"
    else:
        return None
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

// ===== Mixed Tabs/Spaces Detection =====

#[test]
fn test_mixed_tabs_spaces_in_line() {
    // This test uses consistent indentation levels but mixed tabs/spaces within lines
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
    let tokens = tokenize_and_show(source);
    println!("Tokens: {:?}", tokens);
    let (errors, warnings) = parse_and_get_diagnostics(source);
    println!("Errors: {:?}", errors);
    println!("Warnings: {:?}", warnings);
    // This has consistent indentation structure (levels 0, 4, 8, 8) but inconsistent style
    // Should parse successfully but warn about style inconsistency
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
    // This has inconsistent indentation structure (levels 0, 8, 4) AND style inconsistency
    // The structure inconsistency should cause a parsing error
    assert!(
        has_error_code(&errors, "E2029"),
        "Should detect unindent mismatch error: {:?}",
        errors
    );
    // May also have style warnings
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
    // This has very inconsistent indentation structure, so should produce parsing errors
    assert!(
        !errors.is_empty(),
        "Complex inconsistent indentation should produce errors: {:?}",
        errors
    );

    // May also have style warnings
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

// ===== Inconsistent Indentation (Errors) =====

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
    let (errors, warnings) = parse_and_get_diagnostics(source);
    // This might be parsed differently, but should at least not crash
    println!("Irregular indent errors: {:?}", errors);
    println!("Irregular indent warnings: {:?}", warnings);
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
    // This should be valid Python - dedenting past multiple levels is allowed
    assert!(
        errors.is_empty(),
        "Dedenting past multiple levels should be valid: {:?}",
        errors
    );
}

// ===== EOF Dedent Scenarios =====

#[test]
fn test_eof_multiple_nested_blocks() {
    let source = r#"
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
        case 1:
            return "one"
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

// ===== Bracket Interaction =====

#[test]
fn test_indentation_inside_brackets_ignored() {
    let source = r#"
def foo():
    result = [
        item
        for item in items
        if condition(item)
    ]
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
def foo():
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

// ===== Edge Cases =====

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
if True:
  if True:
    if True:
      if True:
        if True:
          if True:
            if True:
              if True:
                if True:
                  if True:
                    x = 1
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
def foo():
    result = some_very_long_function_name( \
        argument1, \
        argument2 \
    )
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

// ===== Regression Tests =====

#[test]
fn test_regression_original_test_case() {
    // This was the original test case from test_errors.rs
    let source = "def foo():\n\tx = 1\n    y = 2"; // Mix of tab and spaces
    let (errors, warnings) = parse_and_get_diagnostics(source);

    // This has invalid indentation structure (levels 0, 8, 4), so should produce errors
    assert!(
        has_error_code(&errors, "E2029"),
        "Should detect unindent mismatch error: {:?}",
        errors
    );
    // May also produce style warnings
    assert!(
        !warnings.is_empty(),
        "Should produce indentation warnings: {:?}",
        warnings
    );
}

#[test]
fn test_regression_no_false_positives() {
    // Ensure valid code doesn't produce warnings
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
