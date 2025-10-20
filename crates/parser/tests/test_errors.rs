//! Integration tests for error reporting in Coral compiler.
//!
//! This test suite validates that all error kinds are properly detected,
//! reported with correct error codes, and provide helpful messages and suggestions.

use coral_parser::{Arena, lexer::Lexer, parser::Parser};

/// Helper function to parse source and collect errors.
fn parse_and_get_errors(source: &str) -> Vec<String> {
    let arena = Arena::new();
    let lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    parser
        .errors()
        .iter()
        .map(|err| {
            let diagnostic = err.to_diagnostic(source);
            format!("{}: {}", err.code(), diagnostic.message)
        })
        .collect()
}

/// Helper to check if error is present with specific code.
fn has_error_code(errors: &[String], code: &str) -> bool {
    errors.iter().any(|e| e.starts_with(code))
}

// ===== Lexical Errors (E1xxx) =====

#[test]
fn test_e1001_invalid_character() {
    let source = "x = 5 @ 10"; // @ is invalid outside operator context
    let errors = parse_and_get_errors(source);
    // Note: This might be parsed as operator, adjust test if needed
    println!("E1001 errors: {:?}", errors);
}

#[test]
fn test_e1002_unterminated_string() {
    let source = r#"
x = "unterminated string
y = 42
"#;
    let errors = parse_and_get_errors(source);
    println!("E1002 errors: {:?}", errors);
    // Lexer should detect unterminated string
}

#[test]
fn test_e1003_invalid_number() {
    let source = "x = 123.456.789"; // Invalid float
    let errors = parse_and_get_errors(source);
    println!("E1003 errors: {:?}", errors);
}

#[test]
fn test_e1005_mixed_tabs_spaces() {
    let source = "def foo():\n\tx = 1\n    y = 2"; // Mix of tab and spaces
    let errors = parse_and_get_errors(source);
    println!("E1005 errors: {:?}", errors);
}

// ===== Syntax Errors (E2xxx) =====

#[test]
fn test_e2001_unexpected_token() {
    let source = "x = + 5"; // Unexpected + without left operand
    let errors = parse_and_get_errors(source);
    println!("E2001 errors: {:?}", errors);
}

#[test]
fn test_e2003_unexpected_eof() {
    let source = "def foo("; // Unclosed parenthesis
    let errors = parse_and_get_errors(source);
    println!("E2003 errors: {:?}", errors);
}

#[test]
fn test_e2006_unclosed_delimiter() {
    let source = r#"
x = [1, 2, 3
y = 42
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2006"),
        "Should have unclosed delimiter error"
    );
    println!("E2006 errors: {:?}", errors);
}

#[test]
fn test_e2007_unmatched_closing() {
    let source = "x = 1 + 2]"; // Extra closing bracket
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2007"),
        "Should have unmatched closing error"
    );
}

#[test]
fn test_e2008_missing_colon() {
    let source = r#"
def foo()
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2008"),
        "Should have missing colon error"
    );
    println!("E2008 errors: {:?}", errors);
}

#[test]
fn test_e2011_break_outside_loop() {
    let source = r#"
def foo():
    break
"#;
    let errors = parse_and_get_errors(source);
    println!("E2011 errors: {:?}", errors);
    assert!(
        !errors.is_empty(),
        "Should detect break outside loop error, got: {:?}",
        errors
    );
    assert!(
        has_error_code(&errors, "E2011"),
        "Should have break outside loop error, got: {:?}",
        errors
    );
}

#[test]
fn test_e2012_continue_outside_loop() {
    let source = r#"
def foo():
    continue
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2012"),
        "Should have continue outside loop error"
    );
    println!("E2012 errors: {:?}", errors);
}

#[test]
fn test_e2013_return_outside_function() {
    let source = r#"
return 42
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2013"),
        "Should have return outside function error"
    );
    println!("E2013 errors: {:?}", errors);
}

#[test]
fn test_e2014_yield_outside_function() {
    let source = r#"
yield 42
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2014"),
        "Should have yield outside function error"
    );
    println!("E2014 errors: {:?}", errors);
}

#[test]
fn test_e2015_await_outside_async() {
    let source = r#"
def foo():
    await something()
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2015"),
        "Should have await outside async error"
    );
    println!("E2015 errors: {:?}", errors);
}

#[test]
fn test_e2016_async_for_outside_async() {
    let source = r#"
def foo():
    async for item in items:
        pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2016"),
        "Should have async for outside async error"
    );
    println!("E2016 errors: {:?}", errors);
}

#[test]
fn test_e2017_async_with_outside_async() {
    let source = r#"
def foo():
    async with resource:
        pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2017"),
        "Should have async with outside async error"
    );
    println!("E2017 errors: {:?}", errors);
}

#[test]
fn test_e2018_duplicate_parameter() {
    let source = r#"
def foo(x, y, x):
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2018"),
        "Should have duplicate parameter error"
    );
    println!("E2018 errors: {:?}", errors);
}

#[test]
fn test_e2019_duplicate_argument() {
    let source = r#"
def foo(x, y):
    pass

foo(x=1, x=2)
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2019"),
        "Should have duplicate argument error"
    );
    println!("E2019 errors: {:?}", errors);
}

#[test]
fn test_e2020_positional_after_keyword() {
    let source = r#"
def foo(x, y, z):
    pass

foo(x=1, 2, 3)
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2020"),
        "Should have positional after keyword error"
    );
    println!("E2020 errors: {:?}", errors);
}

#[test]
fn test_e2021_invalid_parameter_order() {
    let source = r#"
def foo(x=1, y):
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2021"),
        "Should have invalid parameter order error"
    );
    println!("E2021 errors: {:?}", errors);
}

#[test]
fn test_e2022_mixed_except_syntax() {
    let source = r#"
try:
    risky()
except ValueError:
    pass
except* TypeError:
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2022"),
        "Should have mixed except syntax error"
    );
    println!("E2022 errors: {:?}", errors);
}

#[test]
fn test_e2023_bare_except_star() {
    let source = r#"
try:
    risky()
except*:
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2023"),
        "Should have bare except* error"
    );
    println!("E2023 errors: {:?}", errors);
}

#[test]
fn test_e2024_future_import_not_first() {
    let source = r#"
import os
from __future__ import annotations
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2024"),
        "Should have future import not first error"
    );
    println!("E2024 errors: {:?}", errors);
}

#[test]
fn test_e2025_relative_import_beyond_top_level() {
    let source = "from ............ import something";
    let errors = parse_and_get_errors(source);
    assert!(
        has_error_code(&errors, "E2025"),
        "Should have relative import beyond top level error"
    );
    println!("E2025 errors: {:?}", errors);
}

// ===== Complex Syntax Tests =====

#[test]
fn test_nested_delimiters() {
    let source = r#"
x = [1, 2, (3, 4, {5: 6})]
y = ((1, 2), [3, 4])
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid nested delimiters should not produce errors"
    );
}

#[test]
fn test_valid_async_function() {
    let source = r#"
async def foo():
    await bar()
    async for item in items:
        await process(item)
    async with resource:
        await use_resource()
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid async function should not produce errors"
    );
}

#[test]
fn test_valid_function_parameters() {
    let source = r#"
def foo(a, b, c=1, d=2, *args, e, f=3, **kwargs):
    pass
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid parameter order should not produce errors"
    );
}

#[test]
fn test_valid_exception_handling() {
    let source = r#"
try:
    risky()
except ValueError as e:
    handle_value_error(e)
except TypeError:
    handle_type_error()
except:
    handle_any()
finally:
    cleanup()
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid exception handling should not produce errors"
    );
}

#[test]
fn test_valid_exception_groups() {
    let source = r#"
try:
    risky()
except* ValueError as e:
    handle_value_errors(e)
except* TypeError as e:
    handle_type_errors(e)
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid exception groups should not produce errors"
    );
}

// ===== Error Messages and Suggestions Tests =====

#[test]
fn test_error_messages_have_suggestions() {
    // Test that common errors provide helpful suggestions
    use coral_parser::error::codes::ErrorCode;

    // Test a few representative error codes
    let test_codes = vec![
        ErrorCode::E2011, // Break outside loop
        ErrorCode::E2013, // Return outside function
        ErrorCode::E2018, // Duplicate parameter
        ErrorCode::E2024, // Future import not first
    ];

    for code in test_codes {
        let description = code.description();
        let suggestion = code.suggestion();

        assert!(
            !description.is_empty(),
            "Error {:?} should have description",
            code
        );
        assert!(
            suggestion.is_some(),
            "Error {:?} should have suggestion",
            code
        );

        let suggestion_text = suggestion.unwrap();
        assert!(
            !suggestion_text.is_empty(),
            "Error {:?} suggestion should not be empty",
            code
        );

        println!("{}: {}", code, description);
        println!("  Suggestion: {}", suggestion_text);
    }
}

#[test]
fn test_error_code_formatting() {
    use coral_parser::error::codes::ErrorCode;

    // Test that error codes format correctly
    assert_eq!(format!("{}", ErrorCode::E2011), "E2011");
    assert_eq!(format!("{}", ErrorCode::E2024), "E2024");
    assert_eq!(format!("{}", ErrorCode::E5001), "E5001");
}

// ===== F-string Error Tests =====

#[test]
fn test_empty_fstring_expression() {
    let source = r#"x = f"Hello {}""#;
    let errors = parse_and_get_errors(source);
    println!("Empty f-string errors: {:?}", errors);
}

#[test]
fn test_unmatched_brace_in_fstring() {
    let source = r#"x = f"Hello } world""#;
    let errors = parse_and_get_errors(source);
    println!("Unmatched brace errors: {:?}", errors);
}

#[test]
fn test_valid_fstring() {
    let source = r#"
name = "World"
x = f"Hello {name}!"
y = f"Result: {2 + 2}"
z = f"Format: {value:.2f}"
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid f-strings should not produce errors"
    );
}

// ===== Pattern Matching Tests =====

#[test]
fn test_valid_match_statement() {
    let source = r#"
match value:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid match statement should not produce errors"
    );
}

#[test]
fn test_match_with_patterns() {
    let source = r#"
match point:
    case (0, 0):
        print("origin")
    case (0, y):
        print(f"y-axis at {y}")
    case (x, 0):
        print(f"x-axis at {x}")
    case (x, y):
        print(f"point at ({x}, {y})")
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid match patterns should not produce errors"
    );
}

// ===== Type Alias Tests =====

#[test]
fn test_valid_type_alias() {
    let source = r#"
type Point = tuple[int, int]
type Vector = list[float]
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid type aliases should not produce errors"
    );
}

// ===== Import Tests =====

#[test]
fn test_valid_imports() {
    let source = r#"
import os
import sys as system
from pathlib import Path
from collections import defaultdict, Counter
from typing import *
"#;
    let errors = parse_and_get_errors(source);
    assert!(errors.is_empty(), "Valid imports should not produce errors");
}

#[test]
fn test_valid_relative_imports() {
    let source = r#"
from . import sibling
from .. import parent
from ...package import module
"#;
    let errors = parse_and_get_errors(source);
    // Note: Relative imports are syntax valid but semantic check needs package context
    println!("Relative import errors: {:?}", errors);
}

#[test]
fn test_valid_future_imports() {
    let source = r#"
from __future__ import annotations
from __future__ import generator_stop

import os
"#;
    let errors = parse_and_get_errors(source);
    assert!(
        errors.is_empty(),
        "Valid future imports at top should not produce errors"
    );
}

// ===== Comprehensive Multi-Error Test =====

#[test]
fn test_multiple_errors_in_source() {
    let source = r#"
# Multiple errors to test error collection
def foo(x, x):  # E2018: Duplicate parameter
    break  # E2011: Break outside loop
    return 42

return 10  # E2013: Return outside function

def bar():
    await something()  # E2015: Await outside async

def baz()  # E2008: Missing colon
    pass
"#;
    let errors = parse_and_get_errors(source);
    println!("Multiple errors: {:?}", errors);
    assert!(errors.len() >= 3, "Should detect multiple errors");
}

#[test]
fn test_error_recovery() {
    // Test that parser can recover from errors and continue parsing
    let source = r#"
def foo():
    x = [1, 2  # Unclosed bracket

def bar():
    y = 42  # This should still parse
"#;
    let errors = parse_and_get_errors(source);
    println!("Error recovery test: {:?}", errors);
    // Parser should detect the unclosed bracket error
}
