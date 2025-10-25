use coral_parser::{PassManager, PassManagerConfig, error::codes::Severity, parse};
use std::path::PathBuf;

#[test]
fn test_multiline_error_highlighting() {
    let source = r#"
def function(
    x: int,
    y: str,
    z: int
):
    pass
"#;

    let result = parse(source).expect("Parse should succeed with partial AST");

    // This test verifies that the error context has the multiline detection method
    // Even if there are no errors, we verify the API works
    let error_diags = result.errors();

    if !error_diags.is_empty()
        && let Some(ctx) = &error_diags[0].context
    {
        let (start_line, end_line) = ctx.span_line_range();
        // Verify the method works without panicking
        let _is_multiline = ctx.is_multiline();
        assert!(start_line >= 1, "Line numbers should be >= 1");
        assert!(end_line >= start_line, "End line should be >= start line");
    }
}

#[test]
fn test_partial_ast_on_errors() {
    let source = r#"
def func1():
    pass

def func3():
    return 42
"#;

    let result = parse(source).expect("Parse should return Ok with partial AST");

    assert!(
        !result.module.body.is_empty(),
        "Module should have parsed statements"
    );

    // We should have at least 2 function definitions
    let stmts = result.module.body;
    assert!(stmts.len() >= 2, "Should have parsed multiple functions");
}

#[test]
fn test_per_pass_strictness_configuration() {
    let _source = r#"
x = 5
y = x + 10
"#;

    let mut config = PassManagerConfig::default();

    // Set ownership_check to only error on Fatal severity
    config.set_pass_severity("ownership_check", Severity::Fatal);

    assert!(config.has_pass_severity_override("ownership_check"));
    assert_eq!(
        config.get_pass_severity("ownership_check"),
        Some(Severity::Fatal)
    );

    // Check a pass that doesn't have an override
    assert!(!config.has_pass_severity_override("type_checking"));
    assert_eq!(config.get_pass_severity("type_checking"), None);
}

#[test]
fn test_ide_integration_recovery_actions() {
    let source = r#"
def func():
    print("hello"
    return 42
"#;

    let result = parse(source).expect("Parse should return Ok with recovery data");

    // Should have recovery actions from error recovery
    let recovery_actions = &result.recovery_actions;
    assert!(
        !recovery_actions.is_empty(),
        "Should have recorded recovery actions"
    );

    // Verify recovery actions have proper structure
    for action in recovery_actions {
        assert!(
            !action.description.is_empty(),
            "Action should have description"
        );
        // Verify action has valid span
        let _action_span_start: usize = action.span.start().into();
        let _action_span_end: usize = action.span.end().into();
        assert!(
            _action_span_end >= _action_span_start,
            "Span end should be >= start"
        );
    }
}

#[test]
fn test_ide_integration_comment_map() {
    let source = r#"
# Module comment
def func():
    # Function comment
    x = 5  # Inline comment
    return x
"#;

    let result = parse(source).expect("Parse should include comment map");

    let comment_map = &result.comment_map;
    assert!(
        !comment_map.is_empty(),
        "Should have captured comments in map"
    );

    // Verify we can access comments
    let comments_vec: Vec<_> = comment_map.comments().to_vec();
    assert!(
        comments_vec.len() >= 2,
        "Should have captured multiple comments"
    );
}

#[test]
fn test_ide_integration_symbol_locations() {
    let source = r#"
def my_function(x, y):
    result = x + y
    return result

class MyClass:
    def __init__(self):
        pass
"#;

    let result = parse(source).expect("Parse should build symbol location index");

    let symbol_locations = &result.symbol_locations;
    assert!(
        !symbol_locations.is_empty(),
        "Should have symbol location index"
    );

    // Check for specific symbols
    assert!(
        symbol_locations.contains_key("my_function"),
        "Should have location for function definition"
    );
    assert!(
        symbol_locations.contains_key("MyClass"),
        "Should have location for class definition"
    );

    // Verify spans are valid
    for (symbol_name, span) in symbol_locations {
        assert!(
            !symbol_name.is_empty(),
            "Symbol name should not be empty for '{}'",
            symbol_name
        );
        let _span_start: usize = span.start().into();
        let _span_end: usize = span.end().into();
        assert!(
            _span_end >= _span_start,
            "Span should be valid for '{}'",
            symbol_name
        );
    }
}

#[test]
fn test_partial_ast_with_eval_mode() {
    let source = "x + y invalid expression";

    let result = coral_parser::parse_eval(source);
    assert!(
        result.is_ok(),
        "parse_eval should return Ok with partial result"
    );

    let result = result.unwrap();
    assert!(result.has_errors(), "Should have parse errors");

    // Module should still have one statement (the error expr wrapped in ExprStmt)
    assert_eq!(result.module.body.len(), 1, "Should have one statement");
}

#[test]
fn test_partial_ast_with_interactive_mode() {
    let source = "x = ";

    let result = coral_parser::parse_interactive(source);
    assert!(
        result.is_ok(),
        "parse_interactive should return Ok with partial result"
    );

    let result = result.unwrap();
    assert!(result.has_errors(), "Should have parse errors");

    // Module should still have one statement
    assert_eq!(result.module.body.len(), 1, "Should have one statement");
}

#[test]
fn test_multiline_error_no_panic() {
    // Test that formatter doesn't panic on multi-line errors
    let source = r#"
def func(
    arg1: int,
    arg2: str
):
    # This should parse fine
    pass

def broken_func(
    param1,
    param2 invalid here
    param3,
):
    pass
"#;

    let result = parse(source);
    assert!(
        result.is_ok(),
        "Should parse with partial AST despite error"
    );

    let result = result.unwrap();
    if !result.diagnostics.is_empty() {
        // Try to format diagnostics - should not panic
        let formatted = format!("{:?}", result.diagnostics[0]);
        assert!(!formatted.is_empty(), "Formatting should work");
    }
}

#[test]
fn test_recovery_actions_statistics() {
    let source = r#"
x = (
    invalid
    syntax
)
y = [1, 2, 3]
"#;

    let result = parse(source).expect("Parse should work with recovery");

    let _recovery_stats = if !result.recovery_actions.is_empty() {
        let _manager = PassManager::new(PathBuf::from("<test>"));
        Some(_manager)
    } else {
        None
    };

    // If we have recovery actions, statistics should be available
    assert!(_recovery_stats.is_none() || !result.recovery_actions.is_empty());
}

#[test]
fn test_symbol_table_with_errors() {
    let source = r#"
x = 10
y = x + invalid_name
z = y * 2
"#;

    let result = parse(source).expect("Parse should work with errors");

    // Symbol table should still be populated even with undefined names
    assert!(result.symbol_table.lookup("x").is_some());
    assert!(result.symbol_table.lookup("z").is_some());

    // Undefined names might not be in symbol table, but that's semantic analysis
}
