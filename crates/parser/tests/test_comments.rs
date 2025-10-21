//! Integration tests for comment and docstring parsing in Coral.
//!
//! This test suite validates that:
//! - Single-line comments (#) are properly preserved for documentation and IDE support
//! - Docstrings are correctly extracted from modules, functions, and classes
//! - Comments don't affect parsing or AST structure
//! - Comment classification (line vs trailing) works correctly
//! - Only single-line docstrings are supported (Coral doesn't have multi-line strings yet)

use coral_parser::{
    Arena, Module,
    lexer::{CommentMap, Lexer, TokenKind},
    parser::Parser,
};

/// Helper function to tokenize source and return token kinds.
fn tokenize(source: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(source);
    let (tokens, errors, _warnings) = lexer.tokenize();

    // Ensure no lexical errors
    assert!(
        errors.is_empty(),
        "Expected no lexical errors, but got: {:?}",
        errors
    );

    tokens
        .iter()
        .map(|t| t.kind)
        .filter(|k| !matches!(k, TokenKind::Newline | TokenKind::Eof))
        .collect()
}

/// Helper function to parse source and return the module and comment map.
fn parse_with_comments(source: &str) -> (&'static Module<'static>, CommentMap) {
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, arena);
    let module = parser.parse_module().unwrap();

    let errors = parser.errors();
    assert!(
        errors.is_empty(),
        "Expected no parse errors, but got:\n{}",
        errors
            .iter()
            .map(|e| {
                let diag = e.to_diagnostic(source);
                format!("{}: {}", e.code(), diag.message)
            })
            .collect::<Vec<_>>()
            .join("\n")
    );

    (module, parser.comment_map.clone())
}

/// Helper to get comment map from parsed source.
fn get_comments(source: &str) -> CommentMap {
    let (_module, comment_map) = parse_with_comments(source);
    comment_map
}

// ===== Comment Tokenization Tests =====

#[test]
fn test_single_line_comment_tokenization() {
    let source = r#"
x = 1
# This is a comment
y = 2
"#;
    let tokens = tokenize(source);
    assert!(tokens.contains(&TokenKind::Comment));
}

#[test]
fn test_comment_with_special_characters() {
    let source = r#"
# Comment with @#$%^&*() symbols!
x = 1
# Unicode: 你好世界 🌍
y = 2
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 2);
}

#[test]
fn test_empty_comment() {
    let source = r#"
x = 1
#
y = 2
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 1);
    assert_eq!(comments.comments()[0].text, "");
}

#[test]
fn test_comment_at_end_of_file() {
    let source = r#"
x = 1
# Final comment"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 1);
}

#[test]
fn test_multiple_comments_on_same_line() {
    // This shouldn't happen in valid Coral code, but let's test robustness
    let source = r#"
x = 1 # Comment 1
y = 2 # Comment 2
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 2);
}

// ===== Comment Classification Tests =====

#[test]
fn test_line_comments() {
    let source = r#"
# This is a line comment
x = 1
    # This is an indented line comment
y = 2
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 2);
    for comment in comments.comments() {
        assert_eq!(comment.kind, coral_parser::lexer::CommentKind::Line);
    }
}

#[test]
fn test_trailing_comments() {
    let source = r#"
x = 1  # This is a trailing comment
y = 2  # Another trailing comment
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 2);
    for comment in comments.comments() {
        assert_eq!(comment.kind, coral_parser::lexer::CommentKind::Trailing);
    }
}

#[test]
fn test_mixed_comment_types() {
    let source = r#"
# Line comment
x = 1  # Trailing comment
# Another line comment
y = 2
"#;
    let comments = get_comments(source);
    assert_eq!(comments.len(), 3);

    let line_comments: Vec<_> = comments
        .comments()
        .iter()
        .filter(|c| c.kind == coral_parser::lexer::CommentKind::Line)
        .collect();
    let trailing_comments: Vec<_> = comments
        .comments()
        .iter()
        .filter(|c| c.kind == coral_parser::lexer::CommentKind::Trailing)
        .collect();

    assert_eq!(line_comments.len(), 2);
    assert_eq!(trailing_comments.len(), 1);
}

// ===== Comment Content Extraction Tests =====

#[test]
fn test_comment_text_extraction() {
    let source = r#"
# This is a comment
x = 1  # Trailing comment here
#Another comment without space
"#;
    let comments = get_comments(source);

    let texts: Vec<&str> = comments
        .comments()
        .iter()
        .map(|c| c.text.as_str())
        .collect();

    assert!(texts.contains(&"This is a comment"));
    assert!(texts.contains(&"Trailing comment here"));
    assert!(texts.contains(&"Another comment without space"));
}

#[test]
fn test_comment_with_leading_trailing_whitespace() {
    let source = r#"
#   Comment with leading spaces
x = 1  #   Trailing with spaces
"#;
    let comments = get_comments(source);

    let texts: Vec<&str> = comments
        .comments()
        .iter()
        .map(|c| c.text.as_str())
        .collect();

    assert!(texts.contains(&"Comment with leading spaces"));
    assert!(texts.contains(&"Trailing with spaces"));
}

// ===== Module Docstring Tests =====

#[test]
fn test_module_docstring() {
    let source = r#"
"This is the module docstring"
x = 1
y = 2
"#;
    let (module, _comments) = parse_with_comments(source);
    assert_eq!(module.docstring, Some("This is the module docstring"));
}

#[test]
fn test_module_docstring_single_quotes() {
    let source = r#"
'Single quoted module docstring'
x = 1
"#;
    let (module, _comments) = parse_with_comments(source);
    assert_eq!(module.docstring, Some("Single quoted module docstring"));
}

#[test]
fn test_module_without_docstring() {
    let source = r#"
x = 1
y = 2
"#;
    let (module, _comments) = parse_with_comments(source);
    assert_eq!(module.docstring, None);
}

#[test]
fn test_module_docstring_not_first_statement() {
    let source = r#"
import os
"This is not a docstring"
x = 1
"#;
    let (module, _comments) = parse_with_comments(source);
    assert_eq!(module.docstring, None);
}

#[test]
fn test_module_docstring_after_comment() {
    let source = r#"
# Module comment
"This is the docstring"
x = 1
"#;
    let (module, _comments) = parse_with_comments(source);
    assert_eq!(module.docstring, Some("This is the docstring"));
}

// ===== Function Docstring Tests =====

#[test]
fn test_function_docstring() {
    let source = r#"
def func():
    "Function docstring"
    return 42
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, Some("Function docstring"));
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_without_docstring() {
    let source = r#"
def func():
    return 42
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, None);
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_function_docstring_not_first_in_body() {
    let source = r#"
def func():
    x = 1
    "This is not a docstring"
    return x
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, None);
        }
        _ => panic!("Expected function definition"),
    }
}

// ===== Class Docstring Tests =====

#[test]
fn test_class_docstring() {
    let source = r#"
class MyClass:
    "Class docstring"
    def method(self):
        pass
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::ClassDef(class_def) => {
            assert_eq!(class_def.docstring, Some("Class docstring"));
        }
        _ => panic!("Expected class definition"),
    }
}

#[test]
fn test_class_without_docstring() {
    let source = r#"
class MyClass:
    def method(self):
        pass
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::ClassDef(class_def) => {
            assert_eq!(class_def.docstring, None);
        }
        _ => panic!("Expected class definition"),
    }
}

// ===== Complex Scenarios =====

#[test]
fn test_comments_and_docstrings_together() {
    let source = r#"
# Module comment
"Module docstring"
# Another comment

def func():
    # Function comment
    "Function docstring"
    return 42

class MyClass:
    # Class comment
    "Class docstring"
    def method(self):
        # Method comment
        return 1
"#;
    let (module, comments) = parse_with_comments(source);

    // Check module docstring
    assert_eq!(module.docstring, Some("Module docstring"));

    // Check function docstring
    match &module.body[1] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, Some("Function docstring"));
        }
        _ => panic!("Expected function definition"),
    }

    // Check class docstring
    match &module.body[2] {
        coral_parser::Stmt::ClassDef(class_def) => {
            assert_eq!(class_def.docstring, Some("Class docstring"));
        }
        _ => panic!("Expected class definition"),
    }

    // Check comments are preserved
    assert_eq!(comments.len(), 5); // 5 comments total
}

#[test]
fn test_comments_in_nested_structures() {
    let source = r#"
def outer():
    # Outer function comment
    "Outer docstring"

    def inner():
        # Inner function comment
        "Inner docstring"
        return 1

    class InnerClass:
        # Inner class comment
        "Inner class docstring"
        pass

    return inner
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::FuncDef(outer_func) => {
            assert_eq!(outer_func.docstring, Some("Outer docstring"));

            // Check inner function
            match &outer_func.body[1] {
                coral_parser::Stmt::FuncDef(inner_func) => {
                    assert_eq!(inner_func.docstring, Some("Inner docstring"));
                }
                _ => panic!("Expected inner function"),
            }

            // Check inner class
            match &outer_func.body[2] {
                coral_parser::Stmt::ClassDef(inner_class) => {
                    assert_eq!(inner_class.docstring, Some("Inner class docstring"));
                }
                _ => panic!("Expected inner class"),
            }
        }
        _ => panic!("Expected outer function"),
    }
}

#[test]
fn test_docstring_with_quotes() {
    let source = r#"
def func():
    'Docstring with "double quotes"'
    return 42

class MyClass:
    "Docstring with 'single quotes'"
    pass
"#;
    let (module, _comments) = parse_with_comments(source);

    match &module.body[0] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(
                func_def.docstring,
                Some(r#"Docstring with "double quotes""#)
            );
        }
        _ => panic!("Expected function definition"),
    }

    match &module.body[1] {
        coral_parser::Stmt::ClassDef(class_def) => {
            assert_eq!(class_def.docstring, Some("Docstring with 'single quotes'"));
        }
        _ => panic!("Expected class definition"),
    }
}

// ===== Edge Cases =====

#[test]
fn test_empty_docstrings() {
    let source = r#"
""
def func():
    ''
    return 42
"#;
    let (module, _comments) = parse_with_comments(source);

    assert_eq!(module.docstring, Some(""));

    match &module.body[1] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, Some(""));
        }
        _ => panic!("Expected function definition"),
    }
}

#[test]
fn test_docstring_with_escapes() {
    let source = r#"
"Docstring with \"quotes\" and \n newline"
def func():
    'Docstring with \'quotes\''
    return 42
"#;
    let (module, _comments) = parse_with_comments(source);

    assert_eq!(
        module.docstring,
        Some("Docstring with \"quotes\" and \n newline")
    );

    match &module.body[1] {
        coral_parser::Stmt::FuncDef(func_def) => {
            assert_eq!(func_def.docstring, Some("Docstring with 'quotes'"));
        }
        _ => panic!("Expected function definition"),
    }
}

// ===== Performance and Correctness =====

#[test]
fn test_comment_map_not_empty_when_comments_present() {
    let source = r#"
# Comment
x = 1
"#;
    let comments = get_comments(source);
    assert!(!comments.is_empty());
}

#[test]
fn test_comment_map_empty_when_no_comments() {
    let source = r#"
x = 1
y = 2
"#;
    let comments = get_comments(source);
    assert!(comments.is_empty());
}

#[test]
fn test_large_file_with_many_comments() {
    let mut source = String::from("# Start\n");
    for i in 0..50 {
        source.push_str(&format!("x{} = {}\n# Comment {}\n", i, i, i));
    }
    source.push_str("# End\n");

    let comments = get_comments(&source);
    assert_eq!(comments.len(), 52); // 1 start + 50 loop comments + 1 end
}

#[test]
fn test_comments_dont_affect_ast_structure() {
    let source_without_comments = r#"
def func():
    return 42

class MyClass:
    pass
"#;

    let source_with_comments = r#"
# Module comment
def func():
    # Function comment
    return 42

class MyClass:
    # Class comment
    pass
"#;

    let arena1 = Arena::new();
    let lexer1 = Lexer::new(source_without_comments);
    let mut parser1 = Parser::new(lexer1, &arena1);
    let module1 = parser1.parse_module().unwrap();

    let arena2 = Arena::new();
    let lexer2 = Lexer::new(source_with_comments);
    let mut parser2 = Parser::new(lexer2, &arena2);
    let module2 = parser2.parse_module().unwrap();

    // AST structure should be the same (ignoring docstrings and comments)
    assert_eq!(module1.body.len(), module2.body.len());
}
