//! Tests for constructor keyword functionality

use coral_parser::{Arena, Lexer, Parser};

#[test]
fn test_constructor_basic() {
    let source = r#"
class Point:
    def constructor(self, x, y):
        self.x = x
        self.y = y
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(
        result.is_ok(),
        "Constructor keyword should parse successfully"
    );
}

#[test]
fn test_constructor_with_types() {
    let source = r#"
class Rectangle:
    width: float
    height: float

    def constructor(self, width: float, height: float):
        self.width = width
        self.height = height
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(result.is_ok(), "Constructor with types should parse");
}

#[test]
fn test_constructor_no_params() {
    let source = r#"
class Empty:
    def constructor(self):
        pass
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(result.is_ok(), "Constructor with no params should parse");
}

#[test]
fn test_constructor_with_defaults() {
    let source = r#"
class Config:
    def constructor(self, timeout: int = 30, retries: int = 3):
        self.timeout = timeout
        self.retries = retries
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(result.is_ok(), "Constructor with defaults should parse");
}

#[test]
fn test_constructor_with_docstring() {
    let source = r#"
class Point:
    def constructor(self, x, y):
        "Initialize a Point with x and y coordinates"
        self.x = x
        self.y = y
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(result.is_ok(), "Constructor with docstring should parse");
}

#[test]
fn test_constructor_multiple_statements() {
    let source = r#"
class Complex:
    def constructor(self, real, imag):
        self.real = real
        self.imag = imag
        self.magnitude = (real * real + imag * imag) ** 0.5
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(
        result.is_ok(),
        "Constructor with multiple statements should parse"
    );
}

#[test]
fn test_class_without_constructor() {
    let source = r#"
class Simple:
    x: int
    y: int
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(
        result.is_ok(),
        "Class without explicit constructor should parse"
    );
}

#[test]
fn test_constructor_with_other_methods() {
    let source = r#"
class Vector:
    def constructor(self, x, y):
        self.x = x
        self.y = y

    def magnitude(self):
        return (self.x * self.x + self.y * self.y) ** 0.5

    def normalize(self):
        mag = self.magnitude()
        self.x = self.x / mag
        self.y = self.y / mag
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(
        result.is_ok(),
        "Constructor with other methods should parse"
    );
}

#[test]
fn test_nested_class_constructors() {
    let source = r#"
class Outer:
    def constructor(self, value):
        self.value = value

        class Inner:
            def constructor(self, inner_value):
                self.inner_value = inner_value
"#;

    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);

    let result = parser.parse_module();
    assert!(result.is_ok(), "Nested class constructors should parse");
}
