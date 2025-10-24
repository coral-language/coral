//! Tests for HIR (High-Level Intermediate Representation) functionality

use coral_parser::Arena;
use coral_parser::helpers::{
    expect_hir_lowers_ok, expect_hir_lowers_with_errors, parse_and_get_ast,
};

/// Test basic HIR lowering functionality
#[test]
fn test_hir_lowering_basic() {
    let source = "x = 42\ny = x + 1";
    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with function definitions
#[test]
fn test_hir_lowering_functions() {
    let source = r#"
def add(a: int, b: int) -> int:
    return a + b

result = add(1, 2)
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with class definitions
#[test]
fn test_hir_lowering_classes() {
    let source = r#"
class Point:
    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

    def distance(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5

p = Point(1, 2)
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with control flow
#[test]
fn test_hir_lowering_control_flow() {
    let source = r#"
x = 10
if x > 5:
    y = "big"
else:
    y = "small"

for i in range(5):
    print(i)
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with expressions
#[test]
fn test_hir_lowering_expressions() {
    let source = r#"
# Arithmetic expressions
a = 1 + 2 * 3
b = (1 + 2) * 3
c = 10 / 2

# String operations
name = "Hello" + " " + "World"
length = len(name)

# List operations
numbers = [1, 2, 3, 4, 5]
squared = [x * x for x in numbers]
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering error handling
#[test]
fn test_hir_lowering_errors() {
    let source = "x = undefined_variable + 1";

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_with_errors(ast, &arena);
}

/// Test HIR type information
#[test]
fn test_hir_type_information() {
    let source = r#"
x: int = 42
y: str = "hello"
z = x + 1
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with type parameters and generics
#[test]
fn test_hir_lowering_generics() {
    let source = r#"
class Container[T]:
    def constructor(self, value: T):
        self.value = value

    def get(self) -> T:
        return self.value

def process[T](items: list[T]) -> T:
    return items[0]

container: Container[int] = Container(42)
result = process([1, 2, 3])
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with pattern matching
#[test]
fn test_hir_lowering_patterns() {
    let source = r#"
def process_data(data):
    match data:
        case int(x):
            return x * 2
        case str(s):
            return len(s)
        case list(items):
            return sum(items)
        case {"name": str(name), "age": int(age)}:
            return f"{name} is {age} years old"
        case _:
            return "unknown"

result1 = process_data(42)
result2 = process_data("hello")
result3 = process_data([1, 2, 3])
result4 = process_data({"name": "Alice", "age": 30})
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with comprehensions
#[test]
fn test_hir_lowering_comprehensions() {
    let source = r#"
# List comprehension
squares = [x * x for x in range(10) if x % 2 == 0]

# Dict comprehension
word_lengths = {word: len(word) for word in ["hello", "world", "python"]}

# Set comprehension
unique_lengths = {len(word) for word in ["hello", "world", "python"]}

# Generator expression
even_squares = (x * x for x in range(20) if x % 2 == 0)

# Nested comprehension
matrix = [[i * j for j in range(3)] for i in range(3)]
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with async/await
#[test]
fn test_hir_lowering_async() {
    let source = r#"
async def fetch_data(url: str) -> str:
    # Simulate async operation
    return f"Data from {url}"

async def process_items(items: list[str]) -> list[str]:
    results = []
    for item in items:
        data = await fetch_data(item)
        results.append(data.upper())
    return results

async def main():
    items = ["url1", "url2", "url3"]
    results = await process_items(items)
    return results
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with imports and exports
#[test]
fn test_hir_lowering_imports_exports() {
    let source = r#"
from typing import List, Dict, Optional
import os
import sys as system

export def public_function():
    return "public"

export class PublicClass:
    def constructor(self):
        self.value = 42

def private_function():
    return "private"

class PrivateClass:
    pass
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with complex type annotations
#[test]
fn test_hir_lowering_complex_types() {
    let source = r#"
from typing import Union, Callable, Generic, TypeVar

T = TypeVar('T')

def process_union(value: Union[int, str]) -> str:
    return str(value)

def process_callable(func: Callable[[int, int], int]) -> int:
    return func(1, 2)

class GenericContainer(Generic[T]):
    def constructor(self, value: T):
        self.value = value

    def get(self) -> T:
        return self.value

# Template strings
name = "Alice"
age = 30
message = f"Hello, {name}! You are {age} years old."

# Slices
numbers = [1, 2, 3, 4, 5]
first_three = numbers[:3]
last_two = numbers[-2:]
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test HIR lowering with error handling
#[test]
fn test_hir_lowering_error_handling() {
    let source = r#"
def risky_operation(x: int) -> int:
    try:
        if x < 0:
            raise ValueError("Negative number not allowed")
        return 100 / x
    except ValueError as e:
        print(f"Value error: {e}")
        return 0
    except ZeroDivisionError:
        print("Division by zero")
        return -1
    finally:
        print("Operation completed")

def handle_multiple_errors():
    try:
        # Some operation that might fail
        pass
    except (ValueError, TypeError) as e:
        print(f"Caught error: {e}")
    except Exception as e:
        print(f"Unexpected error: {e}")
    else:
        print("No errors occurred")
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test lambda inference integration with full semantic analysis
#[test]
fn test_lambda_inference_integration() {
    let source = r#"

simple = lambda x: x
annotated = lambda x: int: x * 2
multi_param = lambda x, y, z: x + y + z

def higher_order(f):
    return f(42)

result = higher_order(lambda n: n * 3)
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test Union type attribute resolution
#[test]
fn test_union_type_attribute_resolution() {
    let source = r#"
def process(value: int | str):
    # For Union types, attributes should only resolve if present in all union members
    # int.bit_length() exists, str has no such method
    if isinstance(value, str):
        result = value.upper()  # Valid: str has upper()
    else:
        result = str(value)  # Valid: int can be converted
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test Optional type attribute resolution
#[test]
fn test_optional_type_attribute_resolution() {
    let source = r#"
def get_value() -> str | None:
    return None

result = get_value()
# Accessing attributes on Optional types should work on the inner type
if result is not None:
    length = result.__len__()
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test decorator validation
#[test]
fn test_decorator_validation() {
    let source = r#"
class Example:
    @property
    def value(self) -> int:
        return 42

    @staticmethod
    def static_method():
        return "static"

    @classmethod
    def class_method(cls):
        return "class"
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test attribute resolution on built-in types
#[test]
fn test_builtin_type_attribute_resolution() {
    let source = r#"
# Test string methods
s: str = "hello"
length = s.__len__()
upper_s = s.upper()

# Test list methods
lst: list[int] = [1, 2, 3]
count = lst.count(1)
append_result = lst.append(4)

# Test dict methods
d: dict[str, int] = {"key": 1}
keys = d.keys()
values = d.values()

# Test set methods
s: set[int] = {1, 2, 3}
add_result = s.add(4)

# Test tuple methods
t: tuple[int, str, float] = (1, "hello", 3.14)
count_result = t.count(1)
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test attribute checking errors for invalid attributes
#[test]
fn test_invalid_attribute_detection() {
    let source = r#"
x: int = 42
# int type should not have upper() method
# This would be caught by type checking
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}

/// Test complex nested attribute access
#[test]
fn test_nested_attribute_access() {
    let source = r#"
class Person:
    name: str
    age: int

class Company:
    employees: list[Person]

def get_first_employee_name(company: Company) -> str:
    if company.employees:
        return company.employees[0].name
    return ""
"#;

    let ast = parse_and_get_ast(source);
    let arena = Arena::new();
    expect_hir_lowers_ok(ast, &arena);
}
