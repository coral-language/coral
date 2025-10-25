//! Integration tests for error reporting in Coral compiler.
//!
//! This test suite validates that all error kinds are properly detected,
//! reported with correct error codes, and provide helpful messages and suggestions.

use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_e1001_invalid_character() {
    let source = "x = 5 $ 10"; // $ is invalid character
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_e1002_unterminated_string() {
    let source = r#"
x = "unterminated string
y = 42
"#;
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_e1003_invalid_number() {
    let source = "x = 123.456.789"; // Invalid float
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_w1005_mixed_tabs_spaces() {
    let source = "def foo():\n\tx = 1\n    y = 2"; // Mix of tab and spaces
    DiagnosticTestBuilder::warnings(source)
        .expect("W1005")
        .assert_all();
}

#[test]
fn test_e2001_unexpected_token() {
    let source = "x = = 5"; // Unexpected = without left operand
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_e2003_unexpected_eof() {
    let source = "def foo("; // Unclosed parenthesis
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_e2006_unclosed_delimiter() {
    let source = r#"
x = [1, 2, 3
y = 42
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2006")
        .assert_all();
}

#[test]
fn test_e2007_unmatched_closing() {
    let source = "x = 1 + 2]"; // Extra closing bracket
    DiagnosticTestBuilder::errors(source)
        .expect("E2007")
        .assert_all();
}

#[test]
fn test_e2008_missing_colon() {
    let source = r#"
def foo()
    pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_e2011_break_outside_loop() {
    let source = r#"
def foo():
    break
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2011")
        .assert_all();
}

#[test]
fn test_e2012_continue_outside_loop() {
    let source = r#"
def foo():
    continue
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2012")
        .assert_all();
}

#[test]
fn test_e2013_return_outside_function() {
    let source = r#"
return 42
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2013")
        .assert_all();
}

#[test]
fn test_e2014_yield_outside_function() {
    let source = r#"
yield 42
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2014")
        .assert_all();
}

#[test]
fn test_e2015_await_outside_async() {
    let source = r#"
def foo():
    await something()
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2015")
        .assert_all();
}

#[test]
fn test_e2016_async_for_outside_async() {
    let source = r#"
def foo():
    async for item in items:
        pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2016")
        .assert_all();
}

#[test]
fn test_e2017_async_with_outside_async() {
    let source = r#"
def foo():
    async with resource:
        pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2017")
        .assert_all();
}

#[test]
fn test_e2018_duplicate_parameter() {
    let source = r#"
def foo(x, y, x):
    pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2018")
        .assert_all();
}

#[test]
fn test_e2019_duplicate_argument() {
    let source = r#"
def foo(x, y):
    pass

foo(x=1, x=2)
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2019")
        .assert_all();
}

#[test]
fn test_e2020_positional_after_keyword() {
    let source = r#"
def foo(x, y, z):
    pass

foo(x=1, 2, 3)
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2020")
        .assert_all();
}

#[test]
fn test_e2021_invalid_parameter_order() {
    let source = r#"
def foo(x=1, y):
    pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2021")
        .assert_all();
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
    DiagnosticTestBuilder::errors(source)
        .expect("E2022")
        .assert_all();
}

#[test]
fn test_e2023_bare_except_star() {
    let source = r#"
try:
    risky()
except*:
    pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2023")
        .assert_all();
}

#[test]
fn test_e2025_relative_import_beyond_top_level() {
    let source = "from ............ import something";
    DiagnosticTestBuilder::errors(source)
        .expect("E2025")
        .assert_all();
}

#[test]
fn test_nested_delimiters() {
    let source = r#"
x = [1, 2, (3, 4, {5: 6})]
y = ((1, 2), [3, 4])
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_async_function() {
    let source = r#"
# Simple async functions demonstrating various patterns

# Basic async function - returns Coroutine[int]
async def simple_async():
    x = 1
    return x

# Async function with parameters
async def async_with_params(name: str, value: int):
    return name

# Async function with type annotation - returns Coroutine[str]
async def async_typed() -> str:
    return "result"

# Async function with for loop
async def async_for_loop():
    for i in range(5):
        if i == 2:
            break
    return 42

# Async function with while loop
async def async_while_loop():
    x = 0
    while x < 10:
        x = x + 1
    return x

# Async function with if/elif/else
async def async_conditionals():
    x = 5
    if x > 10:
        return 1
    elif x > 0:
        return 2
    else:
        return 3

# Async function with try/except/finally
async def async_exception_handling():
    try:
        return 1
    except:
        return 2
    finally:
        pass

# Async function with nested function
async def async_with_nested():
    def helper():
        return 42
    return helper()

# Multiple async functions in same module
async def async_task_1():
    return 1

async def async_task_2():
    return 2

async def async_task_3():
    return 3

# Async function calling other async functions
async def async_caller():
    x = 1
    return x

# Comprehensive main async function using await
async def main():
    # Call async functions with await (mandatory)
    # Calling without await would return Coroutine[T] instead of T
    result = await simple_async()

    # Await with parameters
    result = await async_with_params("test", 5)

    # Await with type annotation
    result = await async_typed()

    # Await for loop version
    result = await async_for_loop()

    # Await while loop version
    result = await async_while_loop()

    # Await conditionals
    result = await async_conditionals()

    # Await exception handling
    result = await async_exception_handling()

    # Await nested function
    result = await async_with_nested()

    # Await individual tasks
    result = await async_task_1()
    result = await async_task_2()
    result = await async_task_3()

    # Await async caller
    result = await async_caller()

    return result
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_function_parameters() {
    let source = r#"
def foo(a, b, c=1, d=2, *args, e, f=3, **kwargs):
    pass
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_exception_handling() {
    let source = r#"
def risky(): pass
def handle_value_error(e): pass
def handle_type_error(): pass
def handle_any(): pass
def cleanup(): pass

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
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_exception_groups() {
    let source = r#"
def risky(): pass
def handle_value_errors(e): pass
def handle_type_errors(e): pass

try:
    risky()
except* ValueError as e:
    handle_value_errors(e)
except* TypeError as e:
    handle_type_errors(e)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_error_messages_have_suggestions() {
    use coral_parser::error::kinds::ErrorKind;

    let test_kinds = vec![
        ErrorKind::BreakOutsideLoop,
        ErrorKind::ReturnOutsideFunction,
        ErrorKind::DuplicateParameter {
            name: "test".to_string(),
        },
        ErrorKind::FutureImportNotFirst,
    ];

    for kind in test_kinds {
        let metadata = kind.metadata();
        let description = metadata.description;
        let suggestion = metadata.suggestion;

        assert!(
            !description.is_empty(),
            "Error {:?} should have description",
            metadata.code
        );
        assert!(
            suggestion.is_some(),
            "Error {:?} should have suggestion",
            metadata.code
        );

        let suggestion_text = suggestion.unwrap();
        assert!(
            !suggestion_text.is_empty(),
            "Error {:?} suggestion should not be empty",
            metadata.code
        );
    }
}

#[test]
fn test_error_code_formatting() {
    use coral_parser::error::codes::ErrorCode;

    assert_eq!(format!("{}", ErrorCode::E2011), "E2011");
    assert_eq!(format!("{}", ErrorCode::E2024), "E2024");
    assert_eq!(format!("{}", ErrorCode::E5001), "E5001");
}

#[test]
fn test_empty_fstring_expression() {
    let source = r#"x = f"Hello {}""#;
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_unmatched_brace_in_fstring() {
    let source = r#"x = f"Hello } world""#;
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_valid_fstring() {
    let source = r#"
name = "World"
x = f"Hello {name}!"
y = f"Result: {2 + 2}"
z = f"Format: {value:.2f}"
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_match_statement() {
    let source = r#"
value = 0

match value:
    case 0:
        print("zero")
    case 1:
        print("one")
    case _:
        print("other")
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_match_with_patterns() {
    let source = r#"
point = (1, 2)

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
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_type_alias() {
    let source = r#"
type Point = tuple[int, int]
type Vector = list[float]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_imports() {
    let source = r#"
import os
import sys as system
from pathlib import Path
from collections import defaultdict, Counter
from typing import *
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_relative_imports() {
    let source = r#"
from . import sibling
from .. import parent
from ...package import module
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_valid_future_imports() {
    let source = r#"
from __future__ import annotations
from __future__ import generator_stop

import os
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_errors_in_source() {
    let source = r#"
# Multiple errors to test error collection
def foo(x, x):  # E2018: Duplicate parameter
    return 42
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2018")
        .assert_all();
}

#[test]
fn test_error_recovery() {
    let source = r#"
def foo():
    x = [1, 2  # Unclosed bracket

def bar():
    y = 42  # This should still parse
"#;
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_recovery_missing_colon_function() {
    let source = r#"
def foo()
    return 42

def bar():
    return 24
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_if() {
    let source = r#"
if x > 5
    print("big")

print("done")
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_while() {
    let source = r#"
while x < 10
    x += 1

print("done")
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_for() {
    let source = r#"
for i in range(10)
    print(i)

print("done")
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_class() {
    let source = r#"
class Foo
    pass

class Bar:
    pass
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_try() {
    let source = r#"
try
    risky()
except Exception:
    handle()
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_except() {
    let source = r#"
try:
    risky()
except Exception
    handle()
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_with() {
    let source = r#"
with open("file.txt") as f
    data = f.read()

print(data)
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_missing_colon_match() {
    let source = r#"
match value
    case 1:
        print("one")
    case 2:
        print("two")
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_multiple_missing_colons() {
    let source = r#"
def foo()
    if x > 5
        return True
    return False

def bar():
    while y < 10
        y += 1
"#;
    DiagnosticTestBuilder::errors(source)
        .expect("E2008")
        .expect("E2008")
        .assert_all();
}

#[test]
fn test_recovery_sophisticated_sync() {
    let source = r#"
def broken(
    x = [1, 2, 3

def good():
    return 42

class MyClass:
    pass
"#;
    DiagnosticTestBuilder::errors(source).assert_some();
}

#[test]
fn test_soft_keyword_match_as_variable() {
    let source = r#"
match = 5
print(match)
match += 10
x = match * 2
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_case_as_variable() {
    let source = r#"
case = "test"
print(case)
case = case.upper()
x = len(case)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_type_as_variable() {
    let source = r#"
type = "string"
print(type)
type = int
x = type(42)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_match_statement() {
    let source = r#"
def foo(value):
    match value:
        case 1:
            print("one")
        case 2:
            print("two")
        case _:
            print("other")
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_type_statement() {
    let source = r#"
type Point = tuple[int, int]
type Vector = list[float]
type Matrix[T] = list[list[T]]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_mixed_context() {
    let source = r#"
match = 42  # match as variable

def check(value):
    match value:  # match as statement
        case 0:
            return "zero"
        case _:
            return "non-zero"

result = check(match)  # match as variable again
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_type_mixed_context() {
    let source = r#"
type MyInt = int  # type as statement

type = str  # type as variable
x = type("hello")  # type as variable

type Point = tuple[int, int]  # type as statement again
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_in_expressions() {
    let source = r#"
def foo():
    match = 5
    case = 10
    type = 15

    x = match + case + type
    return x
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_as_function_parameter() {
    let source = r#"
def foo(match, case, type):
    print(match, case, type)
    return match + case + type

result = foo(1, 2, 3)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_in_comprehension() {
    let source = r#"
match = [1, 2, 3, 4, 5]
result = [case for case in match if case > 2]
type = {x: x**2 for x in match}
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_case_outside_match() {
    let source = r#"
case = "uppercase"
test_case = case.upper()

def process_case(case):
    return case.lower()

result = process_case(test_case)
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_soft_keyword_with_augmented_assignment() {
    let source = r#"
match = 10
match += 5
match -= 2
match *= 3
match /= 2
match //= 1
match %= 4
match **= 2

type = 100
type &= 15
type |= 8
type ^= 3
type <<= 2
type >>= 1

case = "@"
case @= [[1, 2]]
"#;
    DiagnosticTestBuilder::errors(source).assert_none();
}
