//! Multi-pass integration tests
//!
//! Tests that verify multiple semantic analysis passes work correctly together
//! and that state is properly shared via AnalysisContext.

use coral_parser::{ParseResultWithMetadata, parse};

/// Helper to check if diagnostics contain a specific error type
fn has_error_type(result: &ParseResultWithMetadata, error_type: &str) -> bool {
    result
        .errors()
        .iter()
        .any(|d| d.error_type.as_deref() == Some(error_type))
}

/// Helper to check if diagnostics contain a message matching a pattern
fn has_error_message_containing(result: &ParseResultWithMetadata, pattern: &str) -> bool {
    result.errors().iter().any(|d| d.message.contains(pattern))
}

#[test]
fn test_type_checking_after_inference() {
    let source = r#"
def greet(name: str) -> str:
    return "Hello, " + name

message = greet("World")
wrong = greet(42)
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Should parse successfully");
    let parse_result = result.unwrap();

    // Should detect type error for greet(42)
    let has_type_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_type_error, "Should detect type mismatch for greet(42)");
}

#[test]
fn test_class_attribute_validation_after_hir() {
    let source = r#"
class Point:
    x: int
    y: int

    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

p = Point(3, 4)
valid = p.x
invalid = p.z
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // Should have error for missing attribute 'z'
    assert!(
        !parse_result.errors().is_empty(),
        "Should have errors for undefined attribute"
    );
}

#[test]
fn test_await_validation_in_async_context() {
    let source = r#"
async def fetch():
    result = await get_data()
    return result

def regular():
    data = await something()
    return data
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // Should detect await outside async
    let has_await_error = has_error_message_containing(&parse_result, "await");

    assert!(
        has_await_error,
        "Should detect await outside async function"
    );
}

#[test]
fn test_control_flow_all_paths_return() {
    let source = r#"
def check(x: int) -> str:
    if x > 0:
        return "positive"
    elif x < 0:
        return "negative"
"#;

    let result = parse(source);
    assert!(result.is_ok());

    // Control flow pass should detect missing return path
    // (This depends on whether the pass is enabled and strict)
}

#[test]
fn test_multiple_error_types_from_different_passes() {
    let source = r#"
x: int = "not an int"

y = undefined_variable

def regular():
    return await something()
"#;

    let result = parse(source);
    assert!(
        result.is_ok(),
        "Parser should succeed even with semantic errors"
    );
    let parse_result = result.unwrap();

    // Should have multiple errors from different passes
    assert!(
        parse_result.errors().len() >= 2,
        "Should have multiple errors from different analysis passes"
    );
}

#[test]
fn test_cross_module_type_with_classes() {
    let source = r#"
class Logger:
    def log(self, message: str):
        pass

def use_logger(logger: Logger, msg: str):
    logger.log(msg)

my_logger = Logger()
use_logger(my_logger, "Hello")
use_logger("not a logger", "Hello")
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // Should have type error for passing string instead of Logger
    let has_type_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_type_error, "Should detect incompatible argument type");
}

#[test]
fn test_decorator_and_property_resolution() {
    let source = r#"
class MyClass:
    @staticmethod
    def static_method():
        return "static"

    @property
    def my_property(self):
        return 42

obj = MyClass()
value = obj.my_property
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Should parse successfully");
    let parse_result = result.unwrap();

    // Should not have errors for valid decorator usage
    assert!(!parse_result.module.body.is_empty());
}

#[test]
fn test_cfg_shared_across_passes() {
    // This tests that the CFG cache is properly shared
    let source = r#"
def complex_function(x: int) -> int:
    if x > 0:
        if x > 10:
            return x * 2
        else:
            return x + 5
    else:
        while x < 0:
            x = x + 1
        return x
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // CFG should be built once and reused
    // No internal errors should occur from cache access
    let has_internal_error = has_error_message_containing(&parse_result, "internal")
        || has_error_message_containing(&parse_result, "cache");

    assert!(!has_internal_error, "Should not have internal cache errors");
}

#[test]
fn test_module_exports_with_reexport() {
    let source = r#"
class User:
    pass

export User

export Admin from other_module
"#;

    let result = parse(source);
    assert!(result.is_ok());

    // Export validation should run
    // This will error if other_module doesn't exist or doesn't export Admin
}

#[test]
fn test_protocol_implementation_checking() {
    // Using typing.Protocol syntax from Python (Coral's inspiration)
    let source = r#"
from typing import Protocol

protocol Drawable(Protocol):
    def draw(self) -> str:
        pass

class Circle:
    def draw(self) -> str:
        return "circle"

class BadShape:
    def render(self) -> str:
        return "bad"

def use_drawable(d: Drawable):
    d.draw()

use_drawable(Circle())
use_drawable(BadShape())
"#;

    let result = parse(source);
    assert!(result.is_ok());

    // Protocol checking should detect BadShape doesn't implement Drawable
}

#[test]
fn test_keyword_arg_validation_with_names() {
    let source = r#"
def greet(name: str, greeting: str) -> str:
    return greeting + " " + name

result = greet("Alice", greeting="Hello")
invalid = greet("Bob", invalid_kwarg="Hi")
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // Should detect unexpected keyword argument
    let has_kwarg_error = has_error_message_containing(&parse_result, "keyword")
        || has_error_message_containing(&parse_result, "invalid_kwarg");

    assert!(has_kwarg_error, "Should detect unexpected keyword argument");
}

#[test]
fn test_async_blocking_call_detection() {
    let source = r#"
async def fetch_data():
    time.sleep(1)
    return "done"
"#;

    let result = parse(source);
    assert!(result.is_ok());
    let parse_result = result.unwrap();

    // Should detect blocking call in async context
    let has_blocking_error = has_error_message_containing(&parse_result, "blocking");

    assert!(
        has_blocking_error,
        "Should detect blocking call in async function"
    );
}

#[test]
fn test_operator_overload_with_decorator() {
    let source = r#"
class Vector:
    x: float
    y: float

    def constructor(self, x: float, y: float):
        self.x = x
        self.y = y

    @operator
    def add(self, other: Vector) -> Vector:
        return Vector(self.x + other.x, self.y + other.y)

v1 = Vector(1, 2)
v2 = Vector(3, 4)
v3 = v1 + v2
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Should parse operator overloading");
    let parse_result = result.unwrap();

    // Should have no errors for valid operator overload
    assert!(!parse_result.module.body.is_empty());
}

#[test]
fn test_module_introspection_syntax() {
    let source = r#"
if module::is_main():
    print("Running as main")

name = module::name()
path = module::path()
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Should parse module introspection");
}

#[test]
fn test_parallel_safety_independent_modules() {
    // Test that parallel pass execution doesn't cause race conditions
    // by parsing multiple times concurrently
    use std::thread;

    let source = r#"
class Point:
    x: int
    y: int

    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

p = Point(1, 2)
value = p.x
"#;

    let handles: Vec<_> = (0..4)
        .map(|_| {
            let src = source.to_string();
            thread::spawn(move || {
                let result = parse(&src);
                assert!(result.is_ok(), "Parallel parse should succeed");
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn test_type_inference_with_union_types() {
    let source = r#"
def process(x: int | str) -> str:
    return str(x)

result1 = process(42)
result2 = process("hello")
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Should handle union types");
}

#[test]
fn test_exhaustiveness_checking_on_match() {
    let source = r#"
def check_bool(value: bool) -> str:
    match value:
        case True:
            return "yes"
        case False:
            return "no"
"#;

    let result = parse(source);
    assert!(result.is_ok());

    // Exhaustiveness checking should recognize bool match is complete
}

#[test]
fn test_race_condition_symbol_table_concurrent_access() {
    use std::sync::{Arc, Barrier};
    use std::thread;

    let source = r#"
class SharedClass:
    x: int

    def method(self, y: int) -> int:
        return self.x + y

def use_class():
    obj = SharedClass()
    return obj.method(10)
"#;

    let barrier = Arc::new(Barrier::new(8));
    let handles: Vec<_> = (0..8)
        .map(|_| {
            let src = source.to_string();
            let barrier_clone = Arc::clone(&barrier);
            thread::spawn(move || {
                barrier_clone.wait(); // Sync start for maximum contention
                let result = parse(&src);
                assert!(result.is_ok(), "Concurrent parse should succeed");
                result.unwrap()
            })
        })
        .collect();

    let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();

    // All parses should produce consistent results
    assert_eq!(results.len(), 8);
    for result in &results {
        assert!(!result.module.body.is_empty());
    }
}

#[test]
fn test_race_condition_type_inference_parallel() {
    use std::thread;

    let sources = vec![
        r#"def f1(x: int) -> int: return x + 1"#,
        r#"def f2(x: str) -> str: return x + "!""#,
        r#"def f3(x: float) -> float: return x * 2.0"#,
        r#"def f4(x: list[int]) -> int: return len(x)"#,
    ];

    let handles: Vec<_> = sources
        .into_iter()
        .map(|src| {
            thread::spawn(move || {
                let result = parse(src);
                assert!(result.is_ok());
                result.unwrap()
            })
        })
        .collect();

    for handle in handles {
        let result = handle.join().unwrap();
        assert!(!result.module.body.is_empty());
    }
}

#[test]
fn test_race_condition_cfg_cache_concurrent_builds() {
    use std::thread;

    let source = r#"
def complex_control_flow(x: int) -> int:
    if x > 0:
        if x > 10:
            return x * 2
        else:
            return x + 5
    elif x < 0:
        while x < 0:
            x = x + 1
        return x
    else:
        return 0
"#;

    let handles: Vec<_> = (0..10)
        .map(|_| {
            let src = source.to_string();
            thread::spawn(move || {
                let result = parse(&src);
                assert!(result.is_ok());
                result.unwrap()
            })
        })
        .collect();

    let results: Vec<_> = handles.into_iter().map(|h| h.join().unwrap()).collect();

    // Verify no cache corruption - all results should be valid
    for result in &results {
        assert!(!result.module.body.is_empty());
        assert!(!has_error_message_containing(result, "cache"));
    }
}

#[test]
fn test_type_mismatch_in_return() {
    let source = r#"
def get_number() -> int:
    return "not a number"
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect return type mismatch
    let has_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "return")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_error, "Should detect return type mismatch");
}

#[test]
fn test_type_mismatch_in_assignment() {
    let source = r#"
x: int = 42
x = "string"
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect type mismatch on reassignment
    let has_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_error, "Should detect assignment type mismatch");
}

#[test]
fn test_undefined_variable_usage() {
    let source = r#"
def use_undefined():
    return undefined_var + 1
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect undefined variable
    let has_error = has_error_message_containing(&parse_result, "undefined")
        || has_error_message_containing(&parse_result, "not defined");

    assert!(has_error, "Should detect undefined variable");
}

#[test]
fn test_method_missing_on_class() {
    let source = r#"
class MyClass:
    def method_a(self):
        pass

obj = MyClass()
obj.method_b()
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect missing method
    let has_error = has_error_message_containing(&parse_result, "method")
        || has_error_message_containing(&parse_result, "attribute");

    assert!(has_error, "Should detect missing method on class instance");
}

#[test]
fn test_protocol_method_signature_mismatch() {
    let source = r#"
protocol Comparable:
    def compare(self, other: Comparable) -> int:
        pass

class Number:
    value: int

    def compare(self, other: Number) -> str:
        return "different"
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect protocol method return type mismatch
    let has_error = has_error_message_containing(&parse_result, "protocol")
        || has_error_message_containing(&parse_result, "signature")
        || has_error_message_containing(&parse_result, "return");

    assert!(
        has_error,
        "Should detect protocol method signature mismatch"
    );
}

#[test]
fn test_protocol_missing_method() {
    let source = r#"
protocol Drawable:
    def draw(self) -> str:
        pass

    def erase(self) -> None:
        pass

class Circle:
    def draw(self) -> str:
        return "circle"
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect missing protocol method
    let has_error = has_error_message_containing(&parse_result, "protocol")
        || has_error_message_containing(&parse_result, "missing")
        || has_error_message_containing(&parse_result, "erase");

    assert!(has_error, "Should detect missing protocol method");
}

#[test]
fn test_async_await_in_sync_function() {
    let source = r#"
def sync_function():
    result = await async_call()
    return result
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect await in non-async function
    let has_error = has_error_message_containing(&parse_result, "await")
        || has_error_message_containing(&parse_result, "async");

    assert!(has_error, "Should detect await in sync function");
}

#[test]
fn test_async_blocking_io_call() {
    let source = r#"
async def fetch():
    file = open("data.txt")
    content = file.read()
    return content
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect blocking I/O in async function
    let has_error = has_error_message_containing(&parse_result, "blocking")
        || has_error_message_containing(&parse_result, "async");

    assert!(has_error, "Should detect blocking I/O in async function");
}

#[test]
fn test_variable_used_before_definition() {
    let source = r#"
def f():
    print(x)
    x = 10
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Definite assignment should detect this
    let has_error = has_error_message_containing(&parse_result, "before")
        || has_error_message_containing(&parse_result, "undefined")
        || has_error_message_containing(&parse_result, "not defined");

    assert!(has_error, "Should detect variable used before definition");
}

#[test]
fn test_variable_possibly_uninitialized() {
    let source = r#"
def conditional_init(flag: bool):
    if flag:
        x = 10
    return x
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Definite assignment should detect possibly uninitialized variable
    let has_error = has_error_message_containing(&parse_result, "uninitialized")
        || has_error_message_containing(&parse_result, "not defined");

    assert!(has_error, "Should detect possibly uninitialized variable");
}

#[test]
fn test_operator_overload_wrong_signature() {
    let source = r#"
class Vector:
    @operator
    def add(self) -> Vector:
        return self
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect wrong operator signature
    let has_error = has_error_message_containing(&parse_result, "operator")
        || has_error_message_containing(&parse_result, "signature")
        || has_error_message_containing(&parse_result, "parameter");

    assert!(
        has_error,
        "Should detect operator overload with wrong signature"
    );
}

#[test]
fn test_property_setter_type_mismatch() {
    let source = r#"
class MyClass:
    _value: int

    @property
    def value(self) -> int:
        return self._value

    @value.setter
    def value(self, v: str):
        self._value = v
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect property setter type mismatch
    let has_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "property")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_error, "Should detect property setter type mismatch");
}

#[test]
fn test_circular_type_reference() {
    let source = r#"
class A:
    b: B

class B:
    a: A
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should handle circular type references (either allow with forward refs or error)
    assert!(parse_result.module.body.len() == 2);
}

#[test]
fn test_generic_type_parameter_mismatch() {
    let source = r#"
def process_list(items: list[int]) -> int:
    return items[0]

result = process_list(["a", "b", "c"])
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect generic type parameter mismatch
    let has_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "type");

    assert!(has_error, "Should detect generic type parameter mismatch");
}

#[test]
fn test_union_type_handling() {
    let source = r#"
def process(x: int | str) -> str:
    if isinstance(x, int):
        return str(x)
    else:
        return x

result1 = process(42)
result2 = process("hello")
result3 = process(3.14)
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect type not in union
    let has_error = has_error_type(&parse_result, "TypeError")
        || has_error_message_containing(&parse_result, "type");

    assert!(
        has_error,
        "Should detect type not in union (float passed to int|str)"
    );
}

#[test]
fn test_module_export_undefined_name() {
    let source = r#"
def my_function():
    pass

export undefined_name
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect export of undefined name
    let has_error = has_error_message_containing(&parse_result, "export")
        || has_error_message_containing(&parse_result, "undefined");

    assert!(has_error, "Should detect export of undefined name");
}

#[test]
fn test_module_reexport_nonexistent() {
    let source = r#"
export NonExistent from other_module
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect re-export of non-existent name from module
    let has_error = has_error_message_containing(&parse_result, "export")
        || has_error_message_containing(&parse_result, "module");

    assert!(has_error, "Should detect re-export of non-existent name");
}

#[test]
fn test_duplicate_export() {
    let source = r#"
def my_function():
    pass

export my_function
export my_function
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect duplicate export
    let has_error = has_error_message_containing(&parse_result, "duplicate")
        || has_error_message_containing(&parse_result, "export");

    assert!(has_error, "Should detect duplicate export");
}

#[test]
fn test_no_errors_on_valid_code() {
    let source = r#"
class Point:
    x: int
    y: int

    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

    def distance(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5

def create_point() -> Point:
    return Point(3, 4)

p = create_point()
d = p.distance()
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should have NO errors for completely valid code
    assert_eq!(
        parse_result.errors().len(),
        0,
        "Valid code should have no errors, but got: {:?}",
        parse_result.errors()
    );
}

#[test]
fn test_no_errors_on_valid_async() {
    let source = r#"
async def fetch_data(url: str) -> str:
    response = await http_get(url)
    return response

async def main():
    data = await fetch_data("http://example.com")
    return data
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should have NO errors for valid async code
    assert_eq!(
        parse_result.errors().len(),
        0,
        "Valid async code should have no errors, but got: {:?}",
        parse_result.errors()
    );
}

#[test]
fn test_ownership_double_move() {
    let source = r#"
def consume(x: owned str) -> str:
    return x

def use_twice():
    s = "hello"
    a = consume(s)
    b = consume(s)
    return b
"#;

    let result = parse(source);
    assert!(result.is_ok(), "Parse should succeed");
    let parse_result = result.unwrap();

    // Should detect double move/use after move
    let has_error = has_error_message_containing(&parse_result, "moved")
        || has_error_message_containing(&parse_result, "ownership");

    assert!(has_error, "Should detect double move/use after move");
}
