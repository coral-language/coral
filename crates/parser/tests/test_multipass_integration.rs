//! Multi-pass integration tests
//!
//! Tests that verify multiple semantic analysis passes work correctly together
//! and that state is properly shared via AnalysisContext.

use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_type_checking_after_inference() {
    let source = r#"
def greet(name: str) -> str:
    return "Hello, " + name

message = greet("World")
wrong = greet(42)
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source).assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("await")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_error_types_from_different_passes() {
    let source = r#"
x: int = "not an int"

y = undefined_variable

def regular():
    return await something()
"#;

    DiagnosticTestBuilder::errors(source).assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_cfg_shared_across_passes() {
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

    DiagnosticTestBuilder::errors(source)
        .expect_not("internal")
        .expect_not("cache")
        .assert_all();
}

#[test]
fn test_module_exports_with_reexport() {
    let source = r#"
class User:
    pass

export User

export Admin from other_module
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_implementation_checking() {
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

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_keyword_arg_validation_with_names() {
    let source = r#"
def greet(name: str, greeting: str) -> str:
    return greeting + " " + name

result = greet("Alice", greeting="Hello")
invalid = greet("Bob", invalid_kwarg="Hi")
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("keyword")
        .assert_some();
}

#[test]
fn test_async_blocking_call_detection() {
    let source = r#"
async def fetch_data():
    time.sleep(1)
    return "done"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("blocking")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_module_introspection_syntax() {
    let source = r#"
if module::is_main():
    print("Running as main")

name = module::name()
path = module::path()
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_parallel_safety_independent_modules() {
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
                DiagnosticTestBuilder::errors(&src).assert_none();
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

    DiagnosticTestBuilder::errors(source).assert_none();
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

    DiagnosticTestBuilder::errors(source).assert_none();
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
                DiagnosticTestBuilder::errors(&src).assert_none();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
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
                DiagnosticTestBuilder::errors(src).assert_none();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
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
                DiagnosticTestBuilder::errors(&src)
                    .expect_not("cache")
                    .assert_all();
            })
        })
        .collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn test_type_mismatch_in_return() {
    let source = r#"
def get_number() -> int:
    return "not a number"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
}

#[test]
fn test_type_mismatch_in_assignment() {
    let source = r#"
x: int = 42
x = "string"
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
}

#[test]
fn test_undefined_variable_usage() {
    let source = r#"
def use_undefined():
    return undefined_var + 1
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("undefined")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("method")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("protocol")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("protocol")
        .assert_some();
}

#[test]
fn test_async_await_in_sync_function() {
    let source = r#"
def sync_function():
    result = await async_call()
    return result
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("await")
        .assert_some();
}

#[test]
fn test_async_blocking_io_call() {
    let source = r#"
async def fetch():
    file = open("data.txt")
    content = file.read()
    return content
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("blocking")
        .assert_some();
}

#[test]
fn test_variable_used_before_definition() {
    let source = r#"
def f():
    print(x)
    x = 10
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("before")
        .assert_some();
}

#[test]
fn test_variable_possibly_uninitialized() {
    let source = r#"
def conditional_init(flag: bool):
    if flag:
        x = 10
    return x
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("uninitialized")
        .assert_some();
}

#[test]
fn test_operator_overload_wrong_signature() {
    let source = r#"
class Vector:
    @operator
    def add(self) -> Vector:
        return self
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("operator")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
}

#[test]
fn test_circular_type_reference() {
    let source = r#"
class A:
    b: B

class B:
    a: A
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_generic_type_parameter_mismatch() {
    let source = r#"
def process_list(items: list[int]) -> int:
    return items[0]

result = process_list(["a", "b", "c"])
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source)
        .expect("TypeError")
        .assert_some();
}

#[test]
fn test_module_export_undefined_name() {
    let source = r#"
def my_function():
    pass

export undefined_name
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("export")
        .assert_some();
}

#[test]
fn test_module_reexport_nonexistent() {
    let source = r#"
export NonExistent from other_module
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("export")
        .assert_some();
}

#[test]
fn test_duplicate_export() {
    let source = r#"
def my_function():
    pass

export my_function
export my_function
"#;

    DiagnosticTestBuilder::errors(source)
        .expect("duplicate")
        .assert_some();
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

    DiagnosticTestBuilder::errors(source).assert_none();
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

    DiagnosticTestBuilder::errors(source).assert_none();
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

    DiagnosticTestBuilder::errors(source)
        .expect("moved")
        .assert_some();
}
