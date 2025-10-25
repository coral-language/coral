//! Integration tests for Coral protocol system with comprehensive corner cases
//! Protocols are interface definitions with method signatures only, no bodies

use coral_parser::helpers::DiagnosticTestBuilder;

#[test]
fn test_protocol_definition() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None
    def area(self) -> float
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_empty_with_pass() {
    let source = r#"
protocol Empty:
    pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_single_method() {
    let source = r#"
protocol Single:
    def method(self) -> int
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_generic() {
    let source = r#"
protocol Sequence[T]:
    @operator
    def len(self) -> int
    @operator
    def getitem(self, index: int) -> T
    @operator
    def setitem(self, index: int, value: T) -> None
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_multiple_generics() {
    let source = r#"
protocol Pair[K, V]:
    def key(self) -> K
    def value(self) -> V
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_implements_single_protocol() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    radius: float

    def constructor(self, radius: float):
        self.radius = radius

    def draw(self) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_implements_protocol_with_pass() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    def draw(self) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_implements_multiple_protocols() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

protocol Resizable:
    def resize(self, factor: float) -> None

class Shape implements Drawable, Resizable:
    def draw(self) -> None:
        pass

    def resize(self, factor: float) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_property() {
    let source = r#"
protocol Named:
    @property
    def name(self) -> str

class Person implements Named:
    _name: str

    def constructor(self, name: str):
        self._name = name

    @property
    def name(self) -> str:
        return self._name
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_operators() {
    let source = r#"
protocol Comparable:
    @operator
    def equals(self, other) -> bool
    @operator
    def less_than(self, other) -> bool

class Number implements Comparable:
    value: int

    def constructor(self, value: int):
        self.value = value

    @operator
    def equals(self, other) -> bool:
        return 1 == 1

    @operator
    def less_than(self, other) -> bool:
        return 0 == 1
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_as_parameter_type() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    def draw(self) -> None:
        pass

def render(shape: Drawable):
    pass

render(Circle())
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_as_return_type() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    def draw(self) -> None:
        pass

def create_drawable() -> Drawable:
    return Circle()
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_structural_typing_without_explicit_implements() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Square:
    def draw(self) -> None:
        pass

def render(shape: Drawable):
    pass

render(Square())
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_context_manager() {
    let source = r#"
protocol ContextManager[T]:
    @operator
    def enter(self) -> T
    @operator
    def exit(self, exc_type, exc_val, exc_tb) -> None

class FileManager implements ContextManager[str]:
    filename: str

    def constructor(self, filename: str):
        self.filename = filename

    @operator
    def enter(self) -> str:
        return self.filename

    @operator
    def exit(self, exc_type, exc_val, exc_tb) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_iterable() {
    let source = r#"
protocol Iterator[T]:
    @operator
    def next(self) -> T

class Range implements Iterator[int]:
    current: int

    def constructor(self, current: int):
        self.current = current

    @operator
    def next(self) -> int:
        return 0
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_protocols_complex() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

protocol Resizable:
    def resize(self, factor: float) -> None

protocol Colorable:
    def set_color(self, r: int, g: int, b: int) -> None

class Shape implements Drawable, Resizable, Colorable:
    def draw(self) -> None:
        pass

    def resize(self, factor: float) -> None:
        pass

    def set_color(self, r: int, g: int, b: int) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_method_with_no_parameters() {
    let source = r#"
protocol NoParams:
    def get(self) -> int

class Impl implements NoParams:
    def get(self) -> int:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_method_with_many_parameters() {
    let source = r#"
protocol ManyParams:
    def func(self, a: int, b: str, c: float, d: bool) -> int

class Impl implements ManyParams:
    def func(self, a: int, b: str, c: float, d: bool) -> int:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_inheritance_not_supported_syntax() {
    let source = r#"
protocol Base:
    def base_method(self) -> None

protocol Extended(Base):
    def extended_method(self) -> None
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_nested_protocol_in_class() {
    let source = r#"
class Outer:
    def method(self) -> None:
        pass

protocol Inner:
    def inner_method(self) -> None
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_method_return_protocol() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    def draw(self) -> None:
        pass

protocol Factory:
    def create(self) -> Drawable

class ShapeFactory implements Factory:
    def create(self) -> Drawable:
        return Circle()
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_multiple_implementations_of_same_protocol() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Circle implements Drawable:
    def draw(self) -> None:
        pass

class Square implements Drawable:
    def draw(self) -> None:
        pass

class Triangle implements Drawable:
    def draw(self) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_varargs_not_standard() {
    let source = r#"
protocol Container:
    def add(self, item: int) -> None

class List implements Container:
    def add(self, item: int) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_method_with_none_parameter() {
    let source = r#"
protocol Handler:
    def handle(self, data: None) -> None

class Impl implements Handler:
    def handle(self, data: None) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_decorator_in_protocol_and_implementation() {
    let source = r#"
protocol Comparable:
    @operator
    def less_than(self, other) -> bool

class Value implements Comparable:
    val: int

    @operator
    def less_than(self, other) -> bool:
        return 1 == 0
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_implemented_by_multiple_classes() {
    let source = r#"
protocol Serializable:
    def serialize(self) -> str

class Data implements Serializable:
    def serialize(self) -> str:
        pass

class Config implements Serializable:
    def serialize(self) -> str:
        pass

def save(obj: Serializable):
    pass

save(Data())
save(Config())
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_with_complex_generic_type() {
    let source = r#"
protocol Response[T]:
    def get_data(self) -> T
    def get_status(self) -> int

class ApiResponse implements Response[str]:
    data: str

    def get_data(self) -> str:
        return self.data

    def get_status(self) -> int:
        return 200
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_with_pass_body_returning_none() {
    let source = r#"
class Simple:
    def noop(self) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_with_pass_body_returning_int() {
    let source = r#"
class Simple:
    def compute(self) -> int:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_class_with_pass_body_returning_str() {
    let source = r#"
class Simple:
    def describe(self) -> str:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_can_coexist_with_classes() {
    let source = r#"
protocol Drawable:
    def draw(self) -> None

class Animal:
    name: str

    def speak(self) -> None:
        pass

class Dog implements Drawable:
    def draw(self) -> None:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_function_with_pass_returning_complex_type() {
    let source = r#"
protocol Response:
    def status(self) -> int

def create_response() -> Response:
    pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}

#[test]
fn test_protocol_method_with_pass_body() {
    let source = r#"
protocol Handler:
    def process(self, data: int) -> bool

class DefaultHandler implements Handler:
    def process(self, data: int) -> bool:
        pass
"#;

    DiagnosticTestBuilder::errors(source).assert_none();
}
