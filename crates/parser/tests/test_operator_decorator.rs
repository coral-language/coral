//! Tests for @operator decorator functionality

use coral_parser::helpers::parse_ok;

#[test]
fn test_operator_add() {
    let source = r#"
class Vector:
    def constructor(self, x, y):
        self.x = x
        self.y = y

    @operator
    def add(self, other):
        return Vector(self.x + other.x, self.y + other.y)
"#;

    parse_ok(source);
}

#[test]
fn test_operator_string_methods() {
    let source = r#"
class Point:
    def constructor(self, x, y):
        self.x = x
        self.y = y

    @operator
    def str(self):
        return f"Point({self.x}, {self.y})"

    @operator
    def repr(self):
        return f"Point(x={self.x}, y={self.y})"
"#;

    parse_ok(source);
}

#[test]
fn test_operator_comparison() {
    let source = r#"
class Number:
    def constructor(self, value):
        self.value = value

    @operator
    def equals(self, other):
        return self.value == other.value

    @operator
    def less_than(self, other):
        return self.value < other.value

    @operator
    def greater_than(self, other):
        return self.value > other.value
"#;

    parse_ok(source);
}

#[test]
fn test_operator_container_protocols() {
    let source = r#"
class MyList:
    def constructor(self, items):
        self.items = items

    @operator
    def len(self):
        return len(self.items)

    @operator
    def getitem(self, index):
        return self.items[index]

    @operator
    def setitem(self, index, value):
        self.items[index] = value

    @operator
    def contains(self, item):
        return item in self.items
"#;

    parse_ok(source);
}

#[test]
fn test_operator_iterator() {
    let source = r#"
class Range:
    def constructor(self, start, stop):
        self.start = start
        self.stop = stop

    @operator
    def iter(self):
        return RangeIterator(self.start, self.stop)

class RangeIterator:
    def constructor(self, current, stop):
        self.current = current
        self.stop = stop

    @operator
    def next(self):
        if self.current >= self.stop:
            return None
        value = self.current
        self.current += 1
        return value
"#;

    parse_ok(source);
}

#[test]
fn test_operator_context_manager() {
    let source = r#"
class FileManager:
    def constructor(self, filename):
        self.filename = filename

    @operator
    def enter(self):
        self.file = open(self.filename)
        return self

    @operator
    def exit(self, exc_type, exc_val, exc_tb):
        if self.file:
            self.file.close()
"#;

    parse_ok(source);
}

#[test]
fn test_operator_callable() {
    let source = r#"
class Multiplier:
    def constructor(self, factor):
        self.factor = factor

    @operator
    def call(self, value):
        return value * self.factor
"#;

    parse_ok(source);
}

#[test]
fn test_operator_arithmetic() {
    let source = r#"
class Complex:
    def constructor(self, real, imag):
        self.real = real
        self.imag = imag

    @operator
    def add(self, other):
        return Complex(self.real + other.real, self.imag + other.imag)

    @operator
    def subtract(self, other):
        return Complex(self.real - other.real, self.imag - other.imag)

    @operator
    def multiply(self, other):
        real = self.real * other.real - self.imag * other.imag
        imag = self.real * other.imag + self.imag * other.real
        return Complex(real, imag)
"#;

    parse_ok(source);
}

#[test]
fn test_operator_unary() {
    let source = r#"
class Number:
    def constructor(self, value):
        self.value = value

    @operator
    def neg(self):
        return Number(-self.value)

    @operator
    def pos(self):
        return Number(+self.value)
"#;

    parse_ok(source);
}

#[test]
fn test_multiple_operators() {
    let source = r#"
class FullFeatured:
    def constructor(self, value):
        self.value = value

    @operator
    def str(self):
        return f"Value: {self.value}"

    @operator
    def add(self, other):
        return FullFeatured(self.value + other.value)

    @operator
    def equals(self, other):
        return self.value == other.value

    @operator
    def len(self):
        return 1
"#;

    parse_ok(source);
}
