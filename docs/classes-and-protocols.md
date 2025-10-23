# Coral Classes and Protocols

Coral provides a modern, explicit class system with protocol-based interfaces. All special behavior is clearly marked with decorators, making the language discoverable and type-safe.

## Table of Contents

- [Overview](#overview)
- [Design Principles](#design-principles)
- [Classes](#classes)
- [Protocol System](#protocol-system)
- [Operator Overloading](#operator-overloading)
- [Properties](#properties)
- [Container Protocols](#container-protocols)
- [Context Managers](#context-managers)
- [Callable Objects](#callable-objects)

## Overview

Coral's class system is designed around three core concepts:

- **Explicit syntax**: Methods are clearly marked with decorators
- **Protocol-based interfaces**: Contracts define capabilities
- **Type safety**: Compile-time checking of implementations

## Design Principles

1. **No Magic**: All special behavior is explicit and discoverable
2. **Protocol-Based**: Use protocols to define contracts and capabilities
3. **Decorator-Driven**: Use decorators to mark special methods
4. **Method Name Clarity**: Method names directly match their operations
5. **Type Safety**: Compile-time checking of protocol implementations
6. **Consistency**: Single patterns for all special behavior

## Classes

### Basic Class Definition

```coral
class Point:
    x: int
    y: int

    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

    def distance_from_origin(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5
```

### Constructors

Classes use the `constructor` keyword for initialization:

```coral
class Rectangle:
    width: float
    height: float

    def constructor(self, width: float, height: float):
        self.width = width
        self.height = height

# Usage
let rect = Rectangle(10.0, 5.0)
```

### Auto-Generated Constructors

If no explicit constructor is defined, Coral auto-generates one from the class attributes:

```coral
class SimplePoint:
    x: int
    y: int

# Auto-generates: def constructor(self, x: int, y: int): ...

let p = SimplePoint(1, 2)  # Works automatically
```

### String Representation

Classes can define how they're displayed using `@operator` decorated methods:

```coral
class Point:
    x: int
    y: int

    def constructor(self, x: int, y: int):
        self.x = x
        self.y = y

    @operator
    def str(self) -> str:
        return f"Point({self.x}, {self.y})"

    @operator
    def repr(self) -> str:
        return f"Point(x={self.x}, y={self.y})"

# Usage
let p = Point(1, 2)
print(p)        # Calls @operator str method -> "Point(1, 2)"
print(repr(p))  # Calls @operator repr method -> "Point(x=1, y=2)"
```

## Protocol System

### Defining Protocols

Protocols define contracts that classes can implement:

```coral
protocol Drawable:
    def draw(self) -> None
    def area(self) -> float

protocol Sequence[T]:
    @operator
    def len(self) -> int
    @operator
    def getitem(self, index: int) -> T
    @operator
    def setitem(self, index: int, value: T) -> None

protocol Iterable[T]:
    @operator
    def iter(self) -> Iterator[T]

protocol Iterator[T]:
    @operator
    def next(self) -> Option[T]

protocol ContextManager[T]:
    @operator
    def enter(self) -> T
    @operator
    def exit(self, exc_type, exc_val, exc_tb) -> None
```

### Implementing Protocols

Classes explicitly declare which protocols they implement:

```coral
class Circle implements Drawable:
    radius: float

    def constructor(self, radius: float):
        self.radius = radius

    def draw(self) -> None:
        print("Drawing a circle")

    def area(self) -> float:
        return 3.14159 * self.radius * self.radius
```

### Multiple Protocols

A class can implement multiple protocols:

```coral
class Rectangle implements Drawable, Sequence[float]:
    width: float
    height: float

    def constructor(self, width: float, height: float):
        self.width = width
        self.height = height

    def draw(self) -> None:
        print("Drawing a rectangle")

    def area(self) -> float:
        return self.width * self.height

    @operator
    def len(self) -> int:
        return 2

    @operator
    def getitem(self, index: int) -> float:
        return self.width if index == 0 else self.height

    @operator
    def setitem(self, index: int, value: float):
        if index == 0:
            self.width = value
        else:
            self.height = value
```

### Using Protocols

Protocols enable type-safe, flexible function parameters:

```coral
def render_shape(shape: Drawable) -> None:
    shape.draw()
    print(f"Area: {shape.area()}")

def process_sequence(seq: Sequence[int]) -> int:
    total = 0
    for i in range(seq.len()):
        total += seq.getitem(i)
    return total

# Usage
let circle = Circle(5.0)
let rectangle = Rectangle(3.0, 4.0)

render_shape(circle)             # ✓ Circle implements Drawable
render_shape(rectangle)          # ✓ Rectangle implements Drawable
process_sequence(rectangle)      # ✓ Rectangle implements Sequence[float]
```

## Operator Overloading

### The @operator Decorator

All operator overloading uses the `@operator` decorator with readable method names:

```coral
class Vector:
    x: float
    y: float

    def constructor(self, x: float, y: float):
        self.x = x
        self.y = y

    @operator
    def add(self, other: Vector) -> Vector:
        return Vector(self.x + other.x, self.y + other.y)

    @operator
    def subtract(self, other: Vector) -> Vector:
        return Vector(self.x - other.x, self.y - other.y)

    @operator
    def multiply(self, scalar: float) -> Vector:
        return Vector(self.x * scalar, self.y * scalar)

    @operator
    def equals(self, other: Vector) -> bool:
        return self.x == other.x and self.y == other.y

    @operator
    def less_than(self, other: Vector) -> bool:
        return self.magnitude() < other.magnitude()

    # Regular method (no decorator needed)
    def magnitude(self) -> float:
        return (self.x * self.x + self.y * self.y) ** 0.5

# Usage
let v1 = Vector(1, 2)
let v2 = Vector(3, 4)
let sum = v1 + v2        # Calls @operator add
let is_equal = v1 == v2  # Calls @operator equals
let mag = v1.magnitude() # Calls regular method
```

### Operator Method Reference

| Operation | Method Name | Example |
|-----------|-------------|---------|
| `+` | `add` | `a + b` |
| `-` | `subtract` | `a - b` |
| `*` | `multiply` | `a * b` |
| `/` | `truediv` | `a / b` |
| `//` | `floordiv` | `a // b` |
| `%` | `mod` | `a % b` |
| `**` | `pow` | `a ** b` |
| `==` | `equals` | `a == b` |
| `!=` | `not_equals` | `a != b` |
| `<` | `less_than` | `a < b` |
| `<=` | `less_equal` | `a <= b` |
| `>` | `greater_than` | `a > b` |
| `>=` | `greater_equal` | `a >= b` |
| `+x` | `pos` | `+a` |
| `-x` | `neg` | `-a` |
| `~x` | `invert` | `~a` |

## Properties

### Computed Properties

Use `@property` decorator for computed properties:

```coral
class Rectangle:
    width: float
    height: float

    def constructor(self, width: float, height: float):
        self.width = width
        self.height = height

    @property
    def area(self) -> float:
        return self.width * self.height

    @property
    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

    @property
    def diagonal(self) -> float:
        return (self.width * self.width + self.height * self.height) ** 0.5

# Usage
let r = Rectangle(5, 3)
print(r.area)      # 15.0 - computed on access
print(r.perimeter) # 16.0
print(r.diagonal)  # 5.83...
```

### Property Setters

Properties can have setters for controlled access:

```coral
class Circle:
    _radius: float

    def constructor(self, radius: float):
        self._radius = radius

    @property
    def radius(self) -> float:
        return self._radius

    @radius.setter
    def radius(self, value: float):
        if value < 0:
            raise ValueError("Radius must be positive")
        self._radius = value

# Usage
let c = Circle(5.0)
c.radius = 10.0  # Calls setter with validation
print(c.radius)  # Calls getter
```

## Container Protocols

### Sequence Protocol

Implement indexing and length operations:

```coral
class MyList[T] implements Sequence[T]:
    items: list[T]

    def constructor(self, items: list[T]):
        self.items = items

    @operator
    def len(self) -> int:
        return len(self.items)

    @operator
    def getitem(self, index: int) -> T:
        return self.items[index]

    @operator
    def setitem(self, index: int, value: T):
        self.items[index] = value

    @operator
    def contains(self, item: T) -> bool:
        return item in self.items

# Usage
let my_list = MyList([1, 2, 3])
print(len(my_list))    # 3 - calls @operator len
print(my_list[0])      # 1 - calls @operator getitem
my_list[1] = 42        # calls @operator setitem
print(2 in my_list)    # True - calls @operator contains
```

### Iterable Protocol

Implement iteration behavior:

```coral
class Range implements Iterable[int]:
    start: int
    stop: int
    step: int

    def constructor(self, start: int, stop: int, step: int = 1):
        self.start = start
        self.stop = stop
        self.step = step

    @operator
    def iter(self) -> Iterator[int]:
        return RangeIterator(self)

class RangeIterator implements Iterator[int]:
    range: Range
    current: int

    def constructor(self, range: Range):
        self.range = range
        self.current = range.start

    @operator
    def next(self) -> Option[int]:
        if self.current >= self.range.stop:
            return None
        let value = self.current
        self.current += self.range.step
        return Some(value)

# Usage
for i in Range(0, 10, 2):
    print(i)  # 0, 2, 4, 6, 8
```

## Context Managers

Use the `ContextManager` protocol with `@operator` decorated methods:

```coral
class FileManager implements ContextManager[FileManager]:
    filename: str
    file: Optional[File]

    def constructor(self, filename: str):
        self.filename = filename
        self.file = None

    @operator
    def enter(self) -> FileManager:
        self.file = open(self.filename, 'r')
        return self

    @operator
    def exit(self, exc_type, exc_val, exc_tb) -> None:
        if self.file:
            self.file.close()

# Usage
with FileManager("data.txt") as fm:
    content = fm.file.read()
# File is automatically closed after the block
```

## Callable Objects

Make objects callable by implementing the `call` operator:

```coral
class Multiplier:
    factor: int

    def constructor(self, factor: int):
        self.factor = factor

    @operator
    def call(self, value: int) -> int:
        return value * self.factor

# Usage
let double = Multiplier(2)
let result = double(5)  # 10 - calls @operator call method
let triple = Multiplier(3)
let result2 = triple(5) # 15
```

## Summary

Coral's class and protocol system provides:

1. **Discoverability**: All special methods clearly marked with `@operator`
2. **Explicitness**: The `constructor` keyword makes initialization clear
3. **Type Safety**: Protocols provide compile-time checking
4. **Consistency**: Single decorator pattern for all special behavior
5. **Readability**: Method names directly correspond to operations
6. **Maintainability**: Clear separation between regular and special methods
7. **Modern Design**: Follows current programming language best practices
8. **IDE Support**: Better autocomplete and error checking

