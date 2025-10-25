# Ownership and Memory Management in Coral

Coral provides **Python-like syntax** with **Rust-like memory safety**, without requiring any ownership annotations or manual memory management from the user.

## The Coral Way: Implicit and Smart

### Write Natural Code

```coral
x = expensive_object()
y = x  # Looks like a copy
z = x  # Also looks like a copy
process(x)
```

Behind the scenes, Coral analyzes your code and determines:
- Whether these are actually copies or moves
- When values can be freed
- Whether variables can be stack-allocated
- Where cleanup code is needed

**You write the logic, Coral handles the memory.**

## How It Works

### 1. Last-Use Analysis

Coral tracks where each variable is last used:

```coral
def process_data(filename):
    data = load_file(filename)
    result = process(data)  # Last use of 'data'
    return result

# Coral detects 'data' is never used after process()
# Code generation can move instead of copy
```

### 2. Smart Function Arguments

```coral
def example():
    x = "hello"
    print(x)  # x is used after
    consume(x)  # x is NOT used after
```

Coral checks usage after each call:
- `print(x)`: Passes copy (x still needed)
- `consume(x)`: Passes move (x never used again)

### 3. Resource Management

Resources are automatically cleaned up:

```coral
def read_file(path):
    f = File(path)  # Resource acquired
    data = f.read()
    return data
# File automatically closed when f goes out of scope
```

Or use explicit cleanup:

```coral
def read_file(path):
    with File(path) as f:
        return f.read()
# Guaranteed cleanup even with exceptions
```

### 4. Reference Cycles

Coral detects circular references:

```coral
class Node:
    def constructor(self, value):
        self.value = value
        self.next = None

# This creates a cycle:
a = Node(1)
b = Node(2)
a.next = b
b.next = a  # Warning: circular reference

# Break it with weakref:
import weakref
a.next = b
b.next = weakref.ref(a)  # No warning
```

## Memory Safety Guarantees

Coral prevents:
- **Use-after-free**: Variables can't be used after their lifetime ends
- **Memory leaks**: Resources are automatically cleaned up
- **Dangling references**: References are validated at compile time

Example of prevented error:

```coral
def bad_example():
    if condition:
        x = get_value()
    return x  # Error: x may not be initialized on all paths
```

Coral catches this at compile time, not runtime.

## Performance Benefits

### Stack Allocation

Local variables that don't escape are stack-allocated:

```coral
def calculate():
    x = 42      # Stack allocated
    y = x * 2   # Stack allocated
    return y
```

### Move Optimization

Variables passed to functions can be moved:

```coral
def process_data():
    data = expensive_operation()
    result = transform(data)  # Data moved if not used after
    return result
```

This avoids expensive copying of large data structures.

## Best Practices

### 1. Let Coral Optimize

Write natural code; Coral will optimize:

```coral
# Don't worry about this:
x = large_object()
y = x  # Is this expensive?

# Coral determines if copy is actually needed
```

### 2. Use Context Managers for Resources

```coral
# Preferred:
with File("data.txt") as f:
    process(f)

# Also fine (auto-cleanup at scope end):
f = File("data.txt")
process(f)
```

### 3. Break Cycles Explicitly

If you create circular data structures, use weak references:

```coral
from weakref import ref

parent.child = child_node
child_node.parent = ref(parent)  # Weak reference
```

### 4. Trust the Compiler

Coral's analysis is conservative and safe. If it compiles, it's memory-safe.

## Advanced: Understanding the Analysis

### Control Flow

Coral uses control flow graphs to track variable lifetimes:

```coral
def example(condition):
    x = value()
    if condition:
        process(x)
    else:
        transform(x)
    # x is last used in both branches
```

### Escape Analysis

Variables that escape are heap-allocated:

```coral
def escapes():
    x = value()
    return x  # x escapes - heap allocated

def no_escape():
    x = value()
    print(x)
    # x doesn't escape - stack allocated
```

## Comparison to Other Languages

### vs Python

- **Python**: Garbage collection, reference counting overhead
- **Coral**: Compile-time analysis, no GC needed

### vs Rust

- **Rust**: Explicit ownership, borrow checker annotations
- **Coral**: Implicit analysis, no annotations needed

### vs C++

- **C++**: Manual memory management, RAII for resources
- **Coral**: Automatic analysis, guaranteed safety

## Error Messages

When Coral detects issues:

```coral
def example():
    if condition:
        x = value()
    use(x)  # Error: x may not be initialized

# Error message:
# E6001: Variable 'x' may not be initialized on all code paths
#   at line 4: use(x)
#   note: 'x' is conditionally initialized at line 3
```

## Resource Types

Coral automatically recognizes these resource types and enforces cleanup:

- **File**: Text and binary file handles
- **Connection**: Database and network connections
- **Lock**: Synchronization primitives
- **Transaction**: Database transactions
- **Iterator**: Active iterators

### Automatic Cleanup

```coral
# Automatic cleanup at scope end
f = File("data.txt")
data = f.read()
# f automatically closed here

# Explicit cleanup
f = File("data.txt")
data = f.read()
f.close()  # Manual close

# Context manager (recommended for resources)
with File("data.txt") as f:
    data = f.read()
# Guaranteed cleanup even with exceptions
```

## Common Patterns

### Processing Collections

```coral
# Elements referenced, then collection moves
data = load_data()
result = process(data)  # data moved if not used after

# Iterating keeps collection alive
for item in items:
    process(item)  # items still valid after loop
```

### Nested Scopes

```coral
def outer():
    x = value()
    if condition:
        y = x + 1  # x borrowed
    use(x)  # x still valid
```

### Exception Handling

```coral
f = File("data.txt")
try:
    process(f)
except Error:
    handle_error()
finally:
    f.close()  # Guaranteed cleanup
```

## Summary

Coral's ownership model gives you:

- ✅ Python-like syntax and ease of use
- ✅ Rust-like memory safety guarantees
- ✅ Zero-cost abstractions
- ✅ No garbage collector
- ✅ Compile-time error detection

**Write natural code, get optimal performance and safety automatically.**
