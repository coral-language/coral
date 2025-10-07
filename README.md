<div align="center">

# 🪸 Coral Programming Language

A fast, elegant, and type-safe language inspired by Rust, Go, Python, Ruby, and Elixir.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

</div>

## ✨ Features

- 🚀 **Fast performance** thanks to a Rust implementation
- 🎯 **Static typing with inference** for safety without verbosity
- 🌊 **Elegant syntax** balancing functional and imperative styles
- 🔧 **First-class functions** and immutable-by-default semantics
- 🔄 **Pipeline-friendly method calls** for ergonomic chaining
- 🎨 **Pattern matching** with guards
- 📦 **`Option` & `Result`** for predictable error handling
- 🔢 **Ranges & iterators** supporting inclusive/exclusive bounds
- 🧵 **String utilities** baked into the standard library
- 💡 **Interactive REPL** for rapid experimentation

## 📦 Installation

### From source

```bash
git clone https://github.com/yourusername/coral.git
cd coral
cargo build --release
# Binary available at target/release/coral
```

### With Cargo

```bash
cargo install coral-lang
```

## 🚀 Quick start

### Hello world

Create `hello.coral`:

```coral
fn main() do
  name = "World"
  println("Hello, {name}!")
end

main()
```

Run it:

```bash
coral hello.coral
```

### REPL mode

```bash
coral
```

```text
🪸 Coral REPL v0.1.0
coral> x = 10
10
coral> y = 20
20
coral> x + y
30
coral> quit
Goodbye! 🪸
```

## 🧠 Language snippets

### Variables and types

```coral
// Immutable by default
name = "Alice"
age = 30

// Explicit mutability
mut counter = 0
counter = counter + 1

// Type annotations
score: Int = 100
price: Float = 29.99
```

### Functions

```coral
fn add(x: Int, y: Int) -> Int do
  return x + y
end

// Expression-bodied function
fn double(x: Int) -> Int = x * 2

// Higher-order functions
fn map(list: List<Int>, f: (Int) -> Int) -> List<Int> do
  return list.map(f)
end
```

### Pattern matching

```coral
fn classify(x: Int) -> String do
  return match x do
    0 -> "zero"
    1 -> "one"
    n when n < 0 -> "negative"
    n when n > 100 -> "large"
    _ -> "normal"
  end
end
```

### Option and Result

```coral
fn divide(a: Int, b: Int) -> Result<Int, String> do
  if b == 0 do
    return Error("Division by zero")
  end
  return Ok(a / b)
end

result = divide(10, 2)
match result do
  Ok(value) -> println("Result: {value}")
  Error(msg) -> println("Error: {msg}")
end
```

### Pipelines and iterators

```coral
numbers = [1, 2, 3, 4, 5]

result = numbers
  .filter(|x| x % 2 == 0)
  .map(|x| x * 2)
  .sum()

println(result) // 12
```

### Ranges

```coral
// Exclusive range
for i in 1..10 do
  println(i) // 1..9
end

// Inclusive range
for i in 1..=10 do
  println(i) // 1..10
end
```

## 🛠️ CLI options

```bash
# Run a file
coral program.coral

# Parse only (syntax check)
coral program.coral --parse

# Skip type checking
coral program.coral --no-typecheck

# Start the REPL
coral
```

## 📚 Documentation

Full docs live at <https://coral-lang.org/docs>.

## 🗂️ Project layout

```text
coral/
├── src/
│   ├── main.rs          # CLI entry point
│   ├── lib.rs           # Library root
│   ├── lexer/           # Tokenization
│   ├── parser/          # AST construction
│   ├── evaluator/       # Tree-walking interpreter
│   ├── types/           # Type system
│   ├── error/           # Error handling
│   └── repl.rs          # Interactive REPL
├── examples/            # Sample Coral programs
└── tests/               # Integration tests
```

## 🗺️ Roadmap

- [x] Lexer and parser
- [x] Tree-walking interpreter
- [x] Type system with inference
- [x] Pattern matching
- [x] REPL mode
- [ ] Bytecode compiler
- [ ] Virtual machine
- [ ] Module system (file imports)
- [ ] Expanded standard library
- [ ] Async/Await
- [ ] Concurrency (actors)
- [ ] Package manager
- [ ] Native code generation (LLVM)
- [ ] Self-hosting compiler

## 🤝 Contributing

Contributions are welcome! See `CONTRIBUTING.md` for guidelines.

### Development setup

```bash
# Clone the repository
git clone https://github.com/yourusername/coral.git
cd coral

# Build
cargo build

# Run tests
cargo test

# Run examples
cargo run --release examples/hello.coral

# Format & lint
cargo fmt
cargo clippy
```

## ✅ Testing

```bash
# Run all tests
cargo test

# Run a specific test
cargo test test_arithmetic

# Show test output
cargo test -- --nocapture
```

## 📄 License

Licensed under either:

- MIT License (see `LICENSE-MIT` or <http://opensource.org/licenses/MIT>)

## 🙏 Acknowledgments

Coral draws inspiration from:

- Rust — memory safety and type system
- Go — simplicity and concurrency
- Python — ease of use
- Ruby — elegant syntax
- Elixir — functional patterns

## 🌐 Community

- Website: <https://coral-lang.org>
- Discord: *Join our Discord*
- Twitter: [@coral_lang](https://twitter.com/coral_lang)

## 👥 Authors

- Rafael Fragoso — initial work — `@orafaelfragoso`

See the full list of contributors on the GitHub project.

---

Made with 🪸 by the Coral community