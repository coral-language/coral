# Coral Development Roadmap

This roadmap outlines the complete development plan for Coral, a fast interpreted/compiled language with elegant syntax and Rust's memory safety.

## Documentation

Complete language specifications:
- `docs/module-system.md` - Module system, exports, and introspection
- `docs/classes-and-protocols.md` - Classes, protocols, operators, and special methods

## Phase 1: Frontend Development (Current)

### Parser Module

- [x] **F-String Parsing Enhancements**
  - [x] Triple-quoted f-strings support
  - [x] Raw f-strings (`rf""`) with proper escape handling
  - [x] Nested f-strings validation and depth limits
  - [x] Better error messages for unclosed braces
  - [x] Format spec validation beyond current implementation
  - [x] Multi-line f-strings with proper indentation handling

- [x] **Pattern Matching Completeness**
  - [x] Guard expression type validation in patterns
  - [x] Wildcard pattern validation (cannot appear in or-patterns)
  - [x] Class pattern validation (positional must come before keyword)
  - [x] Better error recovery for malformed patterns
  - [x] Validate star patterns only appear once per sequence

- [x] **Error Recovery Improvements**
  - [x] More sophisticated synchronization points
  - [x] Better recovery from missing colons in compound statements
  - [x] Recovery from unclosed delimiters with suggestions
  - [x] Cascading error prevention (don't report dependent errors)
  - [x] Smart error messages for common syntax mistakes

- [x] **Soft Keyword Handling**
  - [x] Ensure soft keywords work correctly in all contexts
  - [x] Validate they can still be used as variable names
  - [x] Add tests for edge cases (e.g., `match = 5`)

### Lexer Module

- [x] **String Literal Enhancements**
  - [ ] Triple-quoted strings (single and double, multi-line support pending)
  - [x] Raw strings (`r""`, `r''`)
  - [x] T-strings (`t""`, `t''`)
  - [x] Proper escape sequence handling and validation
  - [x] Unicode escape sequences (`\uXXXX`, `\UXXXXXXXX`)
  - [x] String prefix combinations validation (`fr""`, `rf""`, `tr""`, etc.)
  - [x] Better error messages for unclosed strings

- [x] **Number Literal Enhancements**
  - [x] Validate underscore positions (cannot be at start/end or consecutive)
  - [x] Better error messages for malformed numbers (e.g., `0x`, `0b`)
  - [x] Validate hex digits in hex literals
  - [x] Validate octal digits in octal literals
  - [x] Validate binary digits in binary literals

- [x] **Indentation Edge Cases**
  - [x] Mix of tabs and spaces detection and warnings
  - [x] Inconsistent indentation warnings
  - [x] Better error messages for indentation issues
  - [x] Handle edge case: dedent at EOF with complex nesting

- [x] **Comments and Docstrings**
  - [x] Preserve comments for documentation generation
  - [x] Extract docstrings (first string in function/class/module)
  - [x] Multi-line comment handling
  - [x] Comment attachment to AST nodes for IDE support

### Arena Module

- [x] **Arena Optimization**
  - [x] Memory usage statistics and reporting
  - [x] Arena reset functionality for REPL/incremental parsing
  - [x] Custom allocation strategies for different AST node sizes
  - [x] Memory pool reuse across multiple parse sessions

- [x] **Interner Improvements**
  - [x] Pre-intern common keywords and built-ins
  - [x] Thread-safe interner for parallel parsing
  - [x] Serialization/deserialization for caching
  - [x] Statistics on intern rates and collisions

### Visitor Module

- [x] **Mutable Visitor Implementation**
  - [x] Design transformation API that works with arena allocation
  - [x] AST transformation helpers (e.g., `transform_expr`, `transform_stmt`)
  - [x] Pattern for building new AST nodes from old ones

- [x] **Parallel Visitor Enhancements**
  - [x] Work-stealing for unbalanced trees
  - [x] Granularity control (when to parallelize vs sequential)
  - [x] Better error aggregation from parallel workers
  - [x] Progress tracking and cancellation support

- [x] **Walk Function Completeness**
  - [x] Ensure all AST node types have walk functions
  - [x] Validate all child nodes are visited
  - [x] Add tests for visitor pattern correctness

### Semantic Module - HIR

- [x] **HIR Design and Implementation**
  - [x] `TypedExpr` with all expression variants
  - [x] `TypedStmt` with all statement variants
  - [x] `TypedPattern` for match statements
  - [x] `TypedItem` for top-level declarations
  - [x] AST-to-HIR lowering with desugaring
  - [x] Resolve all name bindings to definition sites
  - [x] Compute class MRO (Method Resolution Order)
  - [x] Flatten inheritance hierarchies
  - [x] Validate type consistency during lowering

### Semantic Module - Parallel Analysis

- [x] **Parallel Analysis Infrastructure**
  - [x] `SyncSymbolTable` with `RwLock<SymbolTable>`
  - [x] Per-scope locking for fine-grained concurrency
  - [x] Lock-free reads where possible
  - [x] Atomic snapshots for consistent views
  - [x] Module dependency graph construction
  - [x] Topological sort for analysis order
  - [x] Parallel analysis of independent modules
  - [x] Error aggregation across threads
  - [x] Integration with `PassManager`

### Semantic Module - Type Inference

- [x] **Lambda Type Inference**
  - [x] Infer parameter types from call sites
  - [x] Infer return type from body
  - [x] Handle closures and captured variables
  - [x] Build proper `Type::Function` types

- [x] **Generator Types**
  - [x] Add `Type::Generator(Box<Type>)` to type system
  - [x] Distinguish generators from lists
  - [x] Handle yield expressions
  - [x] Track generator protocol methods

- [x] **Attribute Type Resolution**
  - [x] Build attribute tables for classes
  - [x] Handle instance vs class attributes
  - [x] Support property descriptors
  - [x] Chain attribute lookups (a.b.c.d)
  - [x] Cache attribute types

- [x] **Pattern Type Inference**
  - [x] Infer types from destructuring patterns
  - [x] Handle tuple/list/class patterns
  - [x] Support nested patterns recursively

- [x] **Flow-Sensitive Type Narrowing**
  - [x] `isinstance()` checks narrow types
  - [x] `is None` / `is not None` narrowing
  - [x] Truthiness-based narrowing
  - [x] Type guards support
  - [x] Integration with CFG (condition tracking complete)

### Semantic Module - Control Flow

- [x] **Control Flow Graph Implementation**
  - [x] Build CFG in `analyze_function()`
  - [x] Create basic blocks for statement sequences
  - [x] Add control flow edges (sequential, branch, loop, exception)
  - [x] Mark terminator blocks
  - [x] Reachability analysis via BFS/DFS on CFG
  - [x] Unreachable code detection
  - [x] Exception flow tracking
  - [x] Finally block validation
  - [x] Store CFGs per function for dataflow analysis
  - [x] Variable def/use tracking in blocks
  - [x] Condition extraction for type narrowing
  - [x] Dataflow analysis framework with worklist algorithm

- [x] **Definite Assignment Analysis**
  - [x] Track variable definitions per block (implemented in CFG)
  - [x] Dataflow analysis through CFG (framework ready)
  - [x] Report uninitialized variable usage
  - [x] Handle conditional initialization
  - [x] Integrate with semantic analyzer pass

- [x] **Constant Propagation**
  - [x] Track constant values through CFG (framework ready)
  - [x] Implement constant value lattice
  - [x] Evaluate constant conditionals
  - [x] Eliminate dead branches
  - [x] Fold constant expressions
  - [x] Integrate with semantic analyzer pass

### Semantic Module - Exhaustiveness

- [x] **Pattern Exhaustiveness Checking**
  - [x] Integer literal exhaustiveness with configurable sampling
  - [x] String literal exhaustiveness
  - [x] Nested pattern exhaustiveness with pattern matrix algorithm
  - [x] Or-pattern handling with expansion support
  - [x] Detect redundant patterns with O(n²) incremental checking
  - [x] Star pattern support for variable-length sequences
  - [x] Mapping pattern support with key tracking
  - [x] Guard handling (conservative exhaustiveness)
  - [x] Improved witness formatting for error messages

### Semantic Module - Type Checking

- [ ] **Custom Class Attributes**
  - [x] Build attribute/method tables for all classes
  - [x] Walk MRO for attribute lookup
  - [ ] Validate method signatures
  - [x] Handle attribute shadowing

- [ ] **Protocol Integration**
  - [x] Validate special methods (iter, enter, etc. with @operator decorator)
  - [ ] Structural subtyping checks
  - [ ] Protocol implementation completeness

- [ ] **Property Support**
  - [x] Detect `@property` and `@operator` decorators
  - [ ] Treat properties as attributes
  - [ ] Validate getter/setter/deleter

### Semantic Module - Module System

- [ ] **Cross-Module Validation**
  - [x] Load and parse imported modules
  - [x] Build module dependency graph
  - [ ] Validate re-export chains (`export X from Y`)
  - [x] Detect circular re-exports
  - [x] Cache parsed modules

### Semantic Module - Decorators

- [ ] **Decorator Validation**
  - [x] Validate decorator signatures match target type
  - [x] Function decorators: `Callable[[F], F]`
  - [x] Class decorators: `Callable[[Type[T]], Type[T]]`
  - [ ] Decorator factories with arguments
  - [x] Stacked decorator validation

### Semantic Module - Protocols

- [ ] **Protocol Signature Validation**
  - [ ] Parameter type compatibility (contravariant)
  - [ ] Return type compatibility (covariant)
  - [ ] Optional parameters validation
  - [ ] `*args` and `**kwargs` handling
  - [ ] Covariance for returns
  - [ ] Contravariance for parameters
  - [ ] Invariance for mutable attributes

### Semantic Module - Inferred Ownership

- [ ] **Smart Ownership Analysis**
  - [ ] Infer lifetimes without annotations
  - [ ] Track value ownership through expressions
  - [ ] Detect use-after-move without user annotations
  - [ ] Smart cleanup insertion
  - [ ] Attribute chains (`a.b.c`)
  - [ ] Subscript operations
  - [ ] Call expressions and returns
  - [ ] Temporary value lifetimes
  - [ ] Detect resource-like types (files, connections)
  - [ ] Ensure cleanup on all paths
  - [ ] Exception-safe resource handling
  - [ ] Generate cleanup code automatically
  - [ ] Define Copy vs Move types
  - [ ] Track moved values automatically
  - [ ] Generate efficient move code

### Semantic Module - Async/Await

- [ ] **Async Validation**
  - [ ] Remove thread-based concurrency code
  - [ ] Validate `.await` only in async functions
  - [ ] Check Future types
  - [ ] Detect blocking calls in async contexts
  - [ ] Validate async lifetimes across await points

### Cross-Cutting Improvements

- [ ] **Better Diagnostics**
  - [x] More precise source locations (column numbers)
  - [ ] Multi-line error highlighting
  - [ ] Contextual error messages
  - [x] Suggest fixes where possible

- [ ] **Incremental Analysis**
  - [ ] Cache analysis results per file
  - [ ] Re-analyze only changed modules
  - [ ] Track dependencies for invalidation
  - [ ] Serialize/deserialize caches

- [ ] **IDE Integration Support**
  - [ ] Partial results on incomplete code
  - [ ] Machine-readable diagnostics
  - [ ] Real-time analysis support
  - [ ] Quick fix suggestions

- [x] **Performance Instrumentation**
  - [x] Per-pass timing
  - [x] Memory profiling
  - [x] Identify bottlenecks
  - [x] Optimization targets

- [ ] **Configuration**
  - [ ] Per-pass strictness levels
  - [x] Ignore specific warnings by code
  - [x] Custom error formatting
  - [ ] Project-wide configuration files

## Phase 2: Backend Development

### Bytecode Interpreter

- [ ] **Bytecode Design**
  - [ ] Design efficient bytecode instruction set
  - [ ] Stack-based vs register-based VM
  - [ ] Instruction encoding and optimization
  - [ ] Debug information support

- [ ] **VM Implementation**
  - [ ] Core VM loop and execution engine
  - [ ] Stack management and frame handling
  - [ ] Exception handling and unwinding
  - [ ] No garbage collection

- [ ] **Standard Library**
  - [ ] Core built-in functions and types
  - [ ] Collections (list, dict, set, tuple)
  - [ ] I/O operations and file handling
  - [ ] String manipulation and formatting
  - [ ] Math and random number generation

### JIT Compilation

- [ ] **JIT Compiler**
  - [ ] Hot spot detection and compilation
  - [ ] Native code generation
  - [ ] Optimization passes
  - [ ] Fallback to interpreter

- [ ] **Optimization**
  - [ ] Inlining and constant folding
  - [ ] Dead code elimination
  - [ ] Loop optimization
  - [ ] Memory layout optimization

### Package Manager

- [ ] **Package System**
  - [ ] Package format and metadata
  - [ ] Dependency resolution
  - [ ] Version management
  - [ ] Installation and uninstallation

- [ ] **Repository**
  - [ ] Package registry
  - [ ] Search and discovery
  - [ ] Security and verification
  - [ ] Mirror support

## Phase 3: Tooling and Ecosystem

### Language Server

- [ ] **LSP Implementation**
  - [ ] Code completion and suggestions
  - [ ] Go to definition and references
  - [ ] Hover information and documentation
  - [ ] Error highlighting and diagnostics
  - [ ] Refactoring support

### Formatter

- [ ] **Code Formatting**
  - [ ] Consistent code style enforcement
  - [ ] Configurable formatting rules
  - [ ] Integration with editors
  - [ ] Batch formatting support

### Testing Framework

- [ ] **Test Runner**
  - [ ] Unit test framework
  - [ ] Integration testing
  - [ ] Mocking and fixtures
  - [ ] Coverage reporting

### Documentation Generator

- [ ] **Doc Generation**
  - [ ] API documentation from code
  - [ ] Tutorial and guide generation
  - [ ] Interactive examples
  - [ ] Cross-reference linking

## Phase 4: Performance and Optimization

### Performance Optimization

- [ ] **Parser Optimization**
  - [ ] Faster lexing and parsing
  - [ ] Memory usage optimization
  - [ ] Parallel parsing support
  - [ ] Incremental parsing

- [ ] **Runtime Optimization**
  - [ ] VM performance tuning
  - [ ] JIT optimization improvements
  - [ ] Memory management optimization
  - [ ] Cache optimization

### Benchmarking

- [ ] **Performance Testing**
  - [ ] Comprehensive benchmark suite
  - [ ] Performance regression testing
  - [ ] Comparison with other languages
  - [ ] Continuous performance monitoring

---

*This roadmap is a living document and will be updated as development progresses. For the most current information, check our [GitHub issues](https://github.com/your-org/coral/issues) and [project board](https://github.com/your-org/coral/projects).*
