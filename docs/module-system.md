# Coral Module System

Coral uses a modern, explicit module system designed for clarity and simplicity. This guide covers the complete module system including exports, imports, and module introspection.

## Table of Contents

- [Overview](#overview)
- [Module Structure](#module-structure)
- [Export Syntax](#export-syntax)
- [Module Introspection](#module-introspection)
- [Examples](#examples)

## Overview

Coral's module system follows these principles:

1. **Private by default**: All definitions are private unless explicitly exported
2. **Explicit exports**: Use the `export` keyword to make names available to other modules
3. **Folder-based namespaces**: Folders represent namespaces, files are modules
4. **Clean introspection**: Use `module::` prefix for module metadata access

## Module Structure

### Files and Folders

- **Modules**: `.coral` files are modules
- **Namespaces**: Folders represent namespaces
- **Entry points**: `main.coral` or use `@entry` decorator

```
myproject/
├── main.coral           # Entry point
├── utils.coral          # Module: utils
├── models/              # Namespace: models
│   ├── user.coral       # Module: models.user
│   └── post.coral       # Module: models.post
└── lib/
    └── helpers.coral    # Module: lib.helpers
```

### Collision Prevention

You **cannot** have both `foo.coral` and `foo/` directory at the same level:

```
❌ INVALID:
myproject/
├── config.coral         # File named "config"
└── config/              # Folder named "config" - CONFLICT!
    └── settings.coral
```

## Export Syntax

### Basic Export

Export one or more names:

```coral
fn calculate(x: int, y: int) -> int {
    return x + y
}

let PI = 3.14159

export calculate, PI
```

### Export with Aliases

Rename exports for external consumers:

```coral
fn internal_helper() -> str {
    return "helper"
}

export internal_helper as helper
```

### Re-exports from Other Modules

Export names from another module (useful for creating public APIs):

```coral
# In api.coral - create a unified public interface
export User, Post from models
export validate, sanitize from utils
```

This allows users to import from `api` instead of knowing internal module structure:

```coral
# Users can do:
from api import User, validate

# Instead of:
from models import User
from utils import validate
```

#### Re-export Validation

Coral validates re-export statements to ensure:

1. **Source module exports exist**: The names you're re-exporting must actually be exported by the source module
2. **No circular re-exports**: You cannot re-export from your own module
3. **No duplicate exports**: Each name can only be exported once (even across regular and re-exports)

**Valid re-export:**
```coral
# models.coral exports User
class User:
    pass
export User

# api.coral can re-export it
export User from models  # ✅ Valid
```

**Invalid re-export:**
```coral
# models.coral does NOT export Admin
class User:
    pass
export User

# api.coral tries to re-export non-existent name
export Admin from models  # ❌ Error: Admin not exported from models
```

### Export Rules

1. **Exported names must be defined** in the current module or imported from another module
2. **No duplicate exports**: Can't export the same name twice
3. **Exports are always public**: Once exported, names are accessible to importers

### Valid Export Examples

```coral
# Export single name
let VERSION = "1.0"
export VERSION

# Export multiple names
fn add(a, b) { return a + b }
fn sub(a, b) { return a - b }
export add, sub

# Export with alias
class InternalUser {
    # ...
}
export InternalUser as User

# Re-export from module
export config, settings from app.config
```

### Invalid Export Examples

```coral
# ❌ Error: Exporting undefined name
export undefined_function

# ❌ Error: Duplicate export
export foo
export foo

# ❌ Error: Re-exporting name that doesn't exist in source module
export NonExistent from models

# ❌ Error: Circular re-export (exporting from self)
export func from mymodule
```

## Module Introspection

Coral provides built-in module introspection via the `module::` prefix.

### Available Functions

#### `module::is_main() -> bool`

Returns `true` if the current module is the entry point:

```coral
if module::is_main() {
    print("This is the main module")
    run_application()
}
```

#### `module::name() -> str`

Returns the fully qualified module name:

```coral
# In file: models/user.coral
print(module::name())  # Output: "models.user"
```

#### `module::path() -> str`

Returns the file system path to the current module:

```coral
print(module::path())  # Output: "/path/to/project/models/user.coral"
```

### Introspection Examples

```coral
# Conditional execution based on entry point
if module::is_main() {
    # Run tests or main application
    run_tests()
}

# Debug logging with module context
fn debug(message: str) {
    print(f"[{module::name()}] {message}")
}

# Resource loading relative to module
fn load_config() {
    let base_path = module::path()
    # Load config relative to this module
}
```

### Invalid Introspection

```coral
# ❌ Error: Unknown introspection function
module::version()

# ❌ Error: Not a function call
let name = module::name

# ✅ Correct: Must be called
let name = module::name()
```

## Examples

### Example 1: Simple Library Module

**math_utils.coral**:
```coral
fn add(a: int, b: int) -> int {
    return a + b
}

fn multiply(a: int, b: int) -> int {
    return a * b
}

fn internal_calculation(x: int) -> int {
    # Private helper - not exported
    return x * 2 + 1
}

export add, multiply
```

**Usage**:
```coral
from math_utils import add, multiply

let result = add(5, 3)      # ✅ OK
let product = multiply(2, 4) # ✅ OK
# internal_calculation(10)   # ❌ Error: not exported
```

### Example 2: Module with Aliases

**database.coral**:
```coral
class DatabaseConnection {
    fn connect(self, url: str) {
        # ...
    }
}

class QueryBuilder {
    fn select(self, table: str) {
        # ...
    }
}

export DatabaseConnection as Connection
export QueryBuilder as Query
```

**Usage**:
```coral
from database import Connection, Query

let conn = Connection()
let query = Query()
```

### Example 3: API Facade Pattern

**models/user.coral**:
```coral
class User {
    # User implementation
}
export User
```

**models/post.coral**:
```coral
class Post {
    # Post implementation
}
export Post
```

**api.coral** (public interface):
```coral
export User from models.user
export Post from models.post

# Additional API utilities
fn create_session() {
    # ...
}
export create_session
```

**Usage**:
```coral
# Clean public API - users don't need to know internal structure
from api import User, Post, create_session
```

### Example 4: Entry Point Pattern

**main.coral**:
```coral
from app import run_server
from utils import setup_logging

fn main() {
    setup_logging()
    run_server()
}

if module::is_main() {
    main()
}
```

This pattern allows `main.coral` to be imported for testing without auto-execution.

### Example 5: Namespace Organization

```
myapp/
├── main.coral
├── core/
│   ├── engine.coral
│   └── config.coral
├── models/
│   ├── user.coral
│   └── post.coral
└── utils/
    ├── validators.coral
    └── formatters.coral
```

**core/engine.coral**:
```coral
class Engine {
    # Implementation
}
export Engine
```

**models/user.coral**:
```coral
from core.engine import Engine

class User {
    fn __init__(self) {
        self.engine = Engine()
    }
}
export User
```

**main.coral**:
```coral
from models.user import User
from core.engine import Engine
from utils.validators import validate_email

if module::is_main() {
    print(f"Starting application: {module::name()}")
    # Run application
}
```

## Summary

- Use `export` keyword to make names public
- All names are private by default
- Use `module::is_main()`, `module::name()`, `module::path()` for introspection
- Organize code with folders as namespaces
- Create clean public APIs with re-exports
- No magic methods - everything is explicit and clear
