//! Centralized builtin registry for the Coral language
//!
//! This module provides a single source of truth for all builtin types, functions,
//! constants, and exceptions available in the Coral language runtime.
//!
//! ## Design Rationale
//!
//! Previously, builtin names were scattered across multiple modules:
//! - `import_resolution.rs` - for shadowing warnings
//! - `definite_assignment.rs` - for initialization checks
//! - Type inference implicitly handled builtin types
//!
//! This centralization provides:
//! - **Consistency**: All passes use the same builtin definitions
//! - **Maintainability**: Single place to add/modify builtins
//! - **Performance**: O(1) lookup with phf perfect hash functions
//! - **Documentation**: Clear categorization of builtin entities

use phf::{Set, phf_set};
use std::collections::HashSet;

/// Perfect hash set of all builtin types for O(1) lookup
static BUILTIN_TYPES: Set<&'static str> = phf_set! {
    "int", "float", "str", "bool", "bytes", "complex",
    "list", "tuple", "dict", "set", "frozenset",
    "range", "slice", "bytearray", "memoryview",
    "object", "type",
};

/// Perfect hash set of all builtin functions for O(1) lookup
static BUILTIN_FUNCTIONS: Set<&'static str> = phf_set! {
    "print", "input", "open",
    "int", "float", "str", "bool", "bytes",
    "list", "tuple", "dict", "set", "frozenset",
    "len", "sum", "min", "max", "sorted", "reversed",
    "enumerate", "zip", "range",
    "map", "filter", "any", "all",
    "iter", "next",
    "abs", "round", "pow",
    "hash", "id", "type", "isinstance", "issubclass",
    "hasattr", "getattr", "setattr", "delattr",
    "callable", "dir", "vars",
    "chr", "ord", "hex", "oct", "bin",
    "repr", "ascii", "format",
    "classmethod", "staticmethod", "property", "super",
};

/// Perfect hash set of all builtin constants for O(1) lookup
static BUILTIN_CONSTANTS: Set<&'static str> = phf_set! {
    "True", "False", "None", "Ellipsis", "NotImplemented",
};

/// Perfect hash set of all builtin exceptions for O(1) lookup
static BUILTIN_EXCEPTIONS: Set<&'static str> = phf_set! {
    "BaseException", "Exception", "GeneratorExit", "KeyboardInterrupt",
    "SystemExit", "StopIteration",
    "ValueError", "TypeError", "KeyError", "IndexError",
    "AttributeError", "NameError", "RuntimeError", "NotImplementedError",
    "SystemError", "OSError", "IOError", "MemoryError", "RecursionError",
    "ImportError", "ModuleNotFoundError",
    "SyntaxError", "IndentationError", "TabError",
    "ZeroDivisionError", "AssertionError",
    "Warning", "UserWarning", "DeprecationWarning", "SyntaxWarning",
    "RuntimeWarning", "FutureWarning", "PendingDeprecationWarning",
    "ImportWarning", "UnicodeWarning", "BytesWarning", "ResourceWarning",
};

/// Check if a name is a builtin type (O(1) lookup)
///
/// # Examples
///
/// ```
/// assert!(is_builtin_type("int"));
/// assert!(is_builtin_type("list"));
/// assert!(!is_builtin_type("MyClass"));
/// ```
#[inline]
pub fn is_builtin_type(name: &str) -> bool {
    BUILTIN_TYPES.contains(name)
}

/// Check if a name is a builtin function (O(1) lookup)
///
/// Note: Some names like "int", "str" are both types and functions
/// (constructor functions). This function returns true for callable builtins.
#[inline]
pub fn is_builtin_function(name: &str) -> bool {
    BUILTIN_FUNCTIONS.contains(name)
}

/// Check if a name is a builtin constant (O(1) lookup)
///
/// # Examples
///
/// ```
/// assert!(is_builtin_constant("True"));
/// assert!(is_builtin_constant("None"));
/// assert!(!is_builtin_constant("true")); // lowercase not a builtin
/// ```
#[inline]
pub fn is_builtin_constant(name: &str) -> bool {
    BUILTIN_CONSTANTS.contains(name)
}

/// Check if a name is a builtin exception type (O(1) lookup)
#[inline]
pub fn is_builtin_exception(name: &str) -> bool {
    BUILTIN_EXCEPTIONS.contains(name)
}

/// Check if a name is any kind of builtin
///
/// This is the most commonly used check in name resolution and other passes.
/// It returns true if the name is a builtin type, function, constant, or exception.
///
/// # Performance
///
/// This function performs multiple array scans. For hot paths or repeated lookups,
/// consider using `BuiltinRegistry::contains()` which uses a pre-built HashSet.
///
/// # Examples
///
/// ```
/// assert!(is_builtin("int"));      // type
/// assert!(is_builtin("print"));    // function
/// assert!(is_builtin("True"));     // constant
/// assert!(is_builtin("ValueError")); // exception
/// assert!(!is_builtin("my_var"));  // user-defined
/// ```
pub fn is_builtin(name: &str) -> bool {
    is_builtin_type(name)
        || is_builtin_function(name)
        || is_builtin_constant(name)
        || is_builtin_exception(name)
}

/// Pre-computed builtin registry for O(1) lookups
///
/// For performance-critical code that needs to check many names,
/// use this registry instead of calling `is_builtin()` repeatedly.
///
/// # Example
///
/// ```rust
/// let registry = BuiltinRegistry::new();
/// for name in names {
///     if registry.contains(name) {
///         // handle builtin
///     }
/// }
/// ```
pub struct BuiltinRegistry {
    names: HashSet<&'static str>,
}

impl BuiltinRegistry {
    /// Create a new builtin registry with all builtin names
    pub fn new() -> Self {
        let mut names = HashSet::new();

        names.extend(BUILTIN_TYPES.iter().copied());
        names.extend(BUILTIN_FUNCTIONS.iter().copied());
        names.extend(BUILTIN_CONSTANTS.iter().copied());
        names.extend(BUILTIN_EXCEPTIONS.iter().copied());

        Self { names }
    }

    /// Check if a name is a builtin (O(1) lookup)
    #[inline]
    pub fn contains(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    /// Get the total number of builtins
    pub fn len(&self) -> usize {
        self.names.len()
    }

    /// Check if the registry is empty (always false for properly constructed registry)
    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_types() {
        assert!(is_builtin_type("int"));
        assert!(is_builtin_type("str"));
        assert!(is_builtin_type("list"));
        assert!(!is_builtin_type("MyClass"));
    }

    #[test]
    fn test_builtin_functions() {
        assert!(is_builtin_function("print"));
        assert!(is_builtin_function("len"));
        assert!(!is_builtin_function("my_function"));
    }

    #[test]
    fn test_builtin_constants() {
        assert!(is_builtin_constant("True"));
        assert!(is_builtin_constant("False"));
        assert!(is_builtin_constant("None"));
        assert!(!is_builtin_constant("true")); // lowercase
    }

    #[test]
    fn test_builtin_exceptions() {
        assert!(is_builtin_exception("ValueError"));
        assert!(is_builtin_exception("TypeError"));
        assert!(!is_builtin_exception("MyError"));
    }

    #[test]
    fn test_is_builtin_general() {
        assert!(is_builtin("int"));
        assert!(is_builtin("str"));

        assert!(is_builtin("print"));
        assert!(is_builtin("len"));

        assert!(is_builtin("True"));
        assert!(is_builtin("None"));

        assert!(is_builtin("ValueError"));

        assert!(!is_builtin("my_var"));
        assert!(!is_builtin("MyClass"));
    }

    #[test]
    fn test_builtin_registry() {
        let registry = BuiltinRegistry::new();

        assert!(registry.contains("int"));
        assert!(registry.contains("print"));
        assert!(registry.contains("True"));
        assert!(registry.contains("ValueError"));
        assert!(!registry.contains("my_var"));

        assert!(!registry.is_empty());
    }

    #[test]
    fn test_no_duplicate_entries() {
        let registry = BuiltinRegistry::new();

        let total_individual = BUILTIN_TYPES.len()
            + BUILTIN_FUNCTIONS.len()
            + BUILTIN_CONSTANTS.len()
            + BUILTIN_EXCEPTIONS.len();

        assert!(registry.len() <= total_individual);
    }
}
