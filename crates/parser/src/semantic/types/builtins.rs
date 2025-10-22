//! Built-in type attribute registry
//!
//! This module provides attribute lookups for built-in types like str, list, dict, etc.
//! It serves as a registry of known methods and attributes for type inference.

use once_cell::sync::Lazy;
use std::collections::HashMap;

use super::context::Type;

/// Registry of built-in type attributes
pub struct BuiltinAttributeRegistry {
    /// String methods and attributes
    str_attributes: HashMap<&'static str, Type>,
    /// List methods and attributes
    list_attributes: HashMap<&'static str, Type>,
    /// Dictionary methods and attributes
    dict_attributes: HashMap<&'static str, Type>,
    /// Integer methods and attributes
    int_attributes: HashMap<&'static str, Type>,
    /// Float methods and attributes
    float_attributes: HashMap<&'static str, Type>,
    /// Boolean methods and attributes
    bool_attributes: HashMap<&'static str, Type>,
    /// Set methods and attributes
    set_attributes: HashMap<&'static str, Type>,
    /// Tuple methods and attributes
    tuple_attributes: HashMap<&'static str, Type>,
    /// Bytes methods and attributes
    bytes_attributes: HashMap<&'static str, Type>,
    /// Complex methods and attributes
    complex_attributes: HashMap<&'static str, Type>,
}

impl BuiltinAttributeRegistry {
    /// Create a new builtin attribute registry with all standard methods
    pub fn new() -> Self {
        Self {
            str_attributes: Self::init_str_attributes(),
            list_attributes: Self::init_list_attributes(),
            dict_attributes: Self::init_dict_attributes(),
            int_attributes: Self::init_int_attributes(),
            float_attributes: Self::init_float_attributes(),
            bool_attributes: Self::init_bool_attributes(),
            set_attributes: Self::init_set_attributes(),
            tuple_attributes: Self::init_tuple_attributes(),
            bytes_attributes: Self::init_bytes_attributes(),
            complex_attributes: Self::init_complex_attributes(),
        }
    }

    /// Look up a builtin attribute by type and attribute name
    pub fn lookup_builtin_attribute(&self, ty: &Type, attr: &str) -> Option<Type> {
        match ty {
            Type::Str => self.str_attributes.get(attr).cloned(),
            Type::List(_) => self.list_attributes.get(attr).cloned(),
            Type::Dict(_, _) => self.dict_attributes.get(attr).cloned(),
            Type::Int => self.int_attributes.get(attr).cloned(),
            Type::Float => self.float_attributes.get(attr).cloned(),
            Type::Bool => self.bool_attributes.get(attr).cloned(),
            Type::Set(_) => self.set_attributes.get(attr).cloned(),
            Type::Tuple(_) => self.tuple_attributes.get(attr).cloned(),
            Type::Bytes => self.bytes_attributes.get(attr).cloned(),
            Type::Complex => self.complex_attributes.get(attr).cloned(),
            _ => None,
        }
    }

    /// Initialize string attributes and methods
    fn init_str_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // String methods that return strings
        for method in [
            "upper",
            "lower",
            "title",
            "capitalize",
            "casefold",
            "swapcase",
        ] {
            attrs.insert(method, Type::function(vec![], Type::Str));
        }

        // String methods that return integers
        attrs.insert("len", Type::function(vec![], Type::Int));
        for method in ["count", "find", "rfind", "index", "rindex"] {
            attrs.insert(method, Type::function(vec![Type::Str], Type::Int));
        }

        // String methods that return booleans
        for method in [
            "startswith",
            "endswith",
            "isalnum",
            "isalpha",
            "isascii",
            "isdecimal",
            "isdigit",
            "isidentifier",
            "islower",
            "isnumeric",
            "isprintable",
            "isspace",
            "istitle",
            "isupper",
        ] {
            attrs.insert(method, Type::function(vec![], Type::Bool));
        }

        // String methods that return lists
        attrs.insert("split", Type::function(vec![], Type::list(Type::Str)));
        attrs.insert("rsplit", Type::function(vec![], Type::list(Type::Str)));
        attrs.insert("splitlines", Type::function(vec![], Type::list(Type::Str)));

        // String methods with various return types
        attrs.insert("strip", Type::function(vec![], Type::Str));
        attrs.insert("lstrip", Type::function(vec![], Type::Str));
        attrs.insert("rstrip", Type::function(vec![], Type::Str));
        attrs.insert(
            "replace",
            Type::function(vec![Type::Str, Type::Str], Type::Str),
        );
        attrs.insert(
            "join",
            Type::function(vec![Type::list(Type::Str)], Type::Str),
        );
        attrs.insert("format", Type::function(vec![], Type::Str)); // Simplified
        attrs.insert("encode", Type::function(vec![], Type::Bytes));
        attrs.insert("center", Type::function(vec![Type::Int], Type::Str));
        attrs.insert("ljust", Type::function(vec![Type::Int], Type::Str));
        attrs.insert("rjust", Type::function(vec![Type::Int], Type::Str));
        attrs.insert("zfill", Type::function(vec![Type::Int], Type::Str));

        attrs
    }

    /// Initialize list attributes and methods
    fn init_list_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // List methods that return None (modify in place)
        for method in [
            "append", "extend", "insert", "remove", "pop", "clear", "reverse", "sort",
        ] {
            attrs.insert(method, Type::function(vec![], Type::None));
        }

        // List methods that return integers
        attrs.insert("count", Type::function(vec![], Type::Int));
        attrs.insert("index", Type::function(vec![], Type::Int));
        attrs.insert("len", Type::function(vec![], Type::Int));

        // List methods that return booleans
        attrs.insert("contains", Type::function(vec![], Type::Bool));

        // List methods that return copies
        attrs.insert(
            "copy",
            Type::function(vec![], Type::List(Box::new(Type::Unknown))),
        );

        attrs
    }

    /// Initialize dictionary attributes and methods
    fn init_dict_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Dict methods that return None (modify in place)
        for method in ["clear", "pop", "popitem", "update"] {
            attrs.insert(method, Type::function(vec![], Type::None));
        }

        // Dict methods that return integers
        attrs.insert("len", Type::function(vec![], Type::Int));

        // Dict methods that return booleans
        attrs.insert("contains", Type::function(vec![], Type::Bool));

        // Dict methods that return keys/values/items
        attrs.insert("keys", Type::function(vec![], Type::list(Type::Unknown)));
        attrs.insert("values", Type::function(vec![], Type::list(Type::Unknown)));
        attrs.insert(
            "items",
            Type::function(
                vec![],
                Type::list(Type::tuple(vec![Type::Unknown, Type::Unknown])),
            ),
        );

        // Dict methods that return values
        attrs.insert("get", Type::function(vec![], Type::Unknown));
        attrs.insert("setdefault", Type::function(vec![], Type::Unknown));

        attrs
    }

    /// Initialize integer attributes and methods
    fn init_int_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Int methods that return booleans
        for method in ["is_integer", "is_even", "is_odd"] {
            // Python-like methods
            attrs.insert(method, Type::function(vec![], Type::Bool));
        }

        attrs
    }

    /// Initialize float attributes and methods
    fn init_float_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Float methods that return booleans
        for method in ["is_integer", "is_finite", "is_infinite", "is_nan"] {
            attrs.insert(method, Type::function(vec![], Type::Bool));
        }

        attrs
    }

    /// Initialize boolean attributes and methods
    fn init_bool_attributes() -> HashMap<&'static str, Type> {
        HashMap::new() // Boolean has few special methods
    }

    /// Initialize set attributes and methods
    fn init_set_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Set methods that return None (modify in place)
        for method in ["add", "remove", "discard", "clear", "pop", "update"] {
            attrs.insert(method, Type::function(vec![], Type::None));
        }

        // Set methods that return integers
        attrs.insert("len", Type::function(vec![], Type::Int));

        // Set methods that return booleans
        attrs.insert("contains", Type::function(vec![], Type::Bool));
        attrs.insert(
            "issubset",
            Type::function(vec![Type::Set(Box::new(Type::Unknown))], Type::Bool),
        );
        attrs.insert(
            "issuperset",
            Type::function(vec![Type::Set(Box::new(Type::Unknown))], Type::Bool),
        );
        attrs.insert(
            "isdisjoint",
            Type::function(vec![Type::Set(Box::new(Type::Unknown))], Type::Bool),
        );

        // Set methods that return new sets
        attrs.insert(
            "union",
            Type::function(vec![], Type::Set(Box::new(Type::Unknown))),
        );
        attrs.insert(
            "intersection",
            Type::function(vec![], Type::Set(Box::new(Type::Unknown))),
        );
        attrs.insert(
            "difference",
            Type::function(vec![], Type::Set(Box::new(Type::Unknown))),
        );
        attrs.insert(
            "symmetric_difference",
            Type::function(vec![], Type::Set(Box::new(Type::Unknown))),
        );

        attrs.insert(
            "copy",
            Type::function(vec![], Type::Set(Box::new(Type::Unknown))),
        );

        attrs
    }

    /// Initialize tuple attributes and methods
    fn init_tuple_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Tuple methods that return integers
        attrs.insert("count", Type::function(vec![], Type::Int));
        attrs.insert("index", Type::function(vec![], Type::Int));
        attrs.insert("len", Type::function(vec![], Type::Int));

        // Tuple methods that return booleans
        attrs.insert("contains", Type::function(vec![], Type::Bool));

        attrs
    }

    /// Initialize bytes attributes and methods
    fn init_bytes_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Bytes methods that return integers
        attrs.insert("len", Type::function(vec![], Type::Int));

        // Bytes methods that return booleans
        attrs.insert("contains", Type::function(vec![], Type::Bool));

        // Bytes methods that return bytes
        attrs.insert("decode", Type::function(vec![], Type::Str));

        attrs
    }

    /// Initialize complex attributes and methods
    fn init_complex_attributes() -> HashMap<&'static str, Type> {
        let mut attrs = HashMap::new();

        // Complex attributes
        attrs.insert("real", Type::Float);
        attrs.insert("imag", Type::Float);

        attrs
    }
}

impl Default for BuiltinAttributeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Global builtin attribute registry instance
pub static BUILTIN_ATTRIBUTE_REGISTRY: Lazy<BuiltinAttributeRegistry> =
    Lazy::new(BuiltinAttributeRegistry::new);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::types::Type;

    #[test]
    fn test_str_attributes() {
        let registry = BuiltinAttributeRegistry::new();

        // Test string methods that return strings
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "upper"),
            Some(Type::function(vec![], Type::Str))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "lower"),
            Some(Type::function(vec![], Type::Str))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "strip"),
            Some(Type::function(vec![], Type::Str))
        );

        // Test string methods that return integers
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "len"),
            Some(Type::function(vec![], Type::Int))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "count"),
            Some(Type::function(vec![Type::Str], Type::Int))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "find"),
            Some(Type::function(vec![Type::Str], Type::Int))
        );

        // Test string methods that return booleans
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "startswith"),
            Some(Type::function(vec![], Type::Bool))
        );

        // Test non-existent attribute
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Str, "nonexistent"),
            None
        );
    }

    #[test]
    fn test_list_attributes() {
        let registry = BuiltinAttributeRegistry::new();

        // Test list methods that return None (modify in place)
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::list(Type::Int), "append"),
            Some(Type::function(vec![], Type::None))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::list(Type::Int), "clear"),
            Some(Type::function(vec![], Type::None))
        );

        // Test list methods that return integers
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::list(Type::Int), "count"),
            Some(Type::function(vec![], Type::Int))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::list(Type::Int), "len"),
            Some(Type::function(vec![], Type::Int))
        );

        // Test list methods that return booleans
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::list(Type::Int), "contains"),
            Some(Type::function(vec![], Type::Bool))
        );
    }

    #[test]
    fn test_dict_attributes() {
        let registry = BuiltinAttributeRegistry::new();

        // Test dict methods that return None
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::dict(Type::Str, Type::Int), "clear"),
            Some(Type::function(vec![], Type::None))
        );

        // Test dict methods that return integers
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::dict(Type::Str, Type::Int), "len"),
            Some(Type::function(vec![], Type::Int))
        );

        // Test dict methods that return collections
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::dict(Type::Str, Type::Int), "keys"),
            Some(Type::function(vec![], Type::list(Type::Unknown)))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::dict(Type::Str, Type::Int), "values"),
            Some(Type::function(vec![], Type::list(Type::Unknown)))
        );
    }

    #[test]
    fn test_set_attributes() {
        let registry = BuiltinAttributeRegistry::new();

        // Test set methods that return None
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::set(Type::Int), "add"),
            Some(Type::function(vec![], Type::None))
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::set(Type::Int), "clear"),
            Some(Type::function(vec![], Type::None))
        );

        // Test set methods that return integers
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::set(Type::Int), "len"),
            Some(Type::function(vec![], Type::Int))
        );

        // Test set methods that return booleans
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::set(Type::Int), "contains"),
            Some(Type::function(vec![], Type::Bool))
        );
    }

    #[test]
    fn test_non_builtin_types() {
        let registry = BuiltinAttributeRegistry::new();

        // Test that non-builtin types return None
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Int, "some_attr"),
            None
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Unknown, "some_attr"),
            None
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Any, "some_attr"),
            None
        );
    }

    #[test]
    fn test_complex_attributes() {
        let registry = BuiltinAttributeRegistry::new();

        // Test complex number attributes
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Complex, "real"),
            Some(Type::Float)
        );
        assert_eq!(
            registry.lookup_builtin_attribute(&Type::Complex, "imag"),
            Some(Type::Float)
        );
    }
}
