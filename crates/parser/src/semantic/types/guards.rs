//! Type guards for user-defined type narrowing functions.
//!
//! Type guards allow users to define functions that narrow types:
//! ```python
//! def is_string(val: Any) -> TypeGuard[str]:
//!     return isinstance(val, str)
//! ```

use super::Type;
use std::collections::HashMap;

/// Registry of type guard functions
#[derive(Debug, Clone)]
pub struct TypeGuardRegistry {
    /// Maps function name to the type it guards for
    guards: HashMap<String, Type>,
}

impl TypeGuardRegistry {
    /// Create a new empty type guard registry
    pub fn new() -> Self {
        Self {
            guards: HashMap::new(),
        }
    }

    /// Register a type guard function
    ///
    /// # Arguments
    /// * `function_name` - The name of the function
    /// * `guarded_type` - The type that the guard narrows to
    pub fn register_guard(&mut self, function_name: String, guarded_type: Type) {
        self.guards.insert(function_name, guarded_type);
    }

    /// Check if a function is a type guard
    ///
    /// # Arguments
    /// * `function_name` - The name of the function to check
    ///
    /// # Returns
    /// True if the function is registered as a type guard
    pub fn is_type_guard(&self, function_name: &str) -> bool {
        self.guards.contains_key(function_name)
    }

    /// Get the type that a guard function narrows to
    ///
    /// # Arguments
    /// * `function_name` - The name of the guard function
    ///
    /// # Returns
    /// The type that the guard narrows to, if it's a registered guard
    pub fn get_guarded_type(&self, function_name: &str) -> Option<&Type> {
        self.guards.get(function_name)
    }

    /// Remove a type guard registration
    pub fn unregister_guard(&mut self, function_name: &str) {
        self.guards.remove(function_name);
    }

    /// Get all registered type guard function names
    pub fn all_guards(&self) -> Vec<&String> {
        self.guards.keys().collect()
    }
}

impl Default for TypeGuardRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Extract type guard information from a function return type annotation
///
/// # Arguments
/// * `return_annotation` - The return type annotation
///
/// # Returns
/// The guarded type if the annotation is TypeGuard[T], otherwise None
pub fn extract_type_guard(return_annotation: &Type) -> Option<Type> {
    // Check if this is a TypeGuard[T] type
    // This would need proper generic type handling
    match return_annotation {
        Type::Generic { base, params } => {
            // Check if base is TypeGuard
            if let Type::Class(name) = base.as_ref()
                && name == "TypeGuard"
                && params.len() == 1
            {
                return Some(params[0].clone());
            }
            None
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_register_and_check_guard() {
        let mut registry = TypeGuardRegistry::new();

        registry.register_guard("is_string".to_string(), Type::Str);

        assert!(registry.is_type_guard("is_string"));
        assert!(!registry.is_type_guard("other_function"));

        let guarded_type = registry.get_guarded_type("is_string").unwrap();
        assert_eq!(*guarded_type, Type::Str);
    }

    #[test]
    fn test_unregister_guard() {
        let mut registry = TypeGuardRegistry::new();

        registry.register_guard("is_int".to_string(), Type::Int);
        assert!(registry.is_type_guard("is_int"));

        registry.unregister_guard("is_int");
        assert!(!registry.is_type_guard("is_int"));
    }

    #[test]
    fn test_extract_type_guard() {
        let type_guard_annotation = Type::Generic {
            base: Box::new(Type::Class("TypeGuard".to_string())),
            params: vec![Type::Str],
        };

        let extracted = extract_type_guard(&type_guard_annotation);
        assert_eq!(extracted, Some(Type::Str));
    }

    #[test]
    fn test_extract_non_guard() {
        let normal_type = Type::Bool;
        let extracted = extract_type_guard(&normal_type);
        assert_eq!(extracted, None);
    }
}
