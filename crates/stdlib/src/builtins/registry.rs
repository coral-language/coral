use crate::value::Value;
use std::collections::HashMap;

pub struct BuiltinRegistry {
    pub functions: HashMap<String, String>,
    pub globals: HashMap<String, Value>,
    pub exceptions: HashMap<String, String>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            functions: HashMap::new(),
            globals: HashMap::new(),
            exceptions: HashMap::new(),
        };

        registry.register_all();
        registry
    }

    fn register_all(&mut self) {
        self.register_functions();
        self.register_globals();
        self.register_exceptions();
    }

    fn register_functions(&mut self) {
        let funcs = vec![
            "print",
            "input",
            "open",
            "type",
            "isinstance",
            "issubclass",
            "callable",
            "getattr",
            "setattr",
            "hasattr",
            "delattr",
            "dir",
            "vars",
            "len",
            "sum",
            "min",
            "max",
            "sorted",
            "reversed",
            "iter",
            "next",
            "range",
            "enumerate",
            "zip",
            "map",
            "filter",
            "any",
            "all",
            "abs",
            "round",
            "pow",
            "divmod",
            "int",
            "float",
            "str",
            "bool",
            "bytes",
            "list",
            "tuple",
            "dict",
            "set",
            "frozenset",
            "hex",
            "oct",
            "bin",
            "chr",
            "ord",
            "ascii",
            "repr",
            "format",
            "hash",
            "id",
            "classmethod",
            "staticmethod",
            "property",
            "super",
        ];

        for func in funcs {
            self.functions.insert(func.to_string(), func.to_string());
        }
    }

    fn register_globals(&mut self) {
        self.globals.insert("True".to_string(), Value::Bool(true));
        self.globals.insert("False".to_string(), Value::Bool(false));
        self.globals.insert("None".to_string(), Value::None);
        self.globals
            .insert("Ellipsis".to_string(), Value::Str("...".to_string()));
        self.globals.insert(
            "NotImplemented".to_string(),
            Value::Str("NotImplemented".to_string()),
        );
    }

    fn register_exceptions(&mut self) {
        let exceptions = vec![
            "BaseException",
            "Exception",
            "GeneratorExit",
            "KeyboardInterrupt",
            "SystemExit",
            "StopIteration",
            "ValueError",
            "TypeError",
            "KeyError",
            "IndexError",
            "AttributeError",
            "NameError",
            "RuntimeError",
            "NotImplementedError",
            "SystemError",
            "OSError",
            "IOError",
            "MemoryError",
            "RecursionError",
            "ImportError",
            "ModuleNotFoundError",
            "SyntaxError",
            "IndentationError",
            "TabError",
            "ZeroDivisionError",
            "AssertionError",
            "Warning",
            "UserWarning",
            "DeprecationWarning",
            "SyntaxWarning",
            "RuntimeWarning",
            "FutureWarning",
            "PendingDeprecationWarning",
            "ImportWarning",
            "UnicodeWarning",
            "BytesWarning",
            "ResourceWarning",
        ];

        for exc in exceptions {
            self.exceptions.insert(exc.to_string(), exc.to_string());
        }
    }

    pub fn get_function(&self, name: &str) -> Option<&String> {
        self.functions.get(name)
    }

    pub fn get_global(&self, name: &str) -> Option<&Value> {
        self.globals.get(name)
    }

    pub fn get_exception(&self, name: &str) -> Option<&String> {
        self.exceptions.get(name)
    }

    pub fn is_builtin_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    pub fn is_builtin_global(&self, name: &str) -> bool {
        self.globals.contains_key(name)
    }

    pub fn is_exception(&self, name: &str) -> bool {
        self.exceptions.contains_key(name)
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
    fn test_registry_get_function_print() {
        let registry = BuiltinRegistry::new();
        let func = registry.get_function("print");
        assert!(func.is_some());
    }

    #[test]
    fn test_registry_get_function_len() {
        let registry = BuiltinRegistry::new();
        let func = registry.get_function("len");
        assert!(func.is_some());
    }

    #[test]
    fn test_registry_get_function_invalid() {
        let registry = BuiltinRegistry::new();
        let func = registry.get_function("nonexistent_function");
        assert!(func.is_none());
    }

    #[test]
    fn test_registry_get_global_true() {
        let registry = BuiltinRegistry::new();
        let global = registry.get_global("True");
        assert!(global.is_some());
        assert_eq!(*global.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_registry_get_global_false() {
        let registry = BuiltinRegistry::new();
        let global = registry.get_global("False");
        assert!(global.is_some());
        assert_eq!(*global.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_registry_get_global_none() {
        let registry = BuiltinRegistry::new();
        let global = registry.get_global("None");
        assert!(global.is_some());
        assert_eq!(*global.unwrap(), Value::None);
    }

    #[test]
    fn test_registry_get_global_invalid() {
        let registry = BuiltinRegistry::new();
        let global = registry.get_global("InvalidGlobal");
        assert!(global.is_none());
    }

    #[test]
    fn test_registry_get_exception_value_error() {
        let registry = BuiltinRegistry::new();
        let exc = registry.get_exception("ValueError");
        assert!(exc.is_some());
    }

    #[test]
    fn test_registry_get_exception_type_error() {
        let registry = BuiltinRegistry::new();
        let exc = registry.get_exception("TypeError");
        assert!(exc.is_some());
    }

    #[test]
    fn test_registry_get_exception_invalid() {
        let registry = BuiltinRegistry::new();
        let exc = registry.get_exception("InvalidException");
        assert!(exc.is_none());
    }

    #[test]
    fn test_registry_all_builtin_functions() {
        let registry = BuiltinRegistry::new();
        let functions = vec![
            "print",
            "input",
            "open",
            "type",
            "isinstance",
            "callable",
            "len",
            "sum",
            "min",
            "max",
            "sorted",
            "reversed",
            "range",
            "any",
            "all",
            "int",
            "float",
            "str",
            "bool",
            "list",
            "tuple",
            "dict",
            "set",
            "frozenset",
            "abs",
            "round",
            "pow",
            "divmod",
            "hex",
            "oct",
            "bin",
            "chr",
            "ord",
            "hash",
        ];

        for func_name in functions {
            assert!(
                registry.get_function(func_name).is_some(),
                "Function {} not found",
                func_name
            );
        }
    }

    #[test]
    fn test_registry_all_exceptions() {
        let registry = BuiltinRegistry::new();
        let exceptions = vec![
            "BaseException",
            "Exception",
            "ValueError",
            "TypeError",
            "KeyError",
            "IndexError",
        ];

        for exc_name in exceptions {
            assert!(
                registry.get_exception(exc_name).is_some(),
                "Exception {} not found",
                exc_name
            );
        }
    }

    #[test]
    fn test_registry_globals_ellipsis() {
        let registry = BuiltinRegistry::new();
        let ellipsis = registry.get_global("Ellipsis");
        assert!(ellipsis.is_some());
    }

    #[test]
    fn test_registry_not_implemented() {
        let registry = BuiltinRegistry::new();
        let not_impl = registry.get_global("NotImplemented");
        assert!(not_impl.is_some());
    }
}
