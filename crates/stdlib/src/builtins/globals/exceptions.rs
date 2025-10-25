use std::fmt;

#[derive(Debug, Clone)]
pub struct ExceptionType {
    pub name: &'static str,
    pub base: Option<&'static str>,
    pub message: String,
}

impl fmt::Display for ExceptionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.message)
    }
}

impl std::error::Error for ExceptionType {}

pub const BASE_EXCEPTION: &str = "BaseException";
pub const EXCEPTION: &str = "Exception";
pub const GENERATOR_EXIT: &str = "GeneratorExit";
pub const KEYBOARD_INTERRUPT: &str = "KeyboardInterrupt";
pub const SYSTEM_EXIT: &str = "SystemExit";
pub const STOP_ITERATION: &str = "StopIteration";

pub const VALUE_ERROR: &str = "ValueError";
pub const TYPE_ERROR: &str = "TypeError";
pub const KEY_ERROR: &str = "KeyError";
pub const INDEX_ERROR: &str = "IndexError";
pub const ATTRIBUTE_ERROR: &str = "AttributeError";
pub const NAME_ERROR: &str = "NameError";
pub const RUNTIME_ERROR: &str = "RuntimeError";
pub const NOT_IMPLEMENTED_ERROR: &str = "NotImplementedError";
pub const ZERO_DIVISION_ERROR: &str = "ZeroDivisionError";
pub const ASSERTION_ERROR: &str = "AssertionError";

pub const SYSTEM_ERROR: &str = "SystemError";
pub const OS_ERROR: &str = "OSError";
pub const IO_ERROR: &str = "IOError";
pub const MEMORY_ERROR: &str = "MemoryError";
pub const RECURSION_ERROR: &str = "RecursionError";

pub const IMPORT_ERROR: &str = "ImportError";
pub const MODULE_NOT_FOUND_ERROR: &str = "ModuleNotFoundError";

pub const SYNTAX_ERROR: &str = "SyntaxError";
pub const INDENTATION_ERROR: &str = "IndentationError";
pub const TAB_ERROR: &str = "TabError";

pub const WARNING: &str = "Warning";
pub const USER_WARNING: &str = "UserWarning";
pub const DEPRECATION_WARNING: &str = "DeprecationWarning";
pub const SYNTAX_WARNING: &str = "SyntaxWarning";
pub const RUNTIME_WARNING: &str = "RuntimeWarning";
pub const FUTURE_WARNING: &str = "FutureWarning";
pub const PENDING_DEPRECATION_WARNING: &str = "PendingDeprecationWarning";
pub const IMPORT_WARNING: &str = "ImportWarning";
pub const UNICODE_WARNING: &str = "UnicodeWarning";
pub const BYTES_WARNING: &str = "BytesWarning";
pub const RESOURCE_WARNING: &str = "ResourceWarning";

pub fn get_exception_base(exception: &str) -> Option<&'static str> {
    match exception {
        BASE_EXCEPTION => None,
        EXCEPTION | GENERATOR_EXIT | KEYBOARD_INTERRUPT | SYSTEM_EXIT | STOP_ITERATION => {
            Some(BASE_EXCEPTION)
        }
        VALUE_ERROR
        | TYPE_ERROR
        | KEY_ERROR
        | INDEX_ERROR
        | ATTRIBUTE_ERROR
        | NAME_ERROR
        | RUNTIME_ERROR
        | NOT_IMPLEMENTED_ERROR
        | ZERO_DIVISION_ERROR
        | ASSERTION_ERROR => Some(EXCEPTION),
        SYSTEM_ERROR | OS_ERROR | IO_ERROR | MEMORY_ERROR | RECURSION_ERROR => Some(EXCEPTION),
        IMPORT_ERROR | MODULE_NOT_FOUND_ERROR => Some(EXCEPTION),
        SYNTAX_ERROR | INDENTATION_ERROR | TAB_ERROR => Some(EXCEPTION),
        WARNING
        | USER_WARNING
        | DEPRECATION_WARNING
        | SYNTAX_WARNING
        | RUNTIME_WARNING
        | FUTURE_WARNING
        | PENDING_DEPRECATION_WARNING
        | IMPORT_WARNING
        | UNICODE_WARNING
        | BYTES_WARNING
        | RESOURCE_WARNING => Some(WARNING),
        _ => None,
    }
}

pub fn is_exception(name: &str) -> bool {
    matches!(
        name,
        "BaseException"
            | "Exception"
            | "ValueError"
            | "TypeError"
            | "KeyError"
            | "IndexError"
            | "AttributeError"
            | "NameError"
            | "RuntimeError"
            | "ZeroDivisionError"
            | "NotImplementedError"
            | "ImportError"
            | "ModuleNotFoundError"
            | "OSError"
            | "IOError"
            | "SystemError"
            | "MemoryError"
            | "RecursionError"
            | "SyntaxError"
            | "IndentationError"
            | "TabError"
            | "AssertionError"
            | "Warning"
            | "UserWarning"
            | "DeprecationWarning"
            | "PendingDeprecationWarning"
            | "RuntimeWarning"
            | "SyntaxWarning"
            | "BytesWarning"
            | "ResourceWarning"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_exception_base_base_exception() {
        assert_eq!(get_exception_base("BaseException"), None);
    }

    #[test]
    fn test_get_exception_base_exception() {
        assert_eq!(get_exception_base("Exception"), Some("BaseException"));
    }

    #[test]
    fn test_get_exception_base_value_error() {
        assert_eq!(get_exception_base("ValueError"), Some("Exception"));
    }

    #[test]
    fn test_get_exception_base_type_error() {
        assert_eq!(get_exception_base("TypeError"), Some("Exception"));
    }

    #[test]
    fn test_get_exception_base_unknown() {
        assert_eq!(get_exception_base("UnknownError"), None);
    }

    #[test]
    fn test_is_exception_base() {
        assert!(is_exception("BaseException"));
        assert!(is_exception("Exception"));
    }

    #[test]
    fn test_is_exception_common() {
        assert!(is_exception("ValueError"));
        assert!(is_exception("TypeError"));
        assert!(is_exception("KeyError"));
        assert!(is_exception("IndexError"));
        assert!(is_exception("AttributeError"));
        assert!(is_exception("NameError"));
        assert!(is_exception("RuntimeError"));
        assert!(is_exception("ZeroDivisionError"));
    }

    #[test]
    fn test_is_exception_system() {
        assert!(is_exception("SystemError"));
        assert!(is_exception("OSError"));
        assert!(is_exception("IOError"));
        assert!(is_exception("MemoryError"));
        assert!(is_exception("RecursionError"));
    }

    #[test]
    fn test_is_exception_import() {
        assert!(is_exception("ImportError"));
        assert!(is_exception("ModuleNotFoundError"));
    }

    #[test]
    fn test_is_exception_syntax() {
        assert!(is_exception("SyntaxError"));
        assert!(is_exception("IndentationError"));
        assert!(is_exception("TabError"));
    }

    #[test]
    fn test_is_exception_warning() {
        assert!(is_exception("Warning"));
        assert!(is_exception("UserWarning"));
        assert!(is_exception("DeprecationWarning"));
        assert!(is_exception("RuntimeWarning"));
    }

    #[test]
    fn test_is_exception_false() {
        assert!(!is_exception("UnknownError"));
        assert!(!is_exception("CustomError"));
        assert!(!is_exception("MyException"));
    }

    #[test]
    fn test_exception_base_hierarchy() {
        assert_eq!(get_exception_base("ValueError"), Some("Exception"));
        assert_eq!(get_exception_base("Exception"), Some("BaseException"));
    }

    #[test]
    fn test_all_exception_names_exist() {
        assert!(is_exception("NotImplementedError"));
        assert!(is_exception("AssertionError"));
        assert!(is_exception("PendingDeprecationWarning"));
        assert!(is_exception("SyntaxWarning"));
        assert!(is_exception("BytesWarning"));
        assert!(is_exception("ResourceWarning"));
    }
}
