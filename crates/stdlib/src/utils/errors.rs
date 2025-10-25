use std::fmt;

#[derive(Debug, Clone)]
pub enum StdlibError {
    TypeError { expected: String, got: String },
    ValueError(String),
    IndexError(String),
    KeyError(String),
    AttributeError(String),
    NameError(String),
    RuntimeError(String),
    ZeroDivisionError,
    NotImplementedError(String),
    ImportError(String),
    OSError(String),
    IOError(String),
    AssertionError(String),
    SystemError(String),
}

impl fmt::Display for StdlibError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StdlibError::TypeError { expected, got } => {
                write!(f, "TypeError: expected {}, got {}", expected, got)
            }
            StdlibError::ValueError(msg) => write!(f, "ValueError: {}", msg),
            StdlibError::IndexError(msg) => write!(f, "IndexError: {}", msg),
            StdlibError::KeyError(msg) => write!(f, "KeyError: {}", msg),
            StdlibError::AttributeError(msg) => write!(f, "AttributeError: {}", msg),
            StdlibError::NameError(msg) => write!(f, "NameError: {}", msg),
            StdlibError::RuntimeError(msg) => write!(f, "RuntimeError: {}", msg),
            StdlibError::ZeroDivisionError => write!(f, "ZeroDivisionError: division by zero"),
            StdlibError::NotImplementedError(msg) => write!(f, "NotImplementedError: {}", msg),
            StdlibError::ImportError(msg) => write!(f, "ImportError: {}", msg),
            StdlibError::OSError(msg) => write!(f, "OSError: {}", msg),
            StdlibError::IOError(msg) => write!(f, "IOError: {}", msg),
            StdlibError::AssertionError(msg) => write!(f, "AssertionError: {}", msg),
            StdlibError::SystemError(msg) => write!(f, "SystemError: {}", msg),
        }
    }
}

impl std::error::Error for StdlibError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_error_display() {
        let err = StdlibError::TypeError {
            expected: "str".to_string(),
            got: "int".to_string(),
        };
        let msg = err.to_string();
        assert!(msg.contains("TypeError"));
        assert!(msg.contains("str"));
        assert!(msg.contains("int"));
    }

    #[test]
    fn test_value_error_display() {
        let err = StdlibError::ValueError("invalid value".to_string());
        let msg = err.to_string();
        assert!(msg.contains("ValueError"));
        assert!(msg.contains("invalid value"));
    }

    #[test]
    fn test_index_error_display() {
        let err = StdlibError::IndexError("index out of range".to_string());
        let msg = err.to_string();
        assert!(msg.contains("IndexError"));
    }

    #[test]
    fn test_key_error_display() {
        let err = StdlibError::KeyError("key not found".to_string());
        let msg = err.to_string();
        assert!(msg.contains("KeyError"));
    }

    #[test]
    fn test_attribute_error_display() {
        let err = StdlibError::AttributeError("no attribute".to_string());
        let msg = err.to_string();
        assert!(msg.contains("AttributeError"));
    }

    #[test]
    fn test_name_error_display() {
        let err = StdlibError::NameError("name not defined".to_string());
        let msg = err.to_string();
        assert!(msg.contains("NameError"));
    }

    #[test]
    fn test_runtime_error_display() {
        let err = StdlibError::RuntimeError("runtime failed".to_string());
        let msg = err.to_string();
        assert!(msg.contains("RuntimeError"));
    }

    #[test]
    fn test_zero_division_error_display() {
        let err = StdlibError::ZeroDivisionError;
        let msg = err.to_string();
        assert!(msg.contains("ZeroDivisionError"));
    }

    #[test]
    fn test_not_implemented_error_display() {
        let err = StdlibError::NotImplementedError("not implemented".to_string());
        let msg = err.to_string();
        assert!(msg.contains("NotImplementedError"));
    }

    #[test]
    fn test_import_error_display() {
        let err = StdlibError::ImportError("import failed".to_string());
        let msg = err.to_string();
        assert!(msg.contains("ImportError"));
    }

    #[test]
    fn test_os_error_display() {
        let err = StdlibError::OSError("os error".to_string());
        let msg = err.to_string();
        assert!(msg.contains("OSError"));
    }

    #[test]
    fn test_io_error_display() {
        let err = StdlibError::IOError("io error".to_string());
        let msg = err.to_string();
        assert!(msg.contains("IOError"));
    }

    #[test]
    fn test_all_error_types_are_cloneable() {
        let err1 = StdlibError::ValueError("test".to_string());
        let err2 = err1.clone();
        assert_eq!(err1.to_string(), err2.to_string());
    }

    #[test]
    fn test_all_error_types_are_debuggable() {
        let err = StdlibError::TypeError {
            expected: "int".to_string(),
            got: "str".to_string(),
        };
        let debug_str = format!("{:?}", err);
        assert!(debug_str.contains("TypeError"));
    }
}
