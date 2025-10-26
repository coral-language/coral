use crate::utils::StdlibError;
use crate::value::Value;
use std::io::{self, Write};

pub fn print(
    args: Vec<Value>,
    sep: Option<String>,
    end: Option<String>,
) -> Result<Value, StdlibError> {
    let separator = sep.unwrap_or_else(|| " ".to_string());
    let ending = end.unwrap_or_else(|| "\n".to_string());

    let output = args
        .iter()
        .map(|v| format!("{}", v))
        .collect::<Vec<_>>()
        .join(&separator);

    print!("{}{}", output, ending);
    io::stdout()
        .flush()
        .map_err(|e| StdlibError::IOError(e.to_string()))?;

    Ok(Value::None)
}

pub fn input(prompt: Option<Value>) -> Result<Value, StdlibError> {
    if let Some(p) = prompt {
        print!("{}", p);
        io::stdout()
            .flush()
            .map_err(|e| StdlibError::IOError(e.to_string()))?;
    }

    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|e| StdlibError::IOError(e.to_string()))?;

    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }

    Ok(Value::Str(line))
}

pub fn open(
    file: Value,
    mode: Option<Value>,
    _buffering: Option<Value>,
) -> Result<Value, StdlibError> {
    let filename = match file {
        Value::Str(s) => s,
        _ => {
            return Err(StdlibError::TypeError {
                expected: "str".to_string(),
                got: file.type_name().to_string(),
            });
        }
    };

    let _file_mode = match mode {
        Some(Value::Str(m)) => m,
        None => "r".to_string(),
        _ => {
            return Err(StdlibError::TypeError {
                expected: "str".to_string(),
                got: "invalid mode type".to_string(),
            });
        }
    };

    Ok(Value::Module {
        name: format!("file::{}", filename),
        exports: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_no_args() {
        let result = print(vec![], None, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_print_single_arg() {
        let result = print(vec![Value::Str("hello".to_string())], None, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_print_multiple_args() {
        let result = print(
            vec![
                Value::Int(1),
                Value::Str("hello".to_string()),
                Value::Bool(true),
            ],
            None,
            None,
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_print_with_separator() {
        let result = print(
            vec![Value::Int(1), Value::Int(2), Value::Int(3)],
            Some(", ".to_string()),
            None,
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_print_with_ending() {
        let result = print(
            vec![Value::Str("test".to_string())],
            None,
            Some("".to_string()),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_open_creates_module() {
        let result = open(Value::Str("test.txt".to_string()), None, None);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::Module { name, .. } => {
                assert!(name.contains("test.txt"));
            }
            _ => panic!("Expected Module variant"),
        }
    }

    #[test]
    fn test_open_with_invalid_filename_type() {
        let result = open(Value::Int(123), None, None);
        assert!(result.is_err());

        match result.unwrap_err() {
            StdlibError::TypeError { expected, got } => {
                assert_eq!(expected, "str");
                assert_eq!(got, "int");
            }
            _ => panic!("Expected TypeError"),
        }
    }

    #[test]
    fn test_open_with_mode() {
        let result = open(
            Value::Str("test.txt".to_string()),
            Some(Value::Str("w".to_string())),
            None,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_open_with_invalid_mode_type() {
        let result = open(
            Value::Str("test.txt".to_string()),
            Some(Value::Int(123)),
            None,
        );
        assert!(result.is_err());

        match result.unwrap_err() {
            StdlibError::TypeError { .. } => {}
            _ => panic!("Expected TypeError"),
        }
    }
}
