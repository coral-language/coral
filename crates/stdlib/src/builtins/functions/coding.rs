use crate::utils::{StdlibError, conversions};
use crate::value::Value;

pub fn hex(x: Value) -> Result<Value, StdlibError> {
    let i = conversions::to_int(&x)?;
    Ok(Value::Str(format!("0x{:x}", i)))
}

pub fn oct(x: Value) -> Result<Value, StdlibError> {
    let i = conversions::to_int(&x)?;
    Ok(Value::Str(format!("0o{:o}", i)))
}

pub fn bin(x: Value) -> Result<Value, StdlibError> {
    let i = conversions::to_int(&x)?;
    Ok(Value::Str(format!("0b{:b}", i)))
}

pub fn chr(code: i64) -> Result<Value, StdlibError> {
    match char::from_u32(code as u32) {
        Some(c) => Ok(Value::Str(c.to_string())),
        None => Err(StdlibError::ValueError(
            "chr() arg not in range(0x110000)".to_string(),
        )),
    }
}

pub fn ord(s: Value) -> Result<Value, StdlibError> {
    match s {
        Value::Str(string) => {
            if string.len() == 1 {
                Ok(Value::Int(string.chars().next().unwrap() as i64))
            } else {
                Err(StdlibError::TypeError {
                    expected: "str of length 1".to_string(),
                    got: format!("length {}", string.len()),
                })
            }
        }
        _ => Err(StdlibError::TypeError {
            expected: "str".to_string(),
            got: s.type_name().to_string(),
        }),
    }
}

pub fn ascii(obj: Value) -> Result<Value, StdlibError> {
    Ok(Value::Str(format!("{:?}", obj)))
}

pub fn repr(obj: Value) -> Result<Value, StdlibError> {
    Ok(Value::Str(format!("{:?}", obj)))
}

pub fn format(value: Value, format_spec: Option<Value>) -> Result<Value, StdlibError> {
    let spec = match format_spec {
        Some(Value::Str(s)) => s,
        None => String::new(),
        _ => {
            return Err(StdlibError::TypeError {
                expected: "str".to_string(),
                got: "invalid format spec".to_string(),
            });
        }
    };

    match value {
        Value::Int(i) => {
            if spec.contains('x') || spec.contains('X') {
                Ok(Value::Str(format!("{:x}", i)))
            } else if spec.contains('b') {
                Ok(Value::Str(format!("{:b}", i)))
            } else if spec.contains('o') {
                Ok(Value::Str(format!("{:o}", i)))
            } else {
                Ok(Value::Str(format!("{}", i)))
            }
        }
        Value::Float(f) => {
            if spec.contains('e') || spec.contains('E') {
                Ok(Value::Str(format!("{:e}", f)))
            } else if spec.contains('.') {
                let digits: String = spec.chars().filter(|c| c.is_ascii_digit()).collect();
                if let Ok(precision) = digits.parse::<usize>() {
                    Ok(Value::Str(format!("{:.prec$}", f, prec = precision)))
                } else {
                    Ok(Value::Str(format!("{}", f)))
                }
            } else {
                Ok(Value::Str(format!("{}", f)))
            }
        }
        _ => Ok(Value::Str(format!("{}", value))),
    }
}

pub fn hash(obj: Value) -> Result<Value, StdlibError> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    match obj {
        Value::None => "None".hash(&mut hasher),
        Value::Bool(b) => b.hash(&mut hasher),
        Value::Int(i) => i.hash(&mut hasher),
        Value::Float(f) => f.to_bits().hash(&mut hasher),
        Value::Str(s) => s.hash(&mut hasher),
        _ => {
            return Err(StdlibError::TypeError {
                expected: "hashable".to_string(),
                got: format!("unhashable type: '{}'", obj.type_name()),
            });
        }
    }
    Ok(Value::Int(hasher.finish() as i64))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_positive() {
        let result = hex(Value::Int(255));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0xff".to_string()));
    }

    #[test]
    fn test_hex_zero() {
        let result = hex(Value::Int(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0x0".to_string()));
    }

    #[test]
    fn test_hex_negative() {
        let result = hex(Value::Int(-16));
        assert!(result.is_ok());
        // Rust's hex representation of negative numbers uses two's complement
        assert_eq!(
            result.unwrap(),
            Value::Str("0xfffffffffffffff0".to_string())
        );
    }

    #[test]
    fn test_oct_positive() {
        let result = oct(Value::Int(64));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0o100".to_string()));
    }

    #[test]
    fn test_oct_zero() {
        let result = oct(Value::Int(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0o0".to_string()));
    }

    #[test]
    fn test_bin_positive() {
        let result = bin(Value::Int(5));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0b101".to_string()));
    }

    #[test]
    fn test_bin_zero() {
        let result = bin(Value::Int(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("0b0".to_string()));
    }

    #[test]
    fn test_chr_valid() {
        let result = chr(65);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("A".to_string()));
    }

    #[test]
    fn test_chr_zero() {
        let result = chr(0);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("\0".to_string()));
    }

    #[test]
    fn test_chr_out_of_range() {
        let result = chr(0x110000);
        assert!(result.is_err());
    }

    #[test]
    fn test_ord_single_char() {
        let result = ord(Value::Str("A".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(65));
    }

    #[test]
    fn test_ord_null_char() {
        let result = ord(Value::Str("\0".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn test_ord_multiple_chars() {
        let result = ord(Value::Str("hello".to_string()));
        assert!(result.is_err());
    }

    #[test]
    fn test_ord_empty_string() {
        let result = ord(Value::Str("".to_string()));
        assert!(result.is_err());
    }

    #[test]
    fn test_ascii_string() {
        let result = ascii(Value::Str("hello".to_string()));
        assert!(result.is_ok());
    }

    #[test]
    fn test_ascii_int() {
        let result = ascii(Value::Int(42));
        assert!(result.is_ok());
    }

    #[test]
    fn test_repr_string() {
        let result = repr(Value::Str("hello".to_string()));
        assert!(result.is_ok());
    }

    #[test]
    fn test_repr_int() {
        let result = repr(Value::Int(42));
        assert!(result.is_ok());
    }

    #[test]
    fn test_format_string() {
        let result = format(Value::Str("hello".to_string()), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("hello".to_string()));
    }

    #[test]
    fn test_format_int() {
        let result = format(Value::Int(42), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("42".to_string()));
    }

    #[test]
    fn test_hash_int() {
        let result = hash(Value::Int(42));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int hash"),
        }
    }

    #[test]
    fn test_hash_string() {
        let result = hash(Value::Str("hello".to_string()));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int hash"),
        }
    }

    #[test]
    fn test_hash_consistency() {
        let h1 = hash(Value::Str("test".to_string()));
        let h2 = hash(Value::Str("test".to_string()));
        assert!(h1.is_ok());
        assert!(h2.is_ok());
        assert_eq!(h1.unwrap(), h2.unwrap());
    }
}
