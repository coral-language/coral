use super::errors::StdlibError;
use crate::value::Value;

pub fn to_int(value: &Value) -> Result<i64, StdlibError> {
    match value {
        Value::Int(i) => Ok(*i),
        Value::Bool(b) => Ok(if *b { 1 } else { 0 }),
        Value::Float(f) => Ok(*f as i64),
        Value::Str(s) => s
            .trim()
            .parse::<i64>()
            .map_err(|_| StdlibError::ValueError(format!("invalid literal for int(): '{}'", s))),
        _ => Err(StdlibError::TypeError {
            expected: "int".to_string(),
            got: value.type_name().to_string(),
        }),
    }
}

pub fn to_float(value: &Value) -> Result<f64, StdlibError> {
    match value {
        Value::Int(i) => Ok(*i as f64),
        Value::Float(f) => Ok(*f),
        Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
        Value::Str(s) => s.trim().parse::<f64>().map_err(|_| {
            StdlibError::ValueError(format!("could not convert string to float: '{}'", s))
        }),
        _ => Err(StdlibError::TypeError {
            expected: "float".to_string(),
            got: value.type_name().to_string(),
        }),
    }
}

pub fn to_string(value: &Value) -> String {
    format!("{}", value)
}

pub fn to_bool(value: &Value) -> bool {
    value.is_truthy()
}

pub fn to_bytes(value: &Value) -> Result<Vec<u8>, StdlibError> {
    match value {
        Value::Bytes(b) => Ok(b.clone()),
        Value::Str(s) => Ok(s.as_bytes().to_vec()),
        _ => Err(StdlibError::TypeError {
            expected: "bytes or str".to_string(),
            got: value.type_name().to_string(),
        }),
    }
}

pub fn from_int(i: i64) -> Value {
    Value::Int(i)
}

pub fn from_float(f: f64) -> Value {
    Value::Float(f)
}

pub fn from_string(s: String) -> Value {
    Value::Str(s)
}

pub fn from_bool(b: bool) -> Value {
    Value::Bool(b)
}

pub fn from_bytes(b: Vec<u8>) -> Value {
    Value::Bytes(b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_int_from_int() {
        assert_eq!(to_int(&Value::Int(42)).unwrap(), 42);
        assert_eq!(to_int(&Value::Int(-1)).unwrap(), -1);
        assert_eq!(to_int(&Value::Int(0)).unwrap(), 0);
    }

    #[test]
    fn test_to_int_from_bool() {
        assert_eq!(to_int(&Value::Bool(true)).unwrap(), 1);
        assert_eq!(to_int(&Value::Bool(false)).unwrap(), 0);
    }

    #[test]
    fn test_to_int_from_float() {
        assert_eq!(to_int(&Value::Float(2.14)).unwrap(), 2);
        assert_eq!(to_int(&Value::Float(3.99)).unwrap(), 3);
        assert_eq!(to_int(&Value::Float(-2.5)).unwrap(), -2);
    }

    #[test]
    fn test_to_int_from_string() {
        assert_eq!(to_int(&Value::Str("42".to_string())).unwrap(), 42);
        assert_eq!(to_int(&Value::Str("-10".to_string())).unwrap(), -10);
        assert_eq!(to_int(&Value::Str(" 100 ".to_string())).unwrap(), 100);
    }

    #[test]
    fn test_to_int_from_invalid_string() {
        assert!(to_int(&Value::Str("not a number".to_string())).is_err());
        assert!(to_int(&Value::Str("12.5".to_string())).is_err());
    }

    #[test]
    fn test_to_int_from_invalid_type() {
        assert!(to_int(&Value::None).is_err());
        assert!(
            to_int(&Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                vec![]
            ))))
            .is_err()
        );
    }

    #[test]
    fn test_to_float_from_int() {
        assert_eq!(to_float(&Value::Int(42)).unwrap(), 42.0);
        assert_eq!(to_float(&Value::Int(-5)).unwrap(), -5.0);
    }

    #[test]
    fn test_to_float_from_float() {
        assert_eq!(to_float(&Value::Float(2.14)).unwrap(), 2.14);
        assert_eq!(to_float(&Value::Float(-2.5)).unwrap(), -2.5);
    }

    #[test]
    fn test_to_float_from_bool() {
        assert_eq!(to_float(&Value::Bool(true)).unwrap(), 1.0);
        assert_eq!(to_float(&Value::Bool(false)).unwrap(), 0.0);
    }

    #[test]
    fn test_to_float_from_string() {
        assert_eq!(to_float(&Value::Str("2.14".to_string())).unwrap(), 2.14);
        assert_eq!(to_float(&Value::Str("42".to_string())).unwrap(), 42.0);
        assert_eq!(to_float(&Value::Str(" -2.5 ".to_string())).unwrap(), -2.5);
    }

    #[test]
    fn test_to_float_from_invalid_string() {
        assert!(to_float(&Value::Str("not a number".to_string())).is_err());
    }

    #[test]
    fn test_to_string() {
        assert_eq!(to_string(&Value::Int(42)), "42");
        assert_eq!(to_string(&Value::Bool(true)), "True");
        assert_eq!(to_string(&Value::Bool(false)), "False");
        assert_eq!(to_string(&Value::Str("hello".to_string())), "hello");
        assert_eq!(to_string(&Value::None), "None");
    }

    #[test]
    fn test_to_bool_truthy_values() {
        assert!(to_bool(&Value::Int(1)));
        assert!(to_bool(&Value::Int(-1)));
        assert!(to_bool(&Value::Float(1.0)));
        assert!(to_bool(&Value::Str("hello".to_string())));
        assert!(to_bool(&Value::Bool(true)));
    }

    #[test]
    fn test_to_bool_falsy_values() {
        assert!(!to_bool(&Value::Int(0)));
        assert!(!to_bool(&Value::Float(0.0)));
        assert!(!to_bool(&Value::Str("".to_string())));
        assert!(!to_bool(&Value::Bool(false)));
        assert!(!to_bool(&Value::None));
    }

    #[test]
    fn test_to_bytes_from_bytes() {
        let bytes = vec![1, 2, 3];
        let result = to_bytes(&Value::Bytes(bytes.clone())).unwrap();
        assert_eq!(result, bytes);
    }

    #[test]
    fn test_to_bytes_from_string() {
        let result = to_bytes(&Value::Str("hello".to_string())).unwrap();
        assert_eq!(result, b"hello".to_vec());
    }

    #[test]
    fn test_to_bytes_from_invalid_type() {
        assert!(to_bytes(&Value::Int(42)).is_err());
        assert!(to_bytes(&Value::None).is_err());
    }

    #[test]
    fn test_from_int() {
        assert_eq!(from_int(42), Value::Int(42));
        assert_eq!(from_int(-5), Value::Int(-5));
    }

    #[test]
    fn test_from_float() {
        assert_eq!(from_float(2.14), Value::Float(2.14));
    }

    #[test]
    fn test_from_string() {
        assert_eq!(
            from_string("hello".to_string()),
            Value::Str("hello".to_string())
        );
    }

    #[test]
    fn test_from_bool() {
        assert_eq!(from_bool(true), Value::Bool(true));
        assert_eq!(from_bool(false), Value::Bool(false));
    }

    #[test]
    fn test_from_bytes() {
        let bytes = vec![1, 2, 3];
        assert_eq!(from_bytes(bytes.clone()), Value::Bytes(bytes));
    }

    #[test]
    fn test_roundtrip_conversions() {
        let original = 42i64;
        let value = from_int(original);
        let converted = to_int(&value).unwrap();
        assert_eq!(original, converted);

        let original = 2.14f64;
        let value = from_float(original);
        let converted = to_float(&value).unwrap();
        assert_eq!(original, converted);
    }
}
