use super::errors::StdlibError;
use crate::value::Value;

pub fn is_equal(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    Ok(a == b)
}

pub fn is_less(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x < y),
        (Value::Int(x), Value::Float(y)) => Ok((*x as f64) < *y),
        (Value::Float(x), Value::Int(y)) => Ok(*x < (*y as f64)),
        (Value::Float(x), Value::Float(y)) => Ok(x < y),
        (Value::Str(x), Value::Str(y)) => Ok(x < y),
        _ => Err(StdlibError::TypeError {
            expected: "comparable types".to_string(),
            got: format!("{} and {}", a.type_name(), b.type_name()),
        }),
    }
}

pub fn is_less_equal(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x <= y),
        (Value::Int(x), Value::Float(y)) => Ok((*x as f64) <= *y),
        (Value::Float(x), Value::Int(y)) => Ok(*x <= (*y as f64)),
        (Value::Float(x), Value::Float(y)) => Ok(x <= y),
        (Value::Str(x), Value::Str(y)) => Ok(x <= y),
        _ => Err(StdlibError::TypeError {
            expected: "comparable types".to_string(),
            got: format!("{} and {}", a.type_name(), b.type_name()),
        }),
    }
}

pub fn is_greater(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x > y),
        (Value::Int(x), Value::Float(y)) => Ok((*x as f64) > *y),
        (Value::Float(x), Value::Int(y)) => Ok(*x > (*y as f64)),
        (Value::Float(x), Value::Float(y)) => Ok(x > y),
        (Value::Str(x), Value::Str(y)) => Ok(x > y),
        _ => Err(StdlibError::TypeError {
            expected: "comparable types".to_string(),
            got: format!("{} and {}", a.type_name(), b.type_name()),
        }),
    }
}

pub fn is_greater_equal(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(x >= y),
        (Value::Int(x), Value::Float(y)) => Ok((*x as f64) >= *y),
        (Value::Float(x), Value::Int(y)) => Ok(*x >= (*y as f64)),
        (Value::Float(x), Value::Float(y)) => Ok(x >= y),
        (Value::Str(x), Value::Str(y)) => Ok(x >= y),
        _ => Err(StdlibError::TypeError {
            expected: "comparable types".to_string(),
            got: format!("{} and {}", a.type_name(), b.type_name()),
        }),
    }
}

pub fn is_not_equal(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    Ok(a != b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_equal_same_type() {
        assert!(is_equal(&Value::Int(42), &Value::Int(42)).unwrap());
        assert!(!is_equal(&Value::Int(42), &Value::Int(43)).unwrap());

        assert!(is_equal(&Value::Float(2.14), &Value::Float(2.14)).unwrap());
        assert!(!is_equal(&Value::Float(2.14), &Value::Float(2.71)).unwrap());

        assert!(is_equal(&Value::Bool(true), &Value::Bool(true)).unwrap());
        assert!(!is_equal(&Value::Bool(true), &Value::Bool(false)).unwrap());

        assert!(
            is_equal(
                &Value::Str("hello".to_string()),
                &Value::Str("hello".to_string())
            )
            .unwrap()
        );
        assert!(
            !is_equal(
                &Value::Str("hello".to_string()),
                &Value::Str("world".to_string())
            )
            .unwrap()
        );
    }

    #[test]
    fn test_is_equal_different_types() {
        assert!(!is_equal(&Value::Int(1), &Value::Bool(true)).unwrap());
        assert!(!is_equal(&Value::Int(42), &Value::Str("42".to_string())).unwrap());
        assert!(!is_equal(&Value::Bool(true), &Value::Int(1)).unwrap());
    }

    #[test]
    fn test_is_less_integers() {
        assert!(is_less(&Value::Int(1), &Value::Int(2)).unwrap());
        assert!(!is_less(&Value::Int(2), &Value::Int(1)).unwrap());
        assert!(!is_less(&Value::Int(1), &Value::Int(1)).unwrap());
    }

    #[test]
    fn test_is_less_floats() {
        assert!(is_less(&Value::Float(1.5), &Value::Float(2.5)).unwrap());
        assert!(!is_less(&Value::Float(2.5), &Value::Float(1.5)).unwrap());
        assert!(!is_less(&Value::Float(1.5), &Value::Float(1.5)).unwrap());
    }

    #[test]
    fn test_is_less_mixed_numeric() {
        assert!(is_less(&Value::Int(1), &Value::Float(1.5)).unwrap());
        assert!(is_less(&Value::Float(1.5), &Value::Int(2)).unwrap());
        assert!(!is_less(&Value::Int(2), &Value::Float(1.5)).unwrap());
    }

    #[test]
    fn test_is_less_strings() {
        assert!(is_less(&Value::Str("a".to_string()), &Value::Str("b".to_string())).unwrap());
        assert!(!is_less(&Value::Str("b".to_string()), &Value::Str("a".to_string())).unwrap());
    }

    #[test]
    fn test_is_less_invalid_types() {
        assert!(is_less(&Value::Int(1), &Value::Bool(true)).is_err());
        assert!(is_less(&Value::Str("a".to_string()), &Value::Int(1)).is_err());
    }

    #[test]
    fn test_is_less_equal_integers() {
        assert!(is_less_equal(&Value::Int(1), &Value::Int(2)).unwrap());
        assert!(is_less_equal(&Value::Int(1), &Value::Int(1)).unwrap());
        assert!(!is_less_equal(&Value::Int(2), &Value::Int(1)).unwrap());
    }

    #[test]
    fn test_is_less_equal_floats() {
        assert!(is_less_equal(&Value::Float(1.5), &Value::Float(2.5)).unwrap());
        assert!(is_less_equal(&Value::Float(1.5), &Value::Float(1.5)).unwrap());
        assert!(!is_less_equal(&Value::Float(2.5), &Value::Float(1.5)).unwrap());
    }

    #[test]
    fn test_is_less_equal_strings() {
        assert!(is_less_equal(&Value::Str("a".to_string()), &Value::Str("b".to_string())).unwrap());
        assert!(is_less_equal(&Value::Str("a".to_string()), &Value::Str("a".to_string())).unwrap());
    }

    #[test]
    fn test_is_greater_integers() {
        assert!(is_greater(&Value::Int(2), &Value::Int(1)).unwrap());
        assert!(!is_greater(&Value::Int(1), &Value::Int(2)).unwrap());
        assert!(!is_greater(&Value::Int(1), &Value::Int(1)).unwrap());
    }

    #[test]
    fn test_is_greater_floats() {
        assert!(is_greater(&Value::Float(2.5), &Value::Float(1.5)).unwrap());
        assert!(!is_greater(&Value::Float(1.5), &Value::Float(2.5)).unwrap());
    }

    #[test]
    fn test_is_greater_mixed_numeric() {
        assert!(is_greater(&Value::Float(1.5), &Value::Int(1)).unwrap());
        assert!(is_greater(&Value::Int(2), &Value::Float(1.5)).unwrap());
        assert!(!is_greater(&Value::Int(1), &Value::Float(1.5)).unwrap());
    }

    #[test]
    fn test_is_greater_strings() {
        assert!(is_greater(&Value::Str("b".to_string()), &Value::Str("a".to_string())).unwrap());
        assert!(!is_greater(&Value::Str("a".to_string()), &Value::Str("b".to_string())).unwrap());
    }

    #[test]
    fn test_is_greater_invalid_types() {
        assert!(is_greater(&Value::Int(1), &Value::Bool(false)).is_err());
    }

    #[test]
    fn test_is_greater_equal_integers() {
        assert!(is_greater_equal(&Value::Int(2), &Value::Int(1)).unwrap());
        assert!(is_greater_equal(&Value::Int(1), &Value::Int(1)).unwrap());
        assert!(!is_greater_equal(&Value::Int(1), &Value::Int(2)).unwrap());
    }

    #[test]
    fn test_is_greater_equal_floats() {
        assert!(is_greater_equal(&Value::Float(2.5), &Value::Float(1.5)).unwrap());
        assert!(is_greater_equal(&Value::Float(1.5), &Value::Float(1.5)).unwrap());
    }

    #[test]
    fn test_is_greater_equal_strings() {
        assert!(
            is_greater_equal(&Value::Str("b".to_string()), &Value::Str("a".to_string())).unwrap()
        );
        assert!(
            is_greater_equal(&Value::Str("a".to_string()), &Value::Str("a".to_string())).unwrap()
        );
    }

    #[test]
    fn test_is_not_equal() {
        assert!(is_not_equal(&Value::Int(1), &Value::Int(2)).unwrap());
        assert!(!is_not_equal(&Value::Int(1), &Value::Int(1)).unwrap());

        assert!(
            is_not_equal(
                &Value::Str("hello".to_string()),
                &Value::Str("world".to_string())
            )
            .unwrap()
        );
        assert!(
            !is_not_equal(
                &Value::Str("hello".to_string()),
                &Value::Str("hello".to_string())
            )
            .unwrap()
        );
    }

    #[test]
    fn test_comparison_transitivity() {
        let one = Value::Int(1);
        let two = Value::Int(2);
        let three = Value::Int(3);

        assert!(is_less(&one, &two).unwrap());
        assert!(is_less(&two, &three).unwrap());
        assert!(is_less(&one, &three).unwrap());
    }

    #[test]
    fn test_comparison_symmetry() {
        let a = Value::Int(5);
        let b = Value::Int(3);

        assert_eq!(is_less(&a, &b).unwrap(), is_greater(&b, &a).unwrap());
    }
}
