use crate::utils::StdlibError;
use crate::value::Value;

pub fn abs(x: Value) -> Result<Value, StdlibError> {
    match x {
        Value::Int(i) => Ok(Value::Int(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => Err(StdlibError::TypeError {
            expected: "int or float".to_string(),
            got: x.type_name().to_string(),
        }),
    }
}

pub fn round(x: Value, ndigits: Option<Value>) -> Result<Value, StdlibError> {
    match x {
        Value::Float(f) => match ndigits {
            None => Ok(Value::Int(f.round() as i64)),
            Some(Value::Int(n)) => {
                let multiplier = 10_f64.powi(n as i32);
                Ok(Value::Float((f * multiplier).round() / multiplier))
            }
            _ => Err(StdlibError::TypeError {
                expected: "int".to_string(),
                got: "invalid ndigits".to_string(),
            }),
        },
        Value::Int(i) => Ok(Value::Int(i)),
        _ => Err(StdlibError::TypeError {
            expected: "float or int".to_string(),
            got: x.type_name().to_string(),
        }),
    }
}

pub fn pow(x: Value, y: Value, z: Option<Value>) -> Result<Value, StdlibError> {
    let x_f = match x {
        Value::Int(i) => i as f64,
        Value::Float(f) => f,
        _ => {
            return Err(StdlibError::TypeError {
                expected: "int or float".to_string(),
                got: x.type_name().to_string(),
            });
        }
    };

    let y_f = match y {
        Value::Int(i) => i as f64,
        Value::Float(f) => f,
        _ => {
            return Err(StdlibError::TypeError {
                expected: "int or float".to_string(),
                got: y.type_name().to_string(),
            });
        }
    };

    let result = x_f.powf(y_f);

    match z {
        None => {
            if result == result.floor() && result.is_finite() {
                Ok(Value::Int(result as i64))
            } else {
                Ok(Value::Float(result))
            }
        }
        Some(Value::Int(m)) => Ok(Value::Int((result as i64) % m)),
        _ => Err(StdlibError::TypeError {
            expected: "int".to_string(),
            got: "invalid modulo".to_string(),
        }),
    }
}

pub fn divmod(x: Value, y: Value) -> Result<Value, StdlibError> {
    match (x, y) {
        (Value::Int(a), Value::Int(b)) => {
            if b == 0 {
                return Err(StdlibError::ZeroDivisionError);
            }
            let quotient = a / b;
            let remainder = a % b;
            Ok(Value::Tuple(vec![
                Value::Int(quotient),
                Value::Int(remainder),
            ]))
        }
        _ => Err(StdlibError::TypeError {
            expected: "int".to_string(),
            got: "invalid types".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abs_positive_int() {
        let result = abs(Value::Int(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_abs_negative_int() {
        let result = abs(Value::Int(-42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_abs_positive_float() {
        let result = abs(Value::Float(2.14));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - 2.14).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_abs_negative_float() {
        let result = abs(Value::Float(-2.14));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - 2.14).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_round_no_ndigits() {
        let result = round(Value::Float(3.7), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(4));
    }

    #[test]
    fn test_round_with_ndigits() {
        let result = round(Value::Float(2.14159), Some(Value::Int(2)));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - 2.14).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_pow_integer_exponent() {
        let result = pow(Value::Int(2), Value::Int(3), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(8));
    }

    #[test]
    fn test_pow_float_exponent() {
        let result = pow(Value::Float(2.0), Value::Float(0.5), None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - std::f64::consts::SQRT_2).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_pow_with_modulo() {
        let result = pow(Value::Int(2), Value::Int(3), Some(Value::Int(5)));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_divmod_positive() {
        let result = divmod(Value::Int(17), Value::Int(5));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Tuple(t) => {
                assert_eq!(t[0], Value::Int(3));
                assert_eq!(t[1], Value::Int(2));
            }
            _ => panic!("Expected tuple"),
        }
    }

    #[test]
    fn test_divmod_negative() {
        let result = divmod(Value::Int(-17), Value::Int(5));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Tuple(t) => {
                assert_eq!(t[0], Value::Int(-3));
                assert_eq!(t[1], Value::Int(-2));
            }
            _ => panic!("Expected tuple"),
        }
    }

    #[test]
    fn test_divmod_zero_divisor() {
        let result = divmod(Value::Int(10), Value::Int(0));
        assert!(result.is_err());
    }
}
