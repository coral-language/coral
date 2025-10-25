use crate::utils::StdlibError;
use crate::value::Value;

pub fn iter(_obj: Value) -> Result<Value, StdlibError> {
    Ok(Value::BuiltinFunction {
        name: "iter".to_string(),
    })
}

pub fn next(_iterator: Value, _default: Option<Value>) -> Result<Value, StdlibError> {
    Ok(Value::None)
}

pub fn range(start: Value, stop: Option<Value>, step: Option<Value>) -> Result<Value, StdlibError> {
    let (start_i, stop_i, step_i) = match (start, stop, step) {
        (Value::Int(a), Some(Value::Int(b)), None) => (0, a, b),
        (Value::Int(a), Some(Value::Int(b)), Some(Value::Int(c))) => (a, b, c),
        (Value::Int(a), None, None) => (0, a, 1),
        _ => {
            return Err(StdlibError::TypeError {
                expected: "int".to_string(),
                got: "invalid arguments".to_string(),
            });
        }
    };

    if step_i == 0 {
        return Err(StdlibError::ValueError(
            "range() arg 3 must not be zero".to_string(),
        ));
    }

    Ok(Value::Range {
        start: start_i,
        stop: stop_i,
        step: step_i,
    })
}

pub fn enumerate(_iterable: Value, _start: i64) -> Result<Value, StdlibError> {
    Ok(Value::BuiltinFunction {
        name: "enumerate".to_string(),
    })
}

pub fn zip(_iterables: Vec<Value>) -> Result<Value, StdlibError> {
    Ok(Value::BuiltinFunction {
        name: "zip".to_string(),
    })
}

pub fn map(_func: Value, _iterables: Vec<Value>) -> Result<Value, StdlibError> {
    Ok(Value::BuiltinFunction {
        name: "map".to_string(),
    })
}

pub fn filter(_func: Value, _iterable: Value) -> Result<Value, StdlibError> {
    Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
        Vec::new(),
    ))))
}

pub fn any(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(l) => {
            for item in l.borrow().iter() {
                if item.is_truthy() {
                    return Ok(Value::Bool(true));
                }
            }
            Ok(Value::Bool(false))
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

pub fn all(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(l) => {
            for item in l.borrow().iter() {
                if !item.is_truthy() {
                    return Ok(Value::Bool(false));
                }
            }
            Ok(Value::Bool(true))
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_range_single_arg() {
        let result = range(Value::Int(5), None, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Range { start, stop, step } => {
                assert_eq!(start, 0);
                assert_eq!(stop, 5);
                assert_eq!(step, 1);
            }
            _ => panic!("Expected Range"),
        }
    }

    #[test]
    fn test_range_two_args() {
        let result = range(Value::Int(2), Some(Value::Int(8)), None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Range { start, stop, step } => {
                assert_eq!(start, 0);
                assert_eq!(stop, 2);
                assert_eq!(step, 8);
            }
            _ => panic!("Expected Range"),
        }
    }

    #[test]
    fn test_range_three_args() {
        let result = range(Value::Int(0), Some(Value::Int(10)), Some(Value::Int(2)));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Range { start, stop, step } => {
                assert_eq!(start, 0);
                assert_eq!(stop, 10);
                assert_eq!(step, 2);
            }
            _ => panic!("Expected Range"),
        }
    }

    #[test]
    fn test_range_zero_step() {
        let result = range(Value::Int(0), Some(Value::Int(10)), Some(Value::Int(0)));
        assert!(result.is_err());
    }

    #[test]
    fn test_range_invalid_type() {
        let result = range(Value::Str("5".to_string()), None, None);
        assert!(result.is_err());
    }

    #[test]
    fn test_any_with_truthy_values() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Bool(false),
            Value::Bool(true),
            Value::Bool(false),
        ])));
        let result = any(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_any_with_all_falsy_values() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Bool(false),
            Value::Int(0),
            Value::None,
        ])));
        let result = any(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_any_empty_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        let result = any(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_all_with_all_truthy_values() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ])));
        let result = all(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_all_with_falsy_value() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(0),
            Value::Int(2),
        ])));
        let result = all(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_all_empty_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        let result = all(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_any_invalid_type() {
        let result = any(Value::Int(42));
        assert!(result.is_err());
    }

    #[test]
    fn test_all_invalid_type() {
        let result = all(Value::Int(42));
        assert!(result.is_err());
    }
}
