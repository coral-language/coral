use crate::utils::{StdlibError, conversions};
use crate::value::Value;
use std::collections::HashMap;

pub fn int_fn(x: Value, base: Option<Value>) -> Result<Value, StdlibError> {
    match base {
        None => {
            let i = conversions::to_int(&x)?;
            Ok(Value::Int(i))
        }
        Some(Value::Int(b)) => match x {
            Value::Str(s) => i64::from_str_radix(s.trim(), b as u32)
                .map(Value::Int)
                .map_err(|_| {
                    StdlibError::ValueError(format!(
                        "invalid literal for int() with base {}: '{}'",
                        b, s
                    ))
                }),
            _ => Err(StdlibError::TypeError {
                expected: "str".to_string(),
                got: x.type_name().to_string(),
            }),
        },
        _ => Err(StdlibError::TypeError {
            expected: "int".to_string(),
            got: "invalid base".to_string(),
        }),
    }
}

pub fn float_fn(x: Value) -> Result<Value, StdlibError> {
    conversions::to_float(&x).map(Value::Float)
}

pub fn str_fn(x: Value) -> Result<Value, StdlibError> {
    Ok(Value::Str(conversions::to_string(&x)))
}

pub fn bool_fn(x: Value) -> Result<Value, StdlibError> {
    Ok(Value::Bool(x.is_truthy()))
}

pub fn bytes_fn(x: Value) -> Result<Value, StdlibError> {
    match x {
        Value::Str(s) => Ok(Value::Bytes(s.into_bytes())),
        Value::Bytes(b) => Ok(Value::Bytes(b)),
        _ => Err(StdlibError::TypeError {
            expected: "str or bytes".to_string(),
            got: x.type_name().to_string(),
        }),
    }
}

pub fn list(_iterable: Value) -> Result<Value, StdlibError> {
    match _iterable {
        Value::List(l) => Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
            l.borrow().clone(),
        )))),
        Value::Tuple(t) => Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(t)))),
        Value::Str(s) => {
            let items: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                items,
            ))))
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: _iterable.type_name().to_string(),
        }),
    }
}

pub fn tuple_fn(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::Tuple(t) => Ok(Value::Tuple(t)),
        Value::List(l) => Ok(Value::Tuple(l.borrow().clone())),
        Value::Str(s) => {
            let items: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
            Ok(Value::Tuple(items))
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

pub fn dict_fn(mapping: Value) -> Result<Value, StdlibError> {
    match mapping {
        Value::Dict(d) => {
            let new_dict = d.borrow().clone();
            Ok(Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(
                new_dict,
            ))))
        }
        _ => Ok(Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(
            HashMap::new(),
        )))),
    }
}

pub fn set_fn(iterable: Value) -> Result<Value, StdlibError> {
    let mut set: std::collections::HashSet<String> = std::collections::HashSet::new();

    match iterable {
        Value::List(l) => {
            for item in l.borrow().iter() {
                if let Value::Str(s) = item {
                    set.insert(s.clone());
                }
            }
        }
        Value::Tuple(t) => {
            for item in t.iter() {
                if let Value::Str(s) = item {
                    set.insert(s.clone());
                }
            }
        }
        Value::Str(s) => {
            for c in s.chars() {
                set.insert(c.to_string());
            }
        }
        Value::Set(s) => {
            return Ok(Value::Set(std::rc::Rc::new(std::cell::RefCell::new(
                s.borrow().clone(),
            ))));
        }
        _ => {
            return Err(StdlibError::TypeError {
                expected: "iterable".to_string(),
                got: iterable.type_name().to_string(),
            });
        }
    }

    Ok(Value::Set(std::rc::Rc::new(std::cell::RefCell::new(set))))
}

pub fn frozenset_fn(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(l) => {
            let mut set = std::collections::HashSet::new();
            for item in l.borrow().iter() {
                if let Value::Str(s) = item {
                    set.insert(s.clone());
                }
            }
            Ok(Value::FrozenSet(set))
        }
        _ => Ok(Value::FrozenSet(std::collections::HashSet::new())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_from_string() {
        let result = int_fn(Value::Str("42".to_string()), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_int_from_float() {
        let result = int_fn(Value::Float(2.14), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(2));
    }

    #[test]
    fn test_int_from_bool() {
        let result = int_fn(Value::Bool(true), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(1));
    }

    #[test]
    fn test_float_from_string() {
        let result = float_fn(Value::Str("2.14".to_string()));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - 2.14).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_float_from_int() {
        let result = float_fn(Value::Int(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Float(42.0));
    }

    #[test]
    fn test_str_from_int() {
        let result = str_fn(Value::Int(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("42".to_string()));
    }

    #[test]
    fn test_str_from_float() {
        let result = str_fn(Value::Float(2.14));
        assert!(result.is_ok());
    }

    #[test]
    fn test_bool_from_int_zero() {
        let result = bool_fn(Value::Int(0));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_bool_from_int_nonzero() {
        let result = bool_fn(Value::Int(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_bytes_from_string() {
        let result = bytes_fn(Value::Str("hello".to_string()));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Bytes(b) => {
                assert_eq!(b, vec![104, 101, 108, 108, 111]);
            }
            _ => panic!("Expected bytes"),
        }
    }

    #[test]
    fn test_list_from_tuple() {
        let tuple = Value::Tuple(vec![Value::Int(1), Value::Int(2)]);
        let result = list(tuple);
        assert!(result.is_ok());
    }

    #[test]
    fn test_tuple_from_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
        ])));
        let result = tuple_fn(list);
        assert!(result.is_ok());
    }

    #[test]
    fn test_dict_from_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Tuple(vec![Value::Str("a".to_string()), Value::Int(1)]),
            Value::Tuple(vec![Value::Str("b".to_string()), Value::Int(2)]),
        ])));
        let result = dict_fn(list);
        assert!(result.is_ok());
    }

    #[test]
    fn test_set_from_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Str("a".to_string()),
            Value::Str("b".to_string()),
        ])));
        let result = set_fn(list);
        assert!(result.is_ok());
    }

    #[test]
    fn test_frozenset_from_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![Value::Str(
            "a".to_string(),
        )])));
        let result = frozenset_fn(list);
        assert!(result.is_ok());
    }
}
