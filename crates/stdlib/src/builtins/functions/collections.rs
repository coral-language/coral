use crate::utils::StdlibError;
use crate::value::Value;

pub fn len(obj: Value) -> Result<Value, StdlibError> {
    match obj {
        Value::Str(s) => Ok(Value::Int(s.len() as i64)),
        Value::Bytes(b) => Ok(Value::Int(b.len() as i64)),
        Value::List(l) => Ok(Value::Int(l.borrow().len() as i64)),
        Value::Tuple(t) => Ok(Value::Int(t.len() as i64)),
        Value::Dict(d) => Ok(Value::Int(d.borrow().len() as i64)),
        Value::Set(s) => Ok(Value::Int(s.borrow().len() as i64)),
        Value::FrozenSet(fs) => Ok(Value::Int(fs.len() as i64)),
        Value::Range { start, stop, step } => {
            let count = if step == 0 {
                0
            } else if step > 0 {
                if start >= stop {
                    0
                } else {
                    (stop - start + step - 1) / step
                }
            } else if start <= stop {
                0
            } else {
                (start - stop - step - 1) / (-step)
            };
            Ok(Value::Int(count))
        }
        _ => Err(StdlibError::TypeError {
            expected: "sequence or collection".to_string(),
            got: obj.type_name().to_string(),
        }),
    }
}

pub fn sum(iterable: Value, start: Option<Value>) -> Result<Value, StdlibError> {
    let initial = match start {
        Some(s) => s,
        None => Value::Int(0),
    };

    match iterable {
        Value::List(l) => {
            let mut result = initial;
            for item in l.borrow().iter() {
                result = add_values(&result, item)?;
            }
            Ok(result)
        }
        Value::Tuple(t) => {
            let mut result = initial;
            for item in t {
                result = add_values(&result, &item)?;
            }
            Ok(result)
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

pub fn min(values: Vec<Value>) -> Result<Value, StdlibError> {
    if values.is_empty() {
        return Err(StdlibError::ValueError(
            "min() arg is an empty sequence".to_string(),
        ));
    }

    let mut result = values[0].clone();
    for val in &values[1..] {
        if compare_lt(val, &result)? {
            result = val.clone();
        }
    }
    Ok(result)
}

pub fn max(values: Vec<Value>) -> Result<Value, StdlibError> {
    if values.is_empty() {
        return Err(StdlibError::ValueError(
            "max() arg is an empty sequence".to_string(),
        ));
    }

    let mut result = values[0].clone();
    for val in &values[1..] {
        if compare_gt(val, &result)? {
            result = val.clone();
        }
    }
    Ok(result)
}

pub fn sorted(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(l) => {
            let mut items = l.borrow().clone();
            items.sort_by(|a, b| {
                if compare_lt(a, b).unwrap_or(false) {
                    std::cmp::Ordering::Less
                } else if compare_gt(a, b).unwrap_or(false) {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Equal
                }
            });
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                items,
            ))))
        }
        Value::Tuple(t) => {
            let mut items = t.clone();
            items.sort_by(|a, b| {
                if compare_lt(a, b).unwrap_or(false) {
                    std::cmp::Ordering::Less
                } else if compare_gt(a, b).unwrap_or(false) {
                    std::cmp::Ordering::Greater
                } else {
                    std::cmp::Ordering::Equal
                }
            });
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                items,
            ))))
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

pub fn reversed(iterable: Value) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(l) => {
            let mut items = l.borrow().clone();
            items.reverse();
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                items,
            ))))
        }
        Value::Tuple(mut t) => {
            t.reverse();
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(t))))
        }
        _ => Err(StdlibError::TypeError {
            expected: "sequence".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

fn add_values(a: &Value, b: &Value) -> Result<Value, StdlibError> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
        (Value::Int(x), Value::Float(y)) => Ok(Value::Float(*x as f64 + y)),
        (Value::Float(x), Value::Int(y)) => Ok(Value::Float(x + *y as f64)),
        (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
        (Value::Str(x), Value::Str(y)) => Ok(Value::Str(format!("{}{}", x, y))),
        _ => Err(StdlibError::TypeError {
            expected: "numeric or compatible types".to_string(),
            got: format!("{} and {}", a.type_name(), b.type_name()),
        }),
    }
}

fn compare_lt(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    crate::utils::comparison::is_less(a, b)
}

fn compare_gt(a: &Value, b: &Value) -> Result<bool, StdlibError> {
    crate::utils::comparison::is_greater(a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_len_string() {
        let result = len(Value::Str("hello".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(5));
    }

    #[test]
    fn test_len_empty_string() {
        let result = len(Value::Str("".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn test_len_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ])));
        let result = len(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_len_empty_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        let result = len(list);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn test_len_tuple() {
        let tuple = Value::Tuple(vec![Value::Int(1), Value::Int(2)]);
        let result = len(tuple);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(2));
    }

    #[test]
    fn test_len_dict() {
        let mut map = std::collections::HashMap::new();
        map.insert("a".to_string(), Value::Int(1));
        map.insert("b".to_string(), Value::Int(2));
        let dict = Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(map)));
        let result = len(dict);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(2));
    }

    #[test]
    fn test_len_range() {
        let range = Value::Range {
            start: 0,
            stop: 10,
            step: 1,
        };
        let result = len(range);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(10));
    }

    #[test]
    fn test_len_invalid_type() {
        let result = len(Value::Int(42));
        assert!(result.is_err());
    }

    #[test]
    fn test_sum_integers() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ])));
        let result = sum(list, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(6));
    }

    #[test]
    fn test_sum_floats() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Float(1.5),
            Value::Float(2.5),
        ])));
        let result = sum(list, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Float(f) => assert!((f - 4.0).abs() < 0.001),
            _ => panic!("Expected float"),
        }
    }

    #[test]
    fn test_sum_with_start() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
        ])));
        let result = sum(list, Some(Value::Int(10)));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(13));
    }

    #[test]
    fn test_sum_empty_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        let result = sum(list, None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn test_min_integers() {
        let result = min(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(1));
    }

    #[test]
    fn test_min_strings() {
        let result = min(vec![
            Value::Str("zebra".to_string()),
            Value::Str("apple".to_string()),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("apple".to_string()));
    }

    #[test]
    fn test_min_empty() {
        let result = min(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_max_integers() {
        let result = max(vec![Value::Int(3), Value::Int(1), Value::Int(2)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_max_strings() {
        let result = max(vec![
            Value::Str("apple".to_string()),
            Value::Str("zebra".to_string()),
        ]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("zebra".to_string()));
    }

    #[test]
    fn test_max_empty() {
        let result = max(vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_sorted_integers() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(3),
            Value::Int(1),
            Value::Int(2),
        ])));
        let result = sorted(list);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                let items = l.borrow();
                assert_eq!(items[0], Value::Int(1));
                assert_eq!(items[1], Value::Int(2));
                assert_eq!(items[2], Value::Int(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_sorted_reverse() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(3),
            Value::Int(1),
            Value::Int(2),
        ])));
        let result = sorted(list);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                let items = l.borrow();
                assert_eq!(items[0], Value::Int(1));
                assert_eq!(items[1], Value::Int(2));
                assert_eq!(items[2], Value::Int(3));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_reversed_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
            Value::Int(3),
        ])));
        let result = reversed(list);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                let items = l.borrow();
                assert_eq!(items[0], Value::Int(3));
                assert_eq!(items[1], Value::Int(2));
                assert_eq!(items[2], Value::Int(1));
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_reversed_tuple() {
        let tuple = Value::Tuple(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let result = reversed(tuple);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                let items = l.borrow();
                assert_eq!(items[0], Value::Int(3));
                assert_eq!(items[1], Value::Int(2));
                assert_eq!(items[2], Value::Int(1));
            }
            _ => panic!("Expected list"),
        }
    }
}
