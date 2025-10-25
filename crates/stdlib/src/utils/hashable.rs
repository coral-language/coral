use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn hash_value(value: &Value) -> Result<u64, String> {
    let mut hasher = DefaultHasher::new();
    match value {
        Value::None => {
            "None".hash(&mut hasher);
        }
        Value::Bool(b) => {
            b.hash(&mut hasher);
        }
        Value::Int(i) => {
            i.hash(&mut hasher);
        }
        Value::Float(f) => {
            f.to_bits().hash(&mut hasher);
        }
        Value::Str(s) => {
            s.hash(&mut hasher);
        }
        Value::Bytes(b) => {
            b.hash(&mut hasher);
        }
        Value::Tuple(items) => {
            for item in items {
                let item_hash = hash_value(item)?;
                item_hash.hash(&mut hasher);
            }
        }
        Value::FrozenSet(set) => {
            let mut sorted: Vec<_> = set.iter().collect();
            sorted.sort();
            for item in sorted {
                item.hash(&mut hasher);
            }
        }
        _ => {
            return Err(format!("unhashable type: '{}'", value.type_name()));
        }
    }
    Ok(hasher.finish())
}

pub fn is_hashable(value: &Value) -> bool {
    matches!(
        value,
        Value::None
            | Value::Bool(_)
            | Value::Int(_)
            | Value::Float(_)
            | Value::Str(_)
            | Value::Bytes(_)
            | Value::Tuple(_)
            | Value::FrozenSet(_)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_none() {
        let result = hash_value(&Value::None);
        assert!(result.is_ok());
        let h1 = result.unwrap();
        let h2 = hash_value(&Value::None).unwrap();
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_bool() {
        let h_true = hash_value(&Value::Bool(true)).unwrap();
        let h_false = hash_value(&Value::Bool(false)).unwrap();
        assert_ne!(h_true, h_false);

        let h_true2 = hash_value(&Value::Bool(true)).unwrap();
        assert_eq!(h_true, h_true2);
    }

    #[test]
    fn test_hash_int() {
        let h1 = hash_value(&Value::Int(42)).unwrap();
        let h2 = hash_value(&Value::Int(42)).unwrap();
        assert_eq!(h1, h2);

        let h3 = hash_value(&Value::Int(43)).unwrap();
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_hash_float() {
        let h1 = hash_value(&Value::Float(2.14)).unwrap();
        let h2 = hash_value(&Value::Float(2.14)).unwrap();
        assert_eq!(h1, h2);

        let h3 = hash_value(&Value::Float(2.71)).unwrap();
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_hash_string() {
        let h1 = hash_value(&Value::Str("hello".to_string())).unwrap();
        let h2 = hash_value(&Value::Str("hello".to_string())).unwrap();
        assert_eq!(h1, h2);

        let h3 = hash_value(&Value::Str("world".to_string())).unwrap();
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_hash_bytes() {
        let h1 = hash_value(&Value::Bytes(vec![1, 2, 3])).unwrap();
        let h2 = hash_value(&Value::Bytes(vec![1, 2, 3])).unwrap();
        assert_eq!(h1, h2);

        let h3 = hash_value(&Value::Bytes(vec![1, 2, 4])).unwrap();
        assert_ne!(h1, h3);
    }

    #[test]
    fn test_hash_tuple() {
        let t1 = Value::Tuple(vec![Value::Int(1), Value::Str("a".to_string())]);
        let t2 = Value::Tuple(vec![Value::Int(1), Value::Str("a".to_string())]);

        let h1 = hash_value(&t1).unwrap();
        let h2 = hash_value(&t2).unwrap();
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_tuple_different() {
        let t1 = Value::Tuple(vec![Value::Int(1)]);
        let t2 = Value::Tuple(vec![Value::Int(2)]);

        let h1 = hash_value(&t1).unwrap();
        let h2 = hash_value(&t2).unwrap();
        assert_ne!(h1, h2);
    }

    #[test]
    fn test_hash_frozenset() {
        let mut set1 = std::collections::HashSet::new();
        set1.insert("a".to_string());
        set1.insert("b".to_string());

        let mut set2 = std::collections::HashSet::new();
        set2.insert("a".to_string());
        set2.insert("b".to_string());

        let h1 = hash_value(&Value::FrozenSet(set1)).unwrap();
        let h2 = hash_value(&Value::FrozenSet(set2)).unwrap();
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_unhashable_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        assert!(hash_value(&list).is_err());
    }

    #[test]
    fn test_hash_unhashable_dict() {
        let dict = Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(
            std::collections::HashMap::new(),
        )));
        assert!(hash_value(&dict).is_err());
    }

    #[test]
    fn test_hash_unhashable_set() {
        let set = Value::Set(std::rc::Rc::new(std::cell::RefCell::new(
            std::collections::HashSet::new(),
        )));
        assert!(hash_value(&set).is_err());
    }

    #[test]
    fn test_is_hashable_hashable_types() {
        assert!(is_hashable(&Value::None));
        assert!(is_hashable(&Value::Bool(true)));
        assert!(is_hashable(&Value::Int(42)));
        assert!(is_hashable(&Value::Float(2.14)));
        assert!(is_hashable(&Value::Str("hello".to_string())));
        assert!(is_hashable(&Value::Bytes(vec![1, 2, 3])));

        let tuple = Value::Tuple(vec![Value::Int(1)]);
        assert!(is_hashable(&tuple));

        let set = std::collections::HashSet::new();
        assert!(is_hashable(&Value::FrozenSet(set)));
    }

    #[test]
    fn test_is_hashable_unhashable_types() {
        assert!(!is_hashable(&Value::List(std::rc::Rc::new(
            std::cell::RefCell::new(vec![])
        ))));
        assert!(!is_hashable(&Value::Dict(std::rc::Rc::new(
            std::cell::RefCell::new(std::collections::HashMap::new())
        ))));
        assert!(!is_hashable(&Value::Set(std::rc::Rc::new(
            std::cell::RefCell::new(std::collections::HashSet::new())
        ))));
        assert!(!is_hashable(&Value::Function {
            name: "f".to_string(),
            arity: 0
        }));
    }

    #[test]
    fn test_hash_consistency() {
        let v = Value::Str("test".to_string());
        let h1 = hash_value(&v).unwrap();
        let h2 = hash_value(&v).unwrap();
        let h3 = hash_value(&v).unwrap();

        assert_eq!(h1, h2);
        assert_eq!(h2, h3);
    }

    #[test]
    fn test_hash_different_types_different_hash() {
        let h_int = hash_value(&Value::Int(1)).unwrap();
        let h_bool = hash_value(&Value::Bool(true)).unwrap();
        assert_ne!(h_int, h_bool);
    }

    #[test]
    fn test_hash_nested_tuple() {
        let inner = Value::Tuple(vec![Value::Int(1), Value::Int(2)]);
        let outer = Value::Tuple(vec![inner.clone(), Value::Str("test".to_string())]);

        let result = hash_value(&outer);
        assert!(result.is_ok());

        let h1 = result.unwrap();
        let h2 = hash_value(&outer).unwrap();
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_empty_tuple() {
        let empty = Value::Tuple(vec![]);
        let result = hash_value(&empty);
        assert!(result.is_ok());

        let h1 = result.unwrap();
        let h2 = hash_value(&empty).unwrap();
        assert_eq!(h1, h2);
    }
}
