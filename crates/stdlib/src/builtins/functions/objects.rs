use crate::utils::StdlibError;
use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

pub fn id(_obj: Value) -> Result<Value, StdlibError> {
    let mut hasher = DefaultHasher::new();
    format!("{:p}", &_obj).hash(&mut hasher);
    Ok(Value::Int(hasher.finish() as i64))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_int() {
        let result = id(Value::Int(42));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }

    #[test]
    fn test_id_string() {
        let result = id(Value::Str("hello".to_string()));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }

    #[test]
    fn test_id_bool() {
        let result = id(Value::Bool(true));
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }

    #[test]
    fn test_id_list() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![
            Value::Int(1),
            Value::Int(2),
        ])));
        let result = id(list);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }

    #[test]
    fn test_id_none() {
        let result = id(Value::None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }

    #[test]
    fn test_id_instance() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };
        let result = id(instance);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Int(_) => {}
            _ => panic!("Expected int"),
        }
    }
}
