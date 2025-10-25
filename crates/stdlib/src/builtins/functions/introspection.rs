use crate::utils::StdlibError;
use crate::value::Value;

pub fn getattr(obj: Value, name: Value, default: Option<Value>) -> Result<Value, StdlibError> {
    match (obj, name) {
        (Value::Instance { attributes, .. }, Value::Str(attr_name)) => {
            let attrs = attributes.borrow();
            Ok(attrs
                .get(&attr_name)
                .cloned()
                .or(default)
                .unwrap_or(Value::None))
        }
        _ => Ok(default.unwrap_or(Value::None)),
    }
}

pub fn setattr(obj: Value, name: Value, value: Value) -> Result<Value, StdlibError> {
    match (obj, name) {
        (Value::Instance { attributes, .. }, Value::Str(attr_name)) => {
            attributes.borrow_mut().insert(attr_name, value);
            Ok(Value::None)
        }
        _ => Err(StdlibError::AttributeError(
            "cannot set attribute".to_string(),
        )),
    }
}

pub fn hasattr(obj: Value, name: Value) -> Result<Value, StdlibError> {
    match (obj, name) {
        (Value::Instance { attributes, .. }, Value::Str(attr_name)) => {
            Ok(Value::Bool(attributes.borrow().contains_key(&attr_name)))
        }
        _ => Ok(Value::Bool(false)),
    }
}

pub fn delattr(obj: Value, name: Value) -> Result<Value, StdlibError> {
    match (obj, name) {
        (Value::Instance { attributes, .. }, Value::Str(attr_name)) => {
            attributes.borrow_mut().remove(&attr_name);
            Ok(Value::None)
        }
        _ => Err(StdlibError::AttributeError(
            "cannot delete attribute".to_string(),
        )),
    }
}

pub fn dir(obj: Option<Value>) -> Result<Value, StdlibError> {
    match obj {
        Some(Value::Instance { attributes, .. }) => {
            let attrs = attributes.borrow();
            let names: Vec<Value> = attrs.keys().map(|k| Value::Str(k.clone())).collect();
            Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
                names,
            ))))
        }
        _ => Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
            Vec::new(),
        )))),
    }
}

pub fn vars(obj: Option<Value>) -> Result<Value, StdlibError> {
    match obj {
        Some(Value::Instance { attributes, .. }) => {
            let attrs = attributes.borrow();
            let mut dict = std::collections::HashMap::new();
            for (k, v) in attrs.iter() {
                dict.insert(k.clone(), v.clone());
            }
            Ok(Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(dict))))
        }
        _ => Err(StdlibError::TypeError {
            expected: "instance".to_string(),
            got: "None".to_string(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_getattr_existing_attribute() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::Str("test".to_string()));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = getattr(instance, Value::Str("name".to_string()), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("test".to_string()));
    }

    #[test]
    fn test_getattr_missing_with_default() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };

        let result = getattr(
            instance,
            Value::Str("missing".to_string()),
            Some(Value::Str("default".to_string())),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Str("default".to_string()));
    }

    #[test]
    fn test_getattr_missing_no_default() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };

        let result = getattr(instance, Value::Str("missing".to_string()), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_getattr_non_instance() {
        let result = getattr(Value::Int(42), Value::Str("attr".to_string()), None);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::None);
    }

    #[test]
    fn test_setattr_new_attribute() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };

        let result = setattr(
            instance.clone(),
            Value::Str("new_attr".to_string()),
            Value::Int(42),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_setattr_update_attribute() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("value".to_string(), Value::Int(1));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = setattr(instance, Value::Str("value".to_string()), Value::Int(2));
        assert!(result.is_ok());
    }

    #[test]
    fn test_setattr_non_instance() {
        let result = setattr(
            Value::Int(42),
            Value::Str("attr".to_string()),
            Value::Int(1),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_hasattr_existing() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("attr".to_string(), Value::Int(1));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = hasattr(instance, Value::Str("attr".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_hasattr_missing() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };

        let result = hasattr(instance, Value::Str("missing".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_hasattr_non_instance() {
        let result = hasattr(Value::Int(42), Value::Str("attr".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_delattr_existing() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("attr".to_string(), Value::Int(1));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = delattr(instance, Value::Str("attr".to_string()));
        assert!(result.is_ok());
    }

    #[test]
    fn test_delattr_non_instance() {
        let result = delattr(Value::Int(42), Value::Str("attr".to_string()));
        assert!(result.is_err());
    }

    #[test]
    fn test_dir_instance_with_attributes() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("attr1".to_string(), Value::Int(1));
        attrs.insert("attr2".to_string(), Value::Str("test".to_string()));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = dir(Some(instance));
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                let items = l.borrow();
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_dir_instance_no_attributes() {
        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
        };

        let result = dir(Some(instance));
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                assert_eq!(l.borrow().len(), 0);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_dir_none() {
        let result = dir(None);
        assert!(result.is_ok());

        match result.unwrap() {
            Value::List(l) => {
                assert_eq!(l.borrow().len(), 0);
            }
            _ => panic!("Expected list"),
        }
    }

    #[test]
    fn test_vars_instance() {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("a".to_string(), Value::Int(1));
        attrs.insert("b".to_string(), Value::Str("test".to_string()));

        let instance = Value::Instance {
            class_name: "TestClass".to_string(),
            attributes: std::rc::Rc::new(std::cell::RefCell::new(attrs)),
        };

        let result = vars(Some(instance));
        assert!(result.is_ok());

        match result.unwrap() {
            Value::Dict(d) => {
                assert_eq!(d.borrow().len(), 2);
            }
            _ => panic!("Expected dict"),
        }
    }

    #[test]
    fn test_vars_non_instance() {
        let result = vars(Some(Value::Int(42)));
        assert!(result.is_err());
    }

    #[test]
    fn test_vars_none() {
        let result = vars(None);
        assert!(result.is_err());
    }
}
