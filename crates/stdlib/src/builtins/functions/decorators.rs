use crate::utils::StdlibError;
use crate::value::Value;

pub fn classmethod(func: Value) -> Result<Value, StdlibError> {
    Ok(func)
}

pub fn staticmethod(func: Value) -> Result<Value, StdlibError> {
    Ok(func)
}

pub fn property(
    _fget: Option<Value>,
    _fset: Option<Value>,
    _fdel: Option<Value>,
) -> Result<Value, StdlibError> {
    Ok(Value::Instance {
        class_name: "property".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
    })
}

pub fn super_fn(_type_: Option<Value>, _obj: Option<Value>) -> Result<Value, StdlibError> {
    Ok(Value::Instance {
        class_name: "super".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(std::collections::HashMap::new())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classmethod() {
        let func = Value::Function {
            name: "test_func".to_string(),
            arity: 0,
        };
        let result = classmethod(func);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Function { name, arity } => {
                assert_eq!(name, "test_func");
                assert_eq!(arity, 0);
            }
            _ => panic!("Expected function"),
        }
    }

    #[test]
    fn test_staticmethod() {
        let func = Value::Function {
            name: "test_func".to_string(),
            arity: 0,
        };
        let result = staticmethod(func);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Function { name, arity } => {
                assert_eq!(name, "test_func");
                assert_eq!(arity, 0);
            }
            _ => panic!("Expected function"),
        }
    }

    #[test]
    fn test_property() {
        let result = property(None, None, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Instance { class_name, .. } => {
                assert_eq!(class_name, "property");
            }
            _ => panic!("Expected instance"),
        }
    }

    #[test]
    fn test_property_with_getter() {
        let getter = Some(Value::Function {
            name: "getter".to_string(),
            arity: 0,
        });
        let result = property(getter, None, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Instance { class_name, .. } => {
                assert_eq!(class_name, "property");
            }
            _ => panic!("Expected instance"),
        }
    }

    #[test]
    fn test_super() {
        let result = super_fn(None, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Instance { class_name, .. } => {
                assert_eq!(class_name, "super");
            }
            _ => panic!("Expected instance"),
        }
    }

    #[test]
    fn test_super_with_type() {
        let type_val = Some(Value::Str("BaseClass".to_string()));
        let result = super_fn(type_val, None);
        assert!(result.is_ok());
        match result.unwrap() {
            Value::Instance { class_name, .. } => {
                assert_eq!(class_name, "super");
            }
            _ => panic!("Expected instance"),
        }
    }
}
