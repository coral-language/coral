use crate::utils::StdlibError;
use crate::value::Value;

pub fn type_of(value: Value) -> Result<Value, StdlibError> {
    Ok(Value::Str(value.type_name().to_string()))
}

pub fn isinstance(obj: Value, class_or_tuple: Value) -> Result<Value, StdlibError> {
    let obj_type = obj.type_name();

    match class_or_tuple {
        Value::Str(type_name) => Ok(Value::Bool(obj_type == type_name)),
        Value::Tuple(types) => {
            for t in types {
                if let Value::Str(type_name) = t
                    && obj_type == type_name
                {
                    return Ok(Value::Bool(true));
                }
            }
            Ok(Value::Bool(false))
        }
        _ => Err(StdlibError::TypeError {
            expected: "type or tuple of types".to_string(),
            got: class_or_tuple.type_name().to_string(),
        }),
    }
}

pub fn issubclass(subclass: Value, superclass: Value) -> Result<Value, StdlibError> {
    match (subclass, superclass) {
        (Value::Str(sub), Value::Str(sup)) => Ok(Value::Bool(
            sub == sup || sub.ends_with(&format!("_{}", sup)),
        )),
        _ => Err(StdlibError::TypeError {
            expected: "type".to_string(),
            got: "invalid class type".to_string(),
        }),
    }
}

pub fn callable(obj: Value) -> Result<Value, StdlibError> {
    Ok(Value::Bool(matches!(
        obj,
        Value::Function { .. } | Value::BuiltinFunction { .. } | Value::Class { .. }
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_of_primitives() {
        assert_eq!(
            type_of(Value::None).unwrap(),
            Value::Str("NoneType".to_string())
        );
        assert_eq!(
            type_of(Value::Bool(true)).unwrap(),
            Value::Str("bool".to_string())
        );
        assert_eq!(
            type_of(Value::Int(42)).unwrap(),
            Value::Str("int".to_string())
        );
        assert_eq!(
            type_of(Value::Float(2.14)).unwrap(),
            Value::Str("float".to_string())
        );
        assert_eq!(
            type_of(Value::Str("test".to_string())).unwrap(),
            Value::Str("str".to_string())
        );
    }

    #[test]
    fn test_type_of_collections() {
        let list = Value::List(std::rc::Rc::new(std::cell::RefCell::new(vec![])));
        assert_eq!(type_of(list).unwrap(), Value::Str("list".to_string()));

        let tuple = Value::Tuple(vec![]);
        assert_eq!(type_of(tuple).unwrap(), Value::Str("tuple".to_string()));

        let dict = Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(
            std::collections::HashMap::new(),
        )));
        assert_eq!(type_of(dict).unwrap(), Value::Str("dict".to_string()));

        let set = Value::Set(std::rc::Rc::new(std::cell::RefCell::new(
            std::collections::HashSet::new(),
        )));
        assert_eq!(type_of(set).unwrap(), Value::Str("set".to_string()));
    }

    #[test]
    fn test_isinstance_with_string_type() {
        let result = isinstance(Value::Int(42), Value::Str("int".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));

        let result = isinstance(Value::Int(42), Value::Str("str".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_isinstance_with_tuple_of_types() {
        let result = isinstance(
            Value::Int(42),
            Value::Tuple(vec![
                Value::Str("str".to_string()),
                Value::Str("int".to_string()),
            ]),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));

        let result = isinstance(
            Value::Int(42),
            Value::Tuple(vec![
                Value::Str("str".to_string()),
                Value::Str("float".to_string()),
            ]),
        );
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_isinstance_with_invalid_type_arg() {
        let result = isinstance(Value::Int(42), Value::Int(123));
        assert!(result.is_err());
    }

    #[test]
    fn test_issubclass_valid() {
        let result = issubclass(Value::Str("int".to_string()), Value::Str("int".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_issubclass_invalid() {
        let result = issubclass(Value::Str("int".to_string()), Value::Str("str".to_string()));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_callable_function() {
        let result = callable(Value::BuiltinFunction {
            name: "print".to_string(),
        });
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_callable_non_function() {
        let result = callable(Value::Int(42));
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Bool(false));
    }
}
