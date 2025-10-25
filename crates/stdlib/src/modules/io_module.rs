use crate::value::Value;
use std::collections::HashMap;

pub fn string_io() -> Value {
    Value::Instance {
        class_name: "StringIO".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}

pub fn bytes_io() -> Value {
    Value::Instance {
        class_name: "BytesIO".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}
