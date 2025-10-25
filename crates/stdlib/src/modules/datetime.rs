use crate::value::Value;
use std::collections::HashMap;

pub fn datetime() -> Value {
    Value::Instance {
        class_name: "datetime".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}

pub fn date() -> Value {
    Value::Instance {
        class_name: "date".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}

pub fn time() -> Value {
    Value::Instance {
        class_name: "time".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}

pub fn timedelta() -> Value {
    Value::Instance {
        class_name: "timedelta".to_string(),
        attributes: std::rc::Rc::new(std::cell::RefCell::new(HashMap::new())),
    }
}
