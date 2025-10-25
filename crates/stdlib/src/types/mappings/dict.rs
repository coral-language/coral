use crate::utils::StdlibError;
use crate::value::Value;
use std::collections::HashMap;

pub fn get(dict: &HashMap<String, Value>, key: &str, default: Option<Value>) -> Value {
    dict.get(key).cloned().or(default).unwrap_or(Value::None)
}

pub fn keys(dict: &HashMap<String, Value>) -> Value {
    let keys: Vec<Value> = dict.keys().map(|k| Value::Str(k.clone())).collect();
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(keys)))
}

pub fn values(dict: &HashMap<String, Value>) -> Value {
    let values: Vec<Value> = dict.values().cloned().collect();
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(values)))
}

pub fn items(dict: &HashMap<String, Value>) -> Value {
    let items: Vec<Value> = dict
        .iter()
        .map(|(k, v)| Value::Tuple(vec![Value::Str(k.clone()), v.clone()]))
        .collect();
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(items)))
}

pub fn pop(dict: &mut HashMap<String, Value>, key: &str, default: Option<Value>) -> Value {
    dict.remove(key).or(default).unwrap_or(Value::None)
}

pub fn pop_item(dict: &mut HashMap<String, Value>) -> Result<Value, StdlibError> {
    match dict.iter().next() {
        Some((k, v)) => {
            let key = k.clone();
            let value = v.clone();
            dict.remove(&key);
            Ok(Value::Tuple(vec![Value::Str(key), value]))
        }
        None => Err(StdlibError::ValueError("dictionary is empty".to_string())),
    }
}

pub fn clear(dict: &mut HashMap<String, Value>) -> Result<Value, StdlibError> {
    dict.clear();
    Ok(Value::None)
}

pub fn update(
    dict: &mut HashMap<String, Value>,
    other: &HashMap<String, Value>,
) -> Result<Value, StdlibError> {
    for (k, v) in other {
        dict.insert(k.clone(), v.clone());
    }
    Ok(Value::None)
}

pub fn setdefault(dict: &mut HashMap<String, Value>, key: &str, default: Option<Value>) -> Value {
    dict.entry(key.to_string())
        .or_insert_with(|| default.clone().unwrap_or(Value::None))
        .clone()
}

pub fn copy(dict: &HashMap<String, Value>) -> Value {
    Value::Dict(std::rc::Rc::new(std::cell::RefCell::new(dict.clone())))
}
