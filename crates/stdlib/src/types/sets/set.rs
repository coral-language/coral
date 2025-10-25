use crate::utils::StdlibError;
use crate::value::Value;
use std::collections::HashSet;

pub fn add(set: &mut HashSet<String>, item: &str) -> Result<Value, StdlibError> {
    set.insert(item.to_string());
    Ok(Value::None)
}

pub fn remove(set: &mut HashSet<String>, item: &str) -> Result<Value, StdlibError> {
    if set.remove(item) {
        Ok(Value::None)
    } else {
        Err(StdlibError::ValueError("item not in set".to_string()))
    }
}

pub fn discard(set: &mut HashSet<String>, item: &str) -> Result<Value, StdlibError> {
    set.remove(item);
    Ok(Value::None)
}

pub fn clear(set: &mut HashSet<String>) -> Result<Value, StdlibError> {
    set.clear();
    Ok(Value::None)
}

pub fn pop(set: &mut HashSet<String>) -> Result<Value, StdlibError> {
    match set.iter().next() {
        Some(item) => {
            let s = item.clone();
            set.remove(&s);
            Ok(Value::Str(s))
        }
        None => Err(StdlibError::ValueError("pop from empty set".to_string())),
    }
}

pub fn union(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    let result: HashSet<String> = set.union(other).cloned().collect();
    Value::Set(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn intersection(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    let result: HashSet<String> = set.intersection(other).cloned().collect();
    Value::Set(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn difference(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    let result: HashSet<String> = set.difference(other).cloned().collect();
    Value::Set(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn symmetric_difference(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    let result: HashSet<String> = set.symmetric_difference(other).cloned().collect();
    Value::Set(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn issubset(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    Value::Bool(set.is_subset(other))
}

pub fn issuperset(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    Value::Bool(set.is_superset(other))
}

pub fn isdisjoint(set: &HashSet<String>, other: &HashSet<String>) -> Value {
    Value::Bool(set.is_disjoint(other))
}

pub fn copy(set: &HashSet<String>) -> Value {
    Value::Set(std::rc::Rc::new(std::cell::RefCell::new(set.clone())))
}
