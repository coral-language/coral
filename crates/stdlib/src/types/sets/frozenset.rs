use crate::value::Value;
use std::collections::HashSet;

pub fn copy(set: &HashSet<String>) -> Value {
    Value::FrozenSet(set.clone())
}
