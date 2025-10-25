use crate::value::Value;

pub fn bool_type(value: bool) -> Value {
    Value::Bool(value)
}
