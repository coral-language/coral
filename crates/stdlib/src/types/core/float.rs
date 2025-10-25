use crate::value::Value;

pub fn float_type(value: f64) -> Value {
    Value::Float(value)
}

pub fn is_finite(value: f64) -> bool {
    value.is_finite()
}

pub fn is_infinite(value: f64) -> bool {
    value.is_infinite()
}

pub fn is_nan(value: f64) -> bool {
    value.is_nan()
}
