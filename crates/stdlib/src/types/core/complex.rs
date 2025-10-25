use crate::value::Value;

pub fn complex_type(real: f64, imag: f64) -> Value {
    Value::Complex { real, imag }
}
