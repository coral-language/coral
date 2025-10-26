use crate::utils::StdlibError;
use crate::value::Value;

pub fn random() -> Value {
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasher, Hasher};
    let mut hasher = RandomState::new().build_hasher();
    hasher.write_u64(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as u64,
    );
    Value::Float((hasher.finish() as f64) / (u64::MAX as f64))
}

pub fn randint(a: i64, b: i64) -> Value {
    if a > b {
        return Value::Int(b);
    }
    let range = (b - a + 1) as f64;
    let r = match random() {
        Value::Float(f) => f,
        _ => 0.5,
    };
    Value::Int(a + (r * range) as i64)
}

pub fn choice(items: &[Value]) -> Result<Value, StdlibError> {
    if items.is_empty() {
        return Err(StdlibError::ValueError(
            "choice from empty sequence".to_string(),
        ));
    }
    let idx = match randint(0, items.len() as i64 - 1) {
        Value::Int(i) => i as usize,
        _ => 0,
    };
    Ok(items.get(idx).cloned().unwrap_or(Value::None))
}
