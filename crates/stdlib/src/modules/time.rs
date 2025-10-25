use crate::value::Value;

pub fn time() -> Value {
    match std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => Value::Float(d.as_secs_f64()),
        Err(_) => Value::Float(0.0),
    }
}

pub fn sleep(seconds: f64) {
    let duration = std::time::Duration::from_secs_f64(seconds);
    std::thread::sleep(duration);
}
