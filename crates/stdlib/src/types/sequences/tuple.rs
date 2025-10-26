use crate::utils::StdlibError;
use crate::value::Value;

pub fn count(tuple: &[Value], item: &Value) -> Result<Value, StdlibError> {
    let count = tuple.iter().filter(|v| *v == item).count();
    Ok(Value::Int(count as i64))
}

pub fn index(tuple: &[Value], item: &Value) -> Result<Value, StdlibError> {
    for (i, v) in tuple.iter().enumerate() {
        if v == item {
            return Ok(Value::Int(i as i64));
        }
    }
    Err(StdlibError::ValueError("value not in tuple".to_string()))
}
