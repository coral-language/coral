use crate::value::Value;
use crate::utils::StdlibError;

pub fn decode(bytes: &[u8], encoding: Option<&str>) -> Result<Value, StdlibError> {
    let enc = encoding.unwrap_or("utf-8");
    if enc == "utf-8" || enc == "utf8" {
        match String::from_utf8(bytes.to_vec()) {
            Ok(s) => Ok(Value::Str(s)),
            Err(_) => Err(StdlibError::ValueError("invalid utf-8 sequence".to_string())),
        }
    } else {
        Err(StdlibError::ValueError(format!("unknown encoding: {}", enc)))
    }
}

pub fn count(bytes: &[u8], sub: &[u8]) -> Value {
    let count = bytes.windows(sub.len()).filter(|window| *window == sub).count();
    Value::Int(count as i64)
}

pub fn find(bytes: &[u8], sub: &[u8]) -> Value {
    bytes.windows(sub.len())
        .position(|window| window == sub)
        .map(|pos| Value::Int(pos as i64))
        .unwrap_or(Value::Int(-1))
}

pub fn startswith(bytes: &[u8], prefix: &[u8]) -> Value {
    Value::Bool(bytes.starts_with(prefix))
}

pub fn endswith(bytes: &[u8], suffix: &[u8]) -> Value {
    Value::Bool(bytes.ends_with(suffix))
}
