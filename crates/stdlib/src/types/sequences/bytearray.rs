use crate::value::Value;
use crate::utils::StdlibError;

pub fn append(ba: &mut Vec<u8>, item: u8) -> Result<Value, StdlibError> {
    ba.push(item);
    Ok(Value::None)
}

pub fn extend(ba: &mut Vec<u8>, other: &[u8]) -> Result<Value, StdlibError> {
    ba.extend_from_slice(other);
    Ok(Value::None)
}

pub fn clear(ba: &mut Vec<u8>) -> Result<Value, StdlibError> {
    ba.clear();
    Ok(Value::None)
}
