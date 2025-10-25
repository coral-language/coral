use crate::utils::StdlibError;
use crate::value::Value;

pub fn get_argv() -> Result<Value, StdlibError> {
    let args: Vec<Value> = std::env::args().map(Value::Str).collect();

    Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(args))))
}

pub fn get_version() -> Value {
    Value::Str("0.1.0".to_string())
}

pub fn get_platform() -> Value {
    Value::Str(std::env::consts::OS.to_string())
}
