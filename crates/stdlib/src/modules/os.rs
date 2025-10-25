use crate::utils::StdlibError;
use crate::value::Value;

pub fn getcwd() -> Result<Value, StdlibError> {
    let cwd = std::env::current_dir().map_err(|e| StdlibError::OSError(e.to_string()))?;

    Ok(Value::Str(cwd.to_string_lossy().to_string()))
}

pub fn listdir(path: &str) -> Result<Value, StdlibError> {
    let entries = std::fs::read_dir(path)
        .map_err(|e| StdlibError::OSError(e.to_string()))?
        .filter_map(|entry| entry.ok())
        .map(|entry| Value::Str(entry.file_name().to_string_lossy().to_string()))
        .collect::<Vec<_>>();

    Ok(Value::List(std::rc::Rc::new(std::cell::RefCell::new(
        entries,
    ))))
}

pub fn getenv(key: &str) -> Value {
    std::env::var(key).map(Value::Str).unwrap_or(Value::None)
}

pub fn getpid() -> Value {
    Value::Int(std::process::id() as i64)
}
