use crate::value::Value;
use crate::utils::StdlibError;

pub fn dumps(obj: &Value) -> Result<Value, StdlibError> {
    let json_str = match obj {
        Value::None => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Str(s) => format!("\"{}\"", s.replace("\"", "\\\"")),
        Value::List(l) => {
            let items: Vec<String> = l.borrow().iter()
                .map(|v| match dumps(v) {
                    Ok(Value::Str(s)) => s,
                    _ => "null".to_string(),
                })
                .collect();
            format!("[{}]", items.join(","))
        }
        Value::Dict(d) => {
            let items: Vec<String> = d.borrow().iter()
                .map(|(k, v)| {
                    let v_str = match dumps(v) {
                        Ok(Value::Str(s)) => s,
                        _ => "null".to_string(),
                    };
                    format!("\"{}\":{}", k, v_str)
                })
                .collect();
            format!("{{{}}}", items.join(","))
        }
        _ => "null".to_string(),
    };
    Ok(Value::Str(json_str))
}

pub fn loads(_json_str: &str) -> Result<Value, StdlibError> {
    Err(StdlibError::NotImplementedError("JSON parsing not yet implemented".to_string()))
}
