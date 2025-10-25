use crate::utils::StdlibError;
use crate::value::Value;

pub fn upper(s: &str) -> Value {
    Value::Str(s.to_uppercase())
}

pub fn lower(s: &str) -> Value {
    Value::Str(s.to_lowercase())
}

pub fn title(s: &str) -> Value {
    let mut result = String::new();
    let mut capitalize_next = true;
    for ch in s.chars() {
        if ch.is_whitespace() {
            capitalize_next = true;
            result.push(ch);
        } else if capitalize_next {
            result.push_str(&ch.to_uppercase().to_string());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    Value::Str(result)
}

pub fn capitalize(s: &str) -> Value {
    let mut chars = s.chars();
    match chars.next() {
        None => Value::Str(String::new()),
        Some(first) => {
            let mut result = first.to_uppercase().to_string();
            result.push_str(&chars.as_str().to_lowercase());
            Value::Str(result)
        }
    }
}

pub fn casefold(s: &str) -> Value {
    Value::Str(s.to_lowercase())
}

pub fn swapcase(s: &str) -> Value {
    let result: String = s
        .chars()
        .map(|ch| {
            if ch.is_uppercase() {
                ch.to_lowercase().to_string()
            } else if ch.is_lowercase() {
                ch.to_uppercase().to_string()
            } else {
                ch.to_string()
            }
        })
        .collect();
    Value::Str(result)
}

pub fn strip(s: &str) -> Value {
    Value::Str(s.trim().to_string())
}

pub fn lstrip(s: &str) -> Value {
    Value::Str(s.trim_start().to_string())
}

pub fn rstrip(s: &str) -> Value {
    Value::Str(s.trim_end().to_string())
}

pub fn split(s: &str, sep: Option<&str>) -> Value {
    let parts: Vec<Value> = match sep {
        Some(separator) => s
            .split(separator)
            .map(|p| Value::Str(p.to_string()))
            .collect(),
        None => s
            .split_whitespace()
            .map(|p| Value::Str(p.to_string()))
            .collect(),
    };
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(parts)))
}

pub fn rsplit(s: &str, sep: Option<&str>) -> Value {
    let mut parts: Vec<Value> = match sep {
        Some(separator) => s
            .rsplit(separator)
            .map(|p| Value::Str(p.to_string()))
            .collect(),
        None => s
            .split_whitespace()
            .map(|p| Value::Str(p.to_string()))
            .collect(),
    };
    parts.reverse();
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(parts)))
}

pub fn splitlines(s: &str) -> Value {
    let parts: Vec<Value> = s.lines().map(|line| Value::Str(line.to_string())).collect();
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(parts)))
}

pub fn join(s: &str, items: &[Value]) -> Result<Value, StdlibError> {
    let strings: Result<Vec<String>, _> = items
        .iter()
        .map(|v| match v {
            Value::Str(s) => Ok(s.clone()),
            _ => Err(StdlibError::TypeError {
                expected: "str".to_string(),
                got: v.type_name().to_string(),
            }),
        })
        .collect();

    Ok(Value::Str(strings?.join(s)))
}

pub fn replace(s: &str, old: &str, new: &str) -> Value {
    Value::Str(s.replace(old, new))
}

pub fn find(s: &str, sub: &str) -> Value {
    match s.find(sub) {
        Some(pos) => Value::Int(pos as i64),
        None => Value::Int(-1),
    }
}

pub fn rfind(s: &str, sub: &str) -> Value {
    match s.rfind(sub) {
        Some(pos) => Value::Int(pos as i64),
        None => Value::Int(-1),
    }
}

pub fn index(s: &str, sub: &str) -> Result<Value, StdlibError> {
    match s.find(sub) {
        Some(pos) => Ok(Value::Int(pos as i64)),
        None => Err(StdlibError::ValueError("substring not found".to_string())),
    }
}

pub fn rindex(s: &str, sub: &str) -> Result<Value, StdlibError> {
    match s.rfind(sub) {
        Some(pos) => Ok(Value::Int(pos as i64)),
        None => Err(StdlibError::ValueError("substring not found".to_string())),
    }
}

pub fn count(s: &str, sub: &str) -> Value {
    let count = s.matches(sub).count();
    Value::Int(count as i64)
}

pub fn startswith(s: &str, prefix: &str) -> Value {
    Value::Bool(s.starts_with(prefix))
}

pub fn endswith(s: &str, suffix: &str) -> Value {
    Value::Bool(s.ends_with(suffix))
}

pub fn isdigit(s: &str) -> Value {
    Value::Bool(!s.is_empty() && s.chars().all(|c| c.is_ascii_digit()))
}

pub fn isalpha(s: &str) -> Value {
    Value::Bool(!s.is_empty() && s.chars().all(|c| c.is_alphabetic()))
}

pub fn isalnum(s: &str) -> Value {
    Value::Bool(!s.is_empty() && s.chars().all(|c| c.is_alphanumeric()))
}

pub fn isascii(s: &str) -> Value {
    Value::Bool(s.is_ascii())
}

pub fn isdecimal(s: &str) -> Value {
    Value::Bool(!s.is_empty() && s.chars().all(|c| c.is_ascii_digit()))
}

pub fn isidentifier(s: &str) -> Value {
    if s.is_empty() {
        return Value::Bool(false);
    }
    let first_valid = s
        .chars()
        .next()
        .map(|c| c.is_alphabetic() || c == '_')
        .unwrap_or(false);
    Value::Bool(first_valid && s.chars().all(|c| c.is_alphanumeric() || c == '_'))
}
