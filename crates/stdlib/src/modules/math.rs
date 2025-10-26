use crate::utils::StdlibError;
use crate::value::Value;
use std::f64::consts;

pub const PI: f64 = consts::PI;
pub const E: f64 = consts::E;
pub const TAU: f64 = consts::TAU;
pub const INF: f64 = f64::INFINITY;
pub const NAN: f64 = f64::NAN;

pub fn sqrt(x: f64) -> Result<Value, StdlibError> {
    Ok(Value::Float(x.sqrt()))
}

pub fn sin(x: f64) -> Value {
    Value::Float(x.sin())
}
pub fn cos(x: f64) -> Value {
    Value::Float(x.cos())
}
pub fn tan(x: f64) -> Value {
    Value::Float(x.tan())
}
pub fn asin(x: f64) -> Value {
    Value::Float(x.asin())
}
pub fn acos(x: f64) -> Value {
    Value::Float(x.acos())
}
pub fn atan(x: f64) -> Value {
    Value::Float(x.atan())
}
pub fn atan2(y: f64, x: f64) -> Value {
    Value::Float(y.atan2(x))
}

pub fn sinh(x: f64) -> Value {
    Value::Float(x.sinh())
}
pub fn cosh(x: f64) -> Value {
    Value::Float(x.cosh())
}
pub fn tanh(x: f64) -> Value {
    Value::Float(x.tanh())
}

pub fn exp(x: f64) -> Value {
    Value::Float(x.exp())
}
pub fn log(x: f64) -> Value {
    Value::Float(x.ln())
}
pub fn log10(x: f64) -> Value {
    Value::Float(x.log10())
}
pub fn log2(x: f64) -> Value {
    Value::Float(x.log2())
}

pub fn ceil(x: f64) -> Value {
    Value::Int(x.ceil() as i64)
}
pub fn floor(x: f64) -> Value {
    Value::Int(x.floor() as i64)
}
pub fn trunc(x: f64) -> Value {
    Value::Int(x.trunc() as i64)
}

pub fn factorial(n: i64) -> Result<Value, StdlibError> {
    if n < 0 {
        return Err(StdlibError::ValueError(
            "factorial not defined for negative values".to_string(),
        ));
    }
    let mut result: i64 = 1;
    for i in 2..=n {
        result = result
            .checked_mul(i)
            .ok_or_else(|| StdlibError::RuntimeError("factorial overflow".to_string()))?;
    }
    Ok(Value::Int(result))
}

pub fn gcd(mut a: i64, mut b: i64) -> Value {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    Value::Int(a.abs())
}

pub fn lcm(a: i64, b: i64) -> Value {
    if a == 0 || b == 0 {
        Value::Int(0)
    } else {
        let gcd_val = match gcd(a, b) {
            Value::Int(g) => g,
            _ => return Value::Int(0),
        };
        Value::Int((a / gcd_val * b).abs())
    }
}
