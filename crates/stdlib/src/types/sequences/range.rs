use crate::utils::StdlibError;
use crate::value::Value;

pub fn range_iter(start: i64, stop: i64, step: i64) -> Result<Vec<Value>, StdlibError> {
    if step == 0 {
        return Err(StdlibError::ValueError(
            "range() arg 3 must not be zero".to_string(),
        ));
    }

    let mut result = Vec::new();
    let mut i = start;

    if step > 0 {
        while i < stop {
            result.push(Value::Int(i));
            i += step;
        }
    } else {
        while i > stop {
            result.push(Value::Int(i));
            i += step;
        }
    }

    Ok(result)
}

pub fn len(start: i64, stop: i64, step: i64) -> Result<Value, StdlibError> {
    if step == 0 {
        return Err(StdlibError::ValueError(
            "range() step argument must not be zero".to_string(),
        ));
    }

    let length = if step > 0 {
        if start >= stop {
            0
        } else {
            (stop - start + step - 1) / step
        }
    } else if start <= stop {
        0
    } else {
        (start - stop - step - 1) / (-step)
    };

    Ok(Value::Int(length))
}
