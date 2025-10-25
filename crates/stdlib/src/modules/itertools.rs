use crate::value::Value;

pub fn chain(iterables: Vec<Value>) -> Value {
    let mut result = Vec::new();
    for iterable in iterables {
        match iterable {
            Value::List(l) => result.extend(l.borrow().clone()),
            Value::Tuple(t) => result.extend(t),
            _ => {}
        }
    }
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn cycle(iterable: &Value) -> Value {
    iterable.clone()
}

pub fn repeat(value: Value, times: i64) -> Value {
    let mut result = Vec::new();
    for _ in 0..times {
        result.push(value.clone());
    }
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(result)))
}

pub fn count(start: i64, step: i64) -> Value {
    let mut result = Vec::new();
    let mut current = start;
    for _ in 0..100 {
        result.push(Value::Int(current));
        current += step;
    }
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(result)))
}
