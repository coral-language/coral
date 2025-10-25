use crate::utils::StdlibError;
use crate::value::Value;

pub fn append(
    list: &std::rc::Rc<std::cell::RefCell<Vec<Value>>>,
    item: Value,
) -> Result<Value, StdlibError> {
    list.borrow_mut().push(item);
    Ok(Value::None)
}

pub fn extend(
    list: &std::rc::Rc<std::cell::RefCell<Vec<Value>>>,
    iterable: &Value,
) -> Result<Value, StdlibError> {
    match iterable {
        Value::List(other) => {
            list.borrow_mut().extend(other.borrow().clone());
            Ok(Value::None)
        }
        Value::Tuple(items) => {
            list.borrow_mut().extend(items.clone());
            Ok(Value::None)
        }
        _ => Err(StdlibError::TypeError {
            expected: "iterable".to_string(),
            got: iterable.type_name().to_string(),
        }),
    }
}

pub fn copy(list: &std::rc::Rc<std::cell::RefCell<Vec<Value>>>) -> Value {
    Value::List(std::rc::Rc::new(std::cell::RefCell::new(
        list.borrow().clone(),
    )))
}
