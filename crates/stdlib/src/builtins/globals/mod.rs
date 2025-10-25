pub mod constants;
pub mod exceptions;

pub use exceptions::{
    ATTRIBUTE_ERROR, BASE_EXCEPTION, EXCEPTION, ExceptionType, INDEX_ERROR, KEY_ERROR, NAME_ERROR,
    RUNTIME_ERROR, TYPE_ERROR, VALUE_ERROR, ZERO_DIVISION_ERROR, get_exception_base, is_exception,
};
