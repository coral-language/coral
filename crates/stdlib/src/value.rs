use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

pub type ValueMap = HashMap<String, Value>;
pub type ValueList = Vec<Value>;
pub type ValueSet = std::collections::HashSet<String>;

#[derive(Clone)]
pub enum Value {
    None,
    Bool(bool),
    Int(i64),
    Float(f64),
    Complex { real: f64, imag: f64 },
    Str(String),
    Bytes(Vec<u8>),
    List(Rc<RefCell<ValueList>>),
    Tuple(Vec<Value>),
    Dict(Rc<RefCell<ValueMap>>),
    Set(Rc<RefCell<std::collections::HashSet<String>>>),
    FrozenSet(std::collections::HashSet<String>),
    Range { start: i64, stop: i64, step: i64 },
    Function {
        name: String,
        arity: usize,
    },
    BuiltinFunction {
        name: String,
    },
    Instance {
        class_name: String,
        attributes: Rc<RefCell<ValueMap>>,
    },
    Class {
        name: String,
        methods: Rc<RefCell<ValueMap>>,
    },
    Module {
        name: String,
        exports: Rc<RefCell<ValueMap>>,
    },
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Bool(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{}", fl),
            Self::Complex { real, imag } => {
                write!(f, "({}+{}j)", real, imag)
            }
            Self::Str(s) => write!(f, "'{}'", s),
            Self::Bytes(b) => write!(f, "b'{:?}'", String::from_utf8_lossy(b)),
            Self::List(_) => write!(f, "[...]"),
            Self::Tuple(_) => write!(f, "(...)"),
            Self::Dict(_) => write!(f, "{{...}}"),
            Self::Set(_) => write!(f, "{{...}}"),
            Self::FrozenSet(_) => write!(f, "frozenset({{...}})"),
            Self::Range { start, stop, step } => {
                write!(f, "range({}, {}, {})", start, stop, step)
            }
            Self::Function { name, arity } => {
                write!(f, "<function {} ({} args)>", name, arity)
            }
            Self::BuiltinFunction { name } => {
                write!(f, "<built-in function {}>", name)
            }
            Self::Instance { class_name, .. } => {
                write!(f, "<{} instance>", class_name)
            }
            Self::Class { name, .. } => {
                write!(f, "<class '{}'>", name)
            }
            Self::Module { name, .. } => {
                write!(f, "<module '{}'>", name)
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::Bool(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(fl) => {
                let s = format!("{}", fl);
                if s.contains('.') {
                    write!(f, "{}", s)
                } else {
                    write!(f, "{}.0", s)
                }
            }
            Self::Complex { real, imag } => {
                write!(f, "({}+{}j)", real, imag)
            }
            Self::Str(s) => write!(f, "{}", s),
            Self::Bytes(b) => write!(f, "{:?}", String::from_utf8_lossy(b)),
            Self::List(list) => {
                let items = list.borrow();
                let formatted: Vec<String> = items.iter()
                    .map(|v| format!("{}", v))
                    .collect();
                write!(f, "[{}]", formatted.join(", "))
            }
            Self::Tuple(items) => {
                let formatted: Vec<String> = items.iter()
                    .map(|v| format!("{}", v))
                    .collect();
                if items.len() == 1 {
                    write!(f, "({},)", formatted[0])
                } else {
                    write!(f, "({})", formatted.join(", "))
                }
            }
            Self::Dict(dict) => {
                let d = dict.borrow();
                let formatted: Vec<String> = d.iter()
                    .map(|(k, v)| format!("'{}': {}", k, v))
                    .collect();
                write!(f, "{{{}}}", formatted.join(", "))
            }
            Self::Set(set) => {
                let s = set.borrow();
                let formatted: Vec<String> = s.iter()
                    .map(|v| format!("'{}'", v))
                    .collect();
                write!(f, "{{{}}}", formatted.join(", "))
            }
            Self::FrozenSet(set) => {
                let formatted: Vec<String> = set.iter()
                    .map(|v| format!("'{}'", v))
                    .collect();
                write!(f, "frozenset({{{}}})", formatted.join(", "))
            }
            Self::Range { start, stop, step } => {
                write!(f, "range({}, {}, {})", start, stop, step)
            }
            Self::Function { name, .. } => write!(f, "<function {}>", name),
            Self::BuiltinFunction { name } => write!(f, "<built-in function {}>", name),
            Self::Instance { class_name, .. } => write!(f, "<{} instance>", class_name),
            Self::Class { name, .. } => write!(f, "<class '{}'>", name),
            Self::Module { name, .. } => write!(f, "<module '{}'>", name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::None, Value::None) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Int(a), Value::Float(b)) => *a as f64 == *b,
            (Value::Float(a), Value::Int(b)) => *a == *b as f64,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::List(a), Value::List(b)) => {
                let a_items = a.borrow();
                let b_items = b.borrow();
                a_items.len() == b_items.len()
                    && a_items.iter().zip(b_items.iter()).all(|(x, y)| x == y)
            }
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Dict(a), Value::Dict(b)) => {
                let a_dict = a.borrow();
                let b_dict = b.borrow();
                a_dict.len() == b_dict.len()
                    && a_dict.iter().all(|(k, v)| b_dict.get(k) == Some(v))
            }
            _ => false,
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &str {
        match self {
            Value::None => "NoneType",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::Complex { .. } => "complex",
            Value::Str(_) => "str",
            Value::Bytes(_) => "bytes",
            Value::List(_) => "list",
            Value::Tuple(_) => "tuple",
            Value::Dict(_) => "dict",
            Value::Set(_) => "set",
            Value::FrozenSet(_) => "frozenset",
            Value::Range { .. } => "range",
            Value::Function { .. } => "function",
            Value::BuiltinFunction { .. } => "builtin_function_or_method",
            Value::Instance { class_name, .. } => class_name,
            Value::Class { name, .. } => name,
            Value::Module { .. } => "module",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::None => false,
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0 && !f.is_nan(),
            Value::Str(s) => !s.is_empty(),
            Value::Bytes(b) => !b.is_empty(),
            Value::List(l) => !l.borrow().is_empty(),
            Value::Tuple(t) => !t.is_empty(),
            Value::Dict(d) => !d.borrow().is_empty(),
            Value::Set(s) => !s.borrow().is_empty(),
            Value::FrozenSet(s) => !s.is_empty(),
            Value::Range { start, stop, step } => {
                if *step == 0 {
                    false
                } else if *step > 0 {
                    start < stop
                } else {
                    start > stop
                }
            }
            _ => true,
        }
    }
}
