use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Int(i64),
  Float(f64),
  String(String),
  Bool(bool),
  List(Vec<Value>),
  Map(HashMap<String, Value>),
  Function {
    params: Vec<String>,
    body: Vec<crate::parser::ast::Stmt>,
    closure: Environment,
  },
  BuiltinFunction {
    name: String,
    arity: usize,
  },
  #[allow(dead_code)]
  Struct {
    name: String,
    fields: HashMap<String, Value>,
  },
  Option(Option<Box<Value>>),
  Result(Result<Box<Value>, Box<Value>>),
  Range {
    start: i64,
    end: i64,
    inclusive: bool,
  },
  None,
}

impl fmt::Display for Value {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Value::Int(n) => write!(f, "{}", n),
      Value::Float(n) => write!(f, "{}", n),
      Value::String(s) => write!(f, "{}", s),
      Value::Bool(b) => write!(f, "{}", b),
      Value::List(items) => {
        write!(f, "[")?;
        for (i, item) in items.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}", item)?;
        }
        write!(f, "]")
      }
      Value::Map(map) => {
        write!(f, "{{")?;
        for (i, (k, v)) in map.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}: {}", k, v)?;
        }
        write!(f, "}}")
      }
      Value::Function { params, .. } => {
        write!(f, "<function({})>", params.join(", "))
      }
      Value::BuiltinFunction { name, .. } => write!(f, "<builtin {}>", name),
      Value::Struct { name, fields } => {
        write!(f, "{} {{ ", name)?;
        for (i, (k, v)) in fields.iter().enumerate() {
          if i > 0 {
            write!(f, ", ")?;
          }
          write!(f, "{}: {}", k, v)?;
        }
        write!(f, " }}")
      }
      Value::Option(opt) => match opt {
        Some(v) => write!(f, "Some({})", v),
        Option::None => write!(f, "None"),
      },
      Value::Result(res) => match res {
        Ok(v) => write!(f, "Ok({})", v),
        Err(e) => write!(f, "Error({})", e),
      },
      Value::Range {
        start,
        end,
        inclusive,
      } => {
        if *inclusive {
          write!(f, "{}..={}", start, end)
        } else {
          write!(f, "{}..{}", start, end)
        }
      }
      Value::None => write!(f, "none"),
    }
  }
}

impl Value {
  pub fn is_truthy(&self) -> bool {
    match self {
      Value::Bool(b) => *b,
      Value::None => false,
      Value::Int(0) => false,
      Value::Float(f) if *f == 0.0 => false,
      Value::String(s) if s.is_empty() => false,
      Value::List(l) if l.is_empty() => false,
      Value::Option(Option::None) => false,
      _ => true,
    }
  }

  pub fn type_name(&self) -> &str {
    match self {
      Value::Int(_) => "Int",
      Value::Float(_) => "Float",
      Value::String(_) => "String",
      Value::Bool(_) => "Bool",
      Value::List(_) => "List",
      Value::Map(_) => "Map",
      Value::Function { .. } => "Function",
      Value::BuiltinFunction { .. } => "BuiltinFunction",
      Value::Struct { name, .. } => name,
      Value::Option(_) => "Option",
      Value::Result(_) => "Result",
      Value::Range { .. } => "Range",
      Value::None => "None",
    }
  }
}

// Environment for variable scoping
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
  scopes: Vec<HashMap<String, Value>>,
}

impl Default for Environment {
  fn default() -> Self {
    Self::new()
  }
}

impl Environment {
  pub fn new() -> Self {
    Self {
      scopes: vec![HashMap::new()],
    }
  }

  pub fn push_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn pop_scope(&mut self) {
    if self.scopes.len() > 1 {
      self.scopes.pop();
    }
  }

  pub fn define(&mut self, name: String, value: Value) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name, value);
    }
  }

  pub fn get(&self, name: &str) -> Option<Value> {
    // Search from innermost to outermost scope
    for scope in self.scopes.iter().rev() {
      if let Some(value) = scope.get(name) {
        return Some(value.clone());
      }
    }
    None
  }

  pub fn set(&mut self, name: &str, value: Value) -> Result<(), String> {
    // Search from innermost to outermost scope for existing variable
    for scope in self.scopes.iter_mut().rev() {
      if scope.contains_key(name) {
        scope.insert(name.to_string(), value);
        return Ok(());
      }
    }

    // Variable doesn't exist anywhere - create it in the CURRENT (innermost) scope
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name.to_string(), value);
      Ok(())
    } else {
      Err("No scope available".to_string())
    }
  }
}
