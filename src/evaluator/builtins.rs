use super::value::Value;

pub fn get_builtin(name: &str) -> Option<Value> {
  match name {
    "print" => Some(Value::BuiltinFunction {
      name: "print".to_string(),
      arity: 1,
    }),
    "println" => Some(Value::BuiltinFunction {
      name: "println".to_string(),
      arity: 1,
    }),
    "len" => Some(Value::BuiltinFunction {
      name: "len".to_string(),
      arity: 1,
    }),
    "push" => Some(Value::BuiltinFunction {
      name: "push".to_string(),
      arity: 2,
    }),
    "pop" => Some(Value::BuiltinFunction {
      name: "pop".to_string(),
      arity: 1,
    }),
    "first" => Some(Value::BuiltinFunction {
      name: "first".to_string(),
      arity: 1,
    }),
    "last" => Some(Value::BuiltinFunction {
      name: "last".to_string(),
      arity: 1,
    }),
    "map" => Some(Value::BuiltinFunction {
      name: "map".to_string(),
      arity: 2,
    }),
    "filter" => Some(Value::BuiltinFunction {
      name: "filter".to_string(),
      arity: 2,
    }),
    "sum" => Some(Value::BuiltinFunction {
      name: "sum".to_string(),
      arity: 1,
    }),
    "toInt" => Some(Value::BuiltinFunction {
      name: "toInt".to_string(),
      arity: 1,
    }),
    "toFloat" => Some(Value::BuiltinFunction {
      name: "toFloat".to_string(),
      arity: 1,
    }),
    "toString" => Some(Value::BuiltinFunction {
      name: "toString".to_string(),
      arity: 1,
    }),
    "Some" => Some(Value::BuiltinFunction {
      name: "Some".to_string(),
      arity: 1,
    }),
    "None" => Some(Value::Option(None)),
    "Ok" => Some(Value::BuiltinFunction {
      name: "Ok".to_string(),
      arity: 1,
    }),
    "Error" => Some(Value::BuiltinFunction {
      name: "Error".to_string(),
      arity: 1,
    }),
    "split" => Some(Value::BuiltinFunction {
      name: "split".to_string(),
      arity: 2,
    }),
    "join" => Some(Value::BuiltinFunction {
      name: "join".to_string(),
      arity: 2,
    }),
    "trim" => Some(Value::BuiltinFunction {
      name: "trim".to_string(),
      arity: 1,
    }),
    "uppercase" => Some(Value::BuiltinFunction {
      name: "uppercase".to_string(),
      arity: 1,
    }),
    "lowercase" => Some(Value::BuiltinFunction {
      name: "lowercase".to_string(),
      arity: 1,
    }),
    "contains" => Some(Value::BuiltinFunction {
      name: "contains".to_string(),
      arity: 2,
    }),
    "startsWith" => Some(Value::BuiltinFunction {
      name: "startsWith".to_string(),
      arity: 2,
    }),
    "endsWith" => Some(Value::BuiltinFunction {
      name: "endsWith".to_string(),
      arity: 2,
    }),
    "replace" => Some(Value::BuiltinFunction {
      name: "replace".to_string(),
      arity: 3,
    }),
    _ => None,
  }
}

pub fn call_builtin(
  name: &str,
  args: Vec<Value>,
  evaluator: &mut super::Evaluator,
) -> Result<Value, String> {
  match name {
    "print" => {
      if args.len() != 1 {
        return Err(format!("print expects 1 argument, got {}", args.len()));
      }
      print!("{}", args[0]);
      Ok(Value::None)
    }

    "println" => {
      if args.len() != 1 {
        return Err(format!("println expects 1 argument, got {}", args.len()));
      }
      println!("{}", args[0]);
      Ok(Value::None)
    }

    "len" => {
      if args.len() != 1 {
        return Err(format!("len expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        Value::List(l) => Ok(Value::Int(l.len() as i64)),
        Value::Map(m) => Ok(Value::Int(m.len() as i64)),
        _ => Err("len expects String, List, or Map".to_string()),
      }
    }

    "push" => {
      if args.len() != 2 {
        return Err(format!("push expects 2 arguments, got {}", args.len()));
      }
      match &args[0] {
        Value::List(list) => {
          let mut new_list = list.clone();
          new_list.push(args[1].clone());
          Ok(Value::List(new_list))
        }
        _ => Err("push expects a List as first argument".to_string()),
      }
    }

    "pop" => {
      if args.len() != 1 {
        return Err(format!("pop expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::List(list) => {
          if list.is_empty() {
            return Ok(Value::None);
          }
          let mut new_list = list.clone();
          new_list.pop();
          Ok(Value::List(new_list))
        }
        _ => Err("pop expects a List".to_string()),
      }
    }

    "first" => {
      if args.len() != 1 {
        return Err(format!("first expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::List(list) => {
          if list.is_empty() {
            Ok(Value::None)
          } else {
            Ok(list[0].clone())
          }
        }
        _ => Err("first expects a List".to_string()),
      }
    }

    "last" => {
      if args.len() != 1 {
        return Err(format!("last expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::List(list) => {
          if list.is_empty() {
            Ok(Value::None)
          } else {
            Ok(list[list.len() - 1].clone())
          }
        }
        _ => Err("last expects a List".to_string()),
      }
    }

    "map" => {
      if args.len() != 2 {
        return Err(format!("map expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::List(list), func) => {
          let mut result = Vec::new();
          for item in list {
            let value = evaluator.call_function(func.clone(), vec![item.clone()])?;
            result.push(value);
          }
          Ok(Value::List(result))
        }
        _ => Err("map expects (List, Function)".to_string()),
      }
    }

    "filter" => {
      if args.len() != 2 {
        return Err(format!("filter expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::List(list), func) => {
          let mut result = Vec::new();
          for item in list {
            let value = evaluator.call_function(func.clone(), vec![item.clone()])?;
            if value.is_truthy() {
              result.push(item.clone());
            }
          }
          Ok(Value::List(result))
        }
        _ => Err("filter expects (List, Function)".to_string()),
      }
    }

    "sum" => {
      if args.len() != 1 {
        return Err(format!("sum expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::List(list) => {
          let mut sum = 0i64;
          for item in list {
            match item {
              Value::Int(n) => sum += n,
              _ => return Err("sum expects a List of integers".to_string()),
            }
          }
          Ok(Value::Int(sum))
        }
        _ => Err("sum expects a List".to_string()),
      }
    }

    "toInt" => {
      if args.len() != 1 {
        return Err(format!("toInt expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => s
          .parse::<i64>()
          .map(Value::Int)
          .map_err(|_| format!("Cannot convert '{}' to Int", s)),
        Value::Float(f) => Ok(Value::Int(*f as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        _ => Err(format!("Cannot convert {} to Int", args[0].type_name())),
      }
    }

    "toFloat" => {
      if args.len() != 1 {
        return Err(format!("toFloat expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => s
          .parse::<f64>()
          .map(Value::Float)
          .map_err(|_| format!("Cannot convert '{}' to Float", s)),
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(f) => Ok(Value::Float(*f)),
        _ => Err(format!("Cannot convert {} to Float", args[0].type_name())),
      }
    }

    "toString" => {
      if args.len() != 1 {
        return Err(format!("toString expects 1 argument, got {}", args.len()));
      }
      Ok(Value::String(args[0].to_string()))
    }

    "Some" => {
      if args.len() != 1 {
        return Err(format!("Some expects 1 argument, got {}", args.len()));
      }
      Ok(Value::Option(Some(Box::new(args[0].clone()))))
    }

    "Ok" => {
      if args.len() != 1 {
        return Err(format!("Ok expects 1 argument, got {}", args.len()));
      }
      Ok(Value::Result(Ok(Box::new(args[0].clone()))))
    }

    "Error" => {
      if args.len() != 1 {
        return Err(format!("Error expects 1 argument, got {}", args.len()));
      }
      Ok(Value::Result(Err(Box::new(args[0].clone()))))
    }

    "split" => {
      if args.len() != 2 {
        return Err(format!("split expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::String(s), Value::String(delimiter)) => {
          let parts: Vec<Value> = s
            .split(delimiter.as_str())
            .map(|part| Value::String(part.to_string()))
            .collect();
          Ok(Value::List(parts))
        }
        _ => Err("split expects (String, String)".to_string()),
      }
    }

    "join" => {
      if args.len() != 2 {
        return Err(format!("join expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::List(items), Value::String(separator)) => {
          let strings: Result<Vec<String>, String> = items
            .iter()
            .map(|v| match v {
              Value::String(s) => Ok(s.clone()),
              _ => Err("join expects List of Strings".to_string()),
            })
            .collect();
          match strings {
            Ok(strs) => Ok(Value::String(strs.join(separator))),
            Err(e) => Err(e),
          }
        }
        _ => Err("join expects (List<String>, String)".to_string()),
      }
    }

    "trim" => {
      if args.len() != 1 {
        return Err(format!("trim expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => Ok(Value::String(s.trim().to_string())),
        _ => Err("trim expects String".to_string()),
      }
    }

    "uppercase" => {
      if args.len() != 1 {
        return Err(format!("uppercase expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_uppercase())),
        _ => Err("uppercase expects String".to_string()),
      }
    }

    "lowercase" => {
      if args.len() != 1 {
        return Err(format!("lowercase expects 1 argument, got {}", args.len()));
      }
      match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_lowercase())),
        _ => Err("lowercase expects String".to_string()),
      }
    }

    "contains" => {
      if args.len() != 2 {
        return Err(format!("contains expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::String(s), Value::String(substr)) => Ok(Value::Bool(s.contains(substr.as_str()))),
        _ => Err("contains expects (String, String)".to_string()),
      }
    }

    "startsWith" => {
      if args.len() != 2 {
        return Err(format!(
          "startsWith expects 2 arguments, got {}",
          args.len()
        ));
      }
      match (&args[0], &args[1]) {
        (Value::String(s), Value::String(prefix)) => {
          Ok(Value::Bool(s.starts_with(prefix.as_str())))
        }
        _ => Err("startsWith expects (String, String)".to_string()),
      }
    }

    "endsWith" => {
      if args.len() != 2 {
        return Err(format!("endsWith expects 2 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1]) {
        (Value::String(s), Value::String(suffix)) => Ok(Value::Bool(s.ends_with(suffix.as_str()))),
        _ => Err("endsWith expects (String, String)".to_string()),
      }
    }

    "replace" => {
      if args.len() != 3 {
        return Err(format!("replace expects 3 arguments, got {}", args.len()));
      }
      match (&args[0], &args[1], &args[2]) {
        (Value::String(s), Value::String(from), Value::String(to)) => {
          Ok(Value::String(s.replace(from.as_str(), to.as_str())))
        }
        _ => Err("replace expects (String, String, String)".to_string()),
      }
    }

    _ => Err(format!("Unknown builtin function: {}", name)),
  }
}
