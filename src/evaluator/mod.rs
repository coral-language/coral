pub mod builtins;
pub mod value;

use crate::parser::ast::*;
use builtins::{call_builtin, get_builtin};
use std::collections::HashMap;
use value::{Environment, Value};

pub struct Evaluator {
  pub environment: Environment,
  return_value: Option<Value>,
}

impl Default for Evaluator {
  fn default() -> Self {
    Self::new()
  }
}

impl Evaluator {
  pub fn new() -> Self {
    Self {
      environment: Environment::new(),
      return_value: None,
    }
  }

  pub fn eval_program(&mut self, program: &Program) -> Result<Value, String> {
    let mut last_value = Value::None;

    for stmt in &program.statements {
      last_value = self.eval_stmt(stmt)?;

      // Check for early return
      if self.return_value.is_some() {
        return Ok(self.return_value.take().unwrap());
      }
    }

    Ok(last_value)
  }

  fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Value, String> {
    match stmt {
      Stmt::Expr(expr) => self.eval_expr(expr),

      Stmt::Let {
        name,
        mutable: _,
        type_annotation: _,
        initializer,
      } => {
        let value = self.eval_expr(initializer)?;
        self.environment.define(name.clone(), value.clone());
        Ok(value)
      }

      Stmt::Return(expr) => {
        let value = if let Some(e) = expr {
          self.eval_expr(e)?
        } else {
          Value::None
        };
        self.return_value = Some(value.clone());
        Ok(value)
      }

      Stmt::Function {
        name,
        params,
        return_type: _,
        body,
      } => {
        let param_names: Vec<String> = params.iter().map(|p| p.name.clone()).collect();

        let func = Value::Function {
          params: param_names,
          body: body.clone(),
          closure: self.environment.clone(),
        };

        self.environment.define(name.clone(), func.clone());
        Ok(func)
      }

      Stmt::TypeDef {
        name: _,
        type_def: _,
      } => {
        // For now, just store type definitions as metadata
        // We'll implement full type checking later
        Ok(Value::None)
      }

      Stmt::Export(_) => {
        // For now, exports are no-ops in the interpreter
        Ok(Value::None)
      }

      Stmt::Import { .. } => {
        // For now, imports are no-ops in the interpreter
        Ok(Value::None)
      }

      Stmt::Module(_) => {
        // For now, module declarations are no-ops
        Ok(Value::None)
      }

      Stmt::For { var, iter, body } => {
        let iterable = self.eval_expr(iter)?;

        let items: Vec<Value> = match iterable {
          Value::List(items) => items,
          Value::Range {
            start,
            end,
            inclusive,
          } => {
            // Check for potential overflow or huge ranges
            let range_size = if inclusive {
              end.saturating_sub(start).saturating_add(1)
            } else {
              end.saturating_sub(start)
            };

            const MAX_RANGE_SIZE: i64 = 10_000_000; // 10 million items max
            if !(0..=MAX_RANGE_SIZE).contains(&range_size) {
              return Err(format!(
                "Range too large or invalid: {}..{}{} (size: {})",
                start,
                if inclusive { "=" } else { "" },
                end,
                range_size
              ));
            }

            let mut items = Vec::new();
            if inclusive {
              for i in start..=end {
                items.push(Value::Int(i));
              }
            } else {
              for i in start..end {
                items.push(Value::Int(i));
              }
            }
            items
          }
          _ => {
            return Err(format!(
              "for loop expects List or Range, got {}",
              iterable.type_name()
            ));
          }
        };

        let mut last = Value::None;
        for item in items {
          self.environment.push_scope();
          self.environment.define(var.clone(), item);

          for stmt in body {
            last = self.eval_stmt(stmt)?;
            if self.return_value.is_some() {
              self.environment.pop_scope();
              return Ok(last);
            }
          }

          self.environment.pop_scope();
        }
        Ok(last)
      }

      Stmt::While { condition, body } => {
        let mut last = Value::None;

        loop {
          let cond_value = self.eval_expr(condition)?;
          if !cond_value.is_truthy() {
            break;
          }

          self.environment.push_scope();

          for stmt in body {
            last = self.eval_stmt(stmt)?;
            if self.return_value.is_some() {
              self.environment.pop_scope();
              return Ok(last);
            }
          }

          self.environment.pop_scope();
        }

        Ok(last)
      }
    }
  }

  fn eval_expr(&mut self, expr: &Expr) -> Result<Value, String> {
    match expr {
      Expr::Int(n) => Ok(Value::Int(*n)),
      Expr::Float(f) => Ok(Value::Float(*f)),
      Expr::String(s) => Ok(Value::String(s.clone())),
      Expr::Bool(b) => Ok(Value::Bool(*b)),

      Expr::Ident(name) => {
        // Check builtins first
        if let Some(builtin) = get_builtin(name) {
          return Ok(builtin);
        }

        // Then check environment
        self
          .environment
          .get(name)
          .ok_or_else(|| format!("Undefined variable: {}", name))
      }

      Expr::List(elements) => {
        let mut values = Vec::new();
        for elem in elements {
          values.push(self.eval_expr(elem)?);
        }
        Ok(Value::List(values))
      }

      Expr::Map(pairs) => {
        let mut map = HashMap::new();
        for (key_expr, value_expr) in pairs {
          let key = self.eval_expr(key_expr)?;
          let key_str = match key {
            Value::String(s) => s,
            _ => key.to_string(),
          };
          let value = self.eval_expr(value_expr)?;
          map.insert(key_str, value);
        }
        Ok(Value::Map(map))
      }

      Expr::Binary { left, op, right } => {
        let left_val = self.eval_expr(left)?;
        let right_val = self.eval_expr(right)?;
        self.eval_binary_op(&left_val, op, &right_val)
      }

      Expr::Unary { op, expr } => {
        let value = self.eval_expr(expr)?;
        self.eval_unary_op(op, &value)
      }

      Expr::Call { callee, args } => {
        let func = self.eval_expr(callee)?;
        let mut arg_values = Vec::new();
        for arg in args {
          arg_values.push(self.eval_expr(arg)?);
        }
        self.call_function(func, arg_values)
      }

      Expr::MethodCall {
        receiver,
        method,
        args,
      } => {
        let receiver_val = self.eval_expr(receiver)?;

        // Check if it's a builtin method
        match (&receiver_val, method.as_str()) {
          (Value::List(_), "map") | (Value::List(_), "filter") => {
            if args.len() != 1 {
              return Err(format!("{} expects 1 argument", method));
            }
            let func = self.eval_expr(&args[0])?;
            call_builtin(method, vec![receiver_val, func], self)
          }

          (Value::List(_), "sum") | (Value::List(_), "first") | (Value::List(_), "last") => {
            call_builtin(method, vec![receiver_val], self)
          }

          (Value::List(_), "push") => {
            if args.len() != 1 {
              return Err("push expects 1 argument".to_string());
            }
            let item = self.eval_expr(&args[0])?;
            call_builtin("push", vec![receiver_val, item], self)
          }

          (Value::String(_), "len") | (Value::List(_), "len") | (Value::Map(_), "len") => {
            call_builtin("len", vec![receiver_val], self)
          }

          (Value::String(_), "split") => {
            if args.len() != 1 {
              return Err("split expects 1 argument".to_string());
            }
            let delimiter = self.eval_expr(&args[0])?;
            call_builtin("split", vec![receiver_val, delimiter], self)
          }

          (Value::String(_), "trim") => call_builtin("trim", vec![receiver_val], self),
          (Value::String(_), "uppercase") => call_builtin("uppercase", vec![receiver_val], self),
          (Value::String(_), "lowercase") => call_builtin("lowercase", vec![receiver_val], self),

          (Value::String(_), "contains") => {
            if args.len() != 1 {
              return Err("contains expects 1 argument".to_string());
            }
            let substr = self.eval_expr(&args[0])?;
            call_builtin("contains", vec![receiver_val, substr], self)
          }

          (Value::String(_), "startsWith") => {
            if args.len() != 1 {
              return Err("startsWith expects 1 argument".to_string());
            }
            let prefix = self.eval_expr(&args[0])?;
            call_builtin("startsWith", vec![receiver_val, prefix], self)
          }

          (Value::String(_), "endsWith") => {
            if args.len() != 1 {
              return Err("endsWith expects 1 argument".to_string());
            }
            let suffix = self.eval_expr(&args[0])?;
            call_builtin("endsWith", vec![receiver_val, suffix], self)
          }

          (Value::String(_), "replace") => {
            if args.len() != 2 {
              return Err("replace expects 2 arguments".to_string());
            }
            let from = self.eval_expr(&args[0])?;
            let to = self.eval_expr(&args[1])?;
            call_builtin("replace", vec![receiver_val, from, to], self)
          }

          (Value::List(_), "join") => {
            if args.len() != 1 {
              return Err("join expects 1 argument".to_string());
            }
            let separator = self.eval_expr(&args[0])?;
            call_builtin("join", vec![receiver_val, separator], self)
          }

          (_, "toInt") => call_builtin("toInt", vec![receiver_val], self),
          (_, "toFloat") => call_builtin("toFloat", vec![receiver_val], self),
          (_, "toString") => call_builtin("toString", vec![receiver_val], self),

          // Struct field access (no args)
          (Value::Struct { fields, .. }, _) if args.is_empty() => fields
            .get(method)
            .cloned()
            .ok_or_else(|| format!("Unknown field: {}", method)),

          _ => Err(format!(
            "Unknown method '{}' for type {}",
            method,
            receiver_val.type_name()
          )),
        }
      }

      Expr::If {
        condition,
        then_branch,
        else_branch,
      } => {
        let cond_value = self.eval_expr(condition)?;

        if cond_value.is_truthy() {
          let mut last = Value::None;
          self.environment.push_scope();

          for stmt in then_branch {
            last = self.eval_stmt(stmt)?;
            if self.return_value.is_some() {
              self.environment.pop_scope();
              return Ok(last);
            }
          }

          self.environment.pop_scope();
          Ok(last)
        } else if let Some(else_stmts) = else_branch {
          let mut last = Value::None;
          self.environment.push_scope();

          for stmt in else_stmts {
            last = self.eval_stmt(stmt)?;
            if self.return_value.is_some() {
              self.environment.pop_scope();
              return Ok(last);
            }
          }

          self.environment.pop_scope();
          Ok(last)
        } else {
          Ok(Value::None)
        }
      }

      Expr::Match { expr, arms } => {
        let value = self.eval_expr(expr)?;

        for arm in arms {
          if self.pattern_matches(&arm.pattern, &value)? {
            // Check guard if present
            if let Some(guard) = &arm.guard {
              let guard_value = self.eval_expr(guard)?;
              if !guard_value.is_truthy() {
                continue;
              }
            }

            // Execute arm body
            let mut last = Value::None;
            self.environment.push_scope();

            // Bind pattern variables
            self.bind_pattern(&arm.pattern, &value)?;

            for stmt in &arm.body {
              last = self.eval_stmt(stmt)?;
              if self.return_value.is_some() {
                self.environment.pop_scope();
                return Ok(last);
              }
            }

            self.environment.pop_scope();
            return Ok(last);
          }
        }

        Err(format!("No matching pattern for value: {}", value))
      }

      Expr::Lambda { params, body } => Ok(Value::Function {
        params: params.clone(),
        body: body.clone(),
        closure: self.environment.clone(),
      }),

      Expr::Grouping(expr) => self.eval_expr(expr),

      Expr::StringInterpolation { parts } => {
        let mut result = String::new();
        for part in parts {
          match part {
            StringPart::Text(text) => result.push_str(text),
            StringPart::Expr(expr) => {
              let value = self.eval_expr(expr)?;
              result.push_str(&value.to_string());
            }
          }
        }
        Ok(Value::String(result))
      }

      Expr::Range {
        start,
        end,
        inclusive,
      } => {
        let start_val = self.eval_expr(start)?;
        let end_val = self.eval_expr(end)?;

        match (start_val, end_val) {
          (Value::Int(s), Value::Int(e)) => Ok(Value::Range {
            start: s,
            end: e,
            inclusive: *inclusive,
          }),
          _ => Err("Range must have integer bounds".to_string()),
        }
      }

      Expr::Assign { name, value } => {
        let val = self.eval_expr(value)?;
        self.environment.set(name, val.clone())?;
        Ok(val)
      }

      Expr::Index { object, index } => {
        let obj_val = self.eval_expr(object)?;
        let index_val = self.eval_expr(index)?;

        match (&obj_val, &index_val) {
          (Value::List(items), Value::Int(idx)) => {
            let idx_usize = if *idx < 0 {
              return Err(format!("List index cannot be negative: {}", idx));
            } else {
              *idx as usize
            };

            items.get(idx_usize).cloned().ok_or_else(|| {
              format!(
                "List index out of bounds: {} (length is {})",
                idx,
                items.len()
              )
            })
          }
          (Value::String(s), Value::Int(idx)) => {
            let chars: Vec<char> = s.chars().collect();
            let idx_usize = if *idx < 0 {
              return Err(format!("String index cannot be negative: {}", idx));
            } else {
              *idx as usize
            };

            chars
              .get(idx_usize)
              .map(|c| Value::String(c.to_string()))
              .ok_or_else(|| {
                format!(
                  "String index out of bounds: {} (length is {})",
                  idx,
                  chars.len()
                )
              })
          }
          (Value::Map(map), Value::String(key)) => map
            .get(key)
            .cloned()
            .ok_or_else(|| format!("Key not found: {}", key)),
          _ => Err(format!(
            "Cannot index {} with {}",
            obj_val.type_name(),
            index_val.type_name()
          )),
        }
      }
    }
  }

  fn eval_binary_op(&self, left: &Value, op: &BinaryOp, right: &Value) -> Result<Value, String> {
    match (left, op, right) {
      // Arithmetic
      (Value::Int(a), BinaryOp::Add, Value::Int(b)) => Ok(Value::Int(a + b)),
      (Value::Float(a), BinaryOp::Add, Value::Float(b)) => Ok(Value::Float(a + b)),
      (Value::Int(a), BinaryOp::Add, Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
      (Value::Float(a), BinaryOp::Add, Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
      (Value::String(a), BinaryOp::Add, Value::String(b)) => {
        Ok(Value::String(format!("{}{}", a, b)))
      }

      (Value::Int(a), BinaryOp::Subtract, Value::Int(b)) => Ok(Value::Int(a - b)),
      (Value::Float(a), BinaryOp::Subtract, Value::Float(b)) => Ok(Value::Float(a - b)),

      (Value::Int(a), BinaryOp::Multiply, Value::Int(b)) => Ok(Value::Int(a * b)),
      (Value::Float(a), BinaryOp::Multiply, Value::Float(b)) => Ok(Value::Float(a * b)),

      (Value::Int(a), BinaryOp::Divide, Value::Int(b)) => {
        if *b == 0 {
          Err("Division by zero".to_string())
        } else {
          Ok(Value::Int(a / b))
        }
      }
      (Value::Float(a), BinaryOp::Divide, Value::Float(b)) => {
        if *b == 0.0 {
          Err("Division by zero".to_string())
        } else {
          Ok(Value::Float(a / b))
        }
      }

      (Value::Int(a), BinaryOp::Modulo, Value::Int(b)) => {
        if *b == 0 {
          Err("Modulo by zero".to_string())
        } else {
          Ok(Value::Int(a % b))
        }
      }

      // Comparison
      (Value::Int(a), BinaryOp::Equal, Value::Int(b)) => Ok(Value::Bool(a == b)),
      (Value::Float(a), BinaryOp::Equal, Value::Float(b)) => Ok(Value::Bool(a == b)),
      (Value::String(a), BinaryOp::Equal, Value::String(b)) => Ok(Value::Bool(a == b)),
      (Value::Bool(a), BinaryOp::Equal, Value::Bool(b)) => Ok(Value::Bool(a == b)),

      (Value::Int(a), BinaryOp::NotEqual, Value::Int(b)) => Ok(Value::Bool(a != b)),
      (Value::Float(a), BinaryOp::NotEqual, Value::Float(b)) => Ok(Value::Bool(a != b)),
      (Value::String(a), BinaryOp::NotEqual, Value::String(b)) => Ok(Value::Bool(a != b)),
      (Value::Bool(a), BinaryOp::NotEqual, Value::Bool(b)) => Ok(Value::Bool(a != b)),

      (Value::Int(a), BinaryOp::Less, Value::Int(b)) => Ok(Value::Bool(a < b)),
      (Value::Float(a), BinaryOp::Less, Value::Float(b)) => Ok(Value::Bool(a < b)),

      (Value::Int(a), BinaryOp::LessEqual, Value::Int(b)) => Ok(Value::Bool(a <= b)),
      (Value::Float(a), BinaryOp::LessEqual, Value::Float(b)) => Ok(Value::Bool(a <= b)),

      (Value::Int(a), BinaryOp::Greater, Value::Int(b)) => Ok(Value::Bool(a > b)),
      (Value::Float(a), BinaryOp::Greater, Value::Float(b)) => Ok(Value::Bool(a > b)),

      (Value::Int(a), BinaryOp::GreaterEqual, Value::Int(b)) => Ok(Value::Bool(a >= b)),
      (Value::Float(a), BinaryOp::GreaterEqual, Value::Float(b)) => Ok(Value::Bool(a >= b)),

      // Logical
      (left, BinaryOp::And, right) => {
        if !left.is_truthy() {
          Ok(left.clone())
        } else {
          Ok(right.clone())
        }
      }

      (left, BinaryOp::Or, right) => {
        if left.is_truthy() {
          Ok(left.clone())
        } else {
          Ok(right.clone())
        }
      }

      _ => Err(format!(
        "Invalid binary operation: {} {:?} {}",
        left.type_name(),
        op,
        right.type_name()
      )),
    }
  }

  fn eval_unary_op(&self, op: &UnaryOp, value: &Value) -> Result<Value, String> {
    match (op, value) {
      (UnaryOp::Minus, Value::Int(n)) => Ok(Value::Int(-n)),
      (UnaryOp::Minus, Value::Float(f)) => Ok(Value::Float(-f)),
      (UnaryOp::Not, val) => Ok(Value::Bool(!val.is_truthy())),
      _ => Err(format!(
        "Invalid unary operation: {:?} {}",
        op,
        value.type_name()
      )),
    }
  }

  pub fn call_function(&mut self, func: Value, args: Vec<Value>) -> Result<Value, String> {
    match func {
      Value::BuiltinFunction { name, arity } => {
        if args.len() != arity {
          return Err(format!(
            "Function '{}' expects {} arguments, got {}",
            name,
            arity,
            args.len()
          ));
        }
        call_builtin(&name, args, self)
      }

      Value::Function {
        params,
        body,
        closure,
      } => {
        if args.len() != params.len() {
          return Err(format!(
            "Function expects {} arguments, got {}",
            params.len(),
            args.len()
          ));
        }

        // Save current environment and switch to closure
        let saved_env = self.environment.clone();
        self.environment = closure;

        // Create new scope for function
        self.environment.push_scope();

        // Bind parameters
        for (param, arg) in params.iter().zip(args.iter()) {
          self.environment.define(param.clone(), arg.clone());
        }

        // Execute function body
        let mut result = Value::None;
        for stmt in &body {
          result = self.eval_stmt(stmt)?;

          // Check for return
          if self.return_value.is_some() {
            result = self.return_value.take().unwrap();
            break;
          }
        }

        // Restore environment
        self.environment.pop_scope();
        self.environment = saved_env;

        Ok(result)
      }

      _ => Err(format!(
        "Cannot call non-function value: {}",
        func.type_name()
      )),
    }
  }

  fn pattern_matches(&self, pattern: &Pattern, value: &Value) -> Result<bool, String> {
    match (pattern, value) {
      (Pattern::Wildcard, _) => Ok(true),

      (Pattern::Ident(_), _) => Ok(true), // Binds to anything

      (Pattern::Int(p), Value::Int(v)) => Ok(p == v),
      (Pattern::String(p), Value::String(v)) => Ok(p == v),
      (Pattern::Bool(p), Value::Bool(v)) => Ok(p == v),

      (
        Pattern::Variant { name, args: _ },
        Value::Struct {
          name: struct_name, ..
        },
      ) => {
        if name != struct_name {
          return Ok(false);
        }
        // For now, simple variant matching
        Ok(true)
      }

      _ => Ok(false),
    }
  }

  fn bind_pattern(&mut self, pattern: &Pattern, value: &Value) -> Result<(), String> {
    match pattern {
      Pattern::Wildcard => Ok(()),

      Pattern::Ident(name) => {
        self.environment.define(name.clone(), value.clone());
        Ok(())
      }

      Pattern::Variant { name: _, args: _ } => {
        // For now, simple binding
        Ok(())
      }

      _ => Ok(()),
    }
  }
}
