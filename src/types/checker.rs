use super::TypeInfo;
use crate::parser::ast::*;
use std::collections::HashMap;

pub struct TypeChecker {
  type_env: HashMap<String, TypeInfo>,
  scopes: Vec<HashMap<String, TypeInfo>>,
}

impl Default for TypeChecker {
  fn default() -> Self {
    Self::new()
  }
}

impl TypeChecker {
  pub fn new() -> Self {
    let mut checker = Self {
      type_env: HashMap::new(),
      scopes: vec![HashMap::new()],
    };

    // Register built-in types
    checker.register_builtin_functions();

    checker
  }

  fn register_builtin_functions(&mut self) {
    self.type_env.insert(
      "print".to_string(),
      TypeInfo::Function {
        params: vec![TypeInfo::Any],
        return_type: Box::new(TypeInfo::None),
      },
    );

    self.type_env.insert(
      "println".to_string(),
      TypeInfo::Function {
        params: vec![TypeInfo::Any],
        return_type: Box::new(TypeInfo::None),
      },
    );

    self.type_env.insert(
      "len".to_string(),
      TypeInfo::Function {
        params: vec![TypeInfo::Any],
        return_type: Box::new(TypeInfo::Int),
      },
    );
  }

  fn push_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  fn pop_scope(&mut self) {
    if self.scopes.len() > 1 {
      self.scopes.pop();
    }
  }

  fn with_scope<F, T>(&mut self, f: F) -> Result<T, String>
  where
    F: FnOnce(&mut Self) -> Result<T, String>,
  {
    self.push_scope();
    let result = f(self);
    self.pop_scope();
    result
  }

  fn check_block(&mut self, statements: &[Stmt]) -> Result<TypeInfo, String> {
    self.with_scope(|checker| {
      let mut last_type = TypeInfo::None;
      for stmt in statements {
        last_type = checker.check_stmt(stmt)?;
      }
      Ok(last_type)
    })
  }

  fn unify_types(&self, left: TypeInfo, right: TypeInfo) -> Result<TypeInfo, String> {
    if left.is_compatible_with(&right) {
      if matches!(left, TypeInfo::Any) {
        Ok(right)
      } else {
        Ok(left)
      }
    } else {
      Err(format!(
        "Type mismatch: expected {} but found {}",
        left, right
      ))
    }
  }

  fn define_type(&mut self, name: String, type_info: TypeInfo) {
    if let Some(scope) = self.scopes.last_mut() {
      scope.insert(name, type_info);
    }
  }

  fn get_type(&self, name: &str) -> Option<TypeInfo> {
    // Search from innermost to outermost scope
    for scope in self.scopes.iter().rev() {
      if let Some(type_info) = scope.get(name) {
        return Some(type_info.clone());
      }
    }
    // Check global type env
    self.type_env.get(name).cloned()
  }

  pub fn check_program(&mut self, program: &Program) -> Result<(), String> {
    for stmt in &program.statements {
      self.check_stmt(stmt)?;
    }
    Ok(())
  }

  fn check_stmt(&mut self, stmt: &Stmt) -> Result<TypeInfo, String> {
    match stmt {
      Stmt::Let {
        name,
        type_annotation,
        initializer,
        ..
      } => {
        let init_type = self.infer_expr_type(initializer)?;

        if let Some(annotation) = type_annotation {
          let expected_type = TypeInfo::from_ast_type(annotation);
          if !init_type.is_compatible_with(&expected_type) {
            return Err(format!(
              "Type mismatch: variable '{}' declared as {} but initialized with {}",
              name, &expected_type, &init_type
            ));
          }
          self.define_type(name.clone(), expected_type);
        } else {
          self.define_type(name.clone(), init_type.clone());
        }

        Ok(init_type)
      }

      Stmt::Function {
        name,
        params,
        return_type,
        body,
      } => {
        let param_types: Vec<TypeInfo> = params
          .iter()
          .map(|p| {
            if let Some(t) = &p.type_annotation {
              TypeInfo::from_ast_type(t)
            } else {
              TypeInfo::Any
            }
          })
          .collect();

        let return_type_info = if let Some(rt) = return_type {
          TypeInfo::from_ast_type(rt)
        } else {
          TypeInfo::None
        };

        let func_type = TypeInfo::Function {
          params: param_types.clone(),
          return_type: Box::new(return_type_info.clone()),
        };

        self.define_type(name.clone(), func_type);

        // Check function body
        self.push_scope();

        for (param, param_type) in params.iter().zip(param_types.iter()) {
          self.define_type(param.name.clone(), param_type.clone());
        }

        let mut body_type = TypeInfo::None;
        for stmt in body {
          body_type = self.check_stmt(stmt)?;
        }

        // Check return type matches
        if !body_type.is_compatible_with(&return_type_info) {
          return Err(format!(
            "Function '{}' declared to return {} but body returns {}",
            name, &return_type_info, &body_type
          ));
        }

        self.pop_scope();

        Ok(TypeInfo::None)
      }

      Stmt::Return(expr) => {
        if let Some(e) = expr {
          self.infer_expr_type(e)
        } else {
          Ok(TypeInfo::None)
        }
      }

      Stmt::Expr(expr) => self.infer_expr_type(expr),

      Stmt::For { var, iter, body } => {
        let iter_type = self.infer_expr_type(iter)?;

        match iter_type {
          TypeInfo::List(inner) => self.with_scope(|checker| {
            checker.define_type(var.clone(), (*inner).clone());
            for stmt in body {
              checker.check_stmt(stmt)?;
            }
            Ok(TypeInfo::None)
          }),
          other => Err(format!("for loop expects List, got {}", other)),
        }
      }

      Stmt::While { condition, body } => {
        let cond_type = self.infer_expr_type(condition)?;

        if !matches!(cond_type, TypeInfo::Bool | TypeInfo::Any) {
          return Err(format!("while condition must be Bool, got {}", &cond_type));
        }

        self.with_scope(|checker| {
          for stmt in body {
            checker.check_stmt(stmt)?;
          }
          Ok(TypeInfo::None)
        })
      }

      Stmt::TypeDef { name, type_def } => {
        match type_def {
          TypeDefinition::Struct { fields } => {
            let mut field_types = HashMap::new();
            for (field_name, field_type) in fields {
              field_types.insert(field_name.clone(), TypeInfo::from_ast_type(field_type));
            }
            self.type_env.insert(
              name.clone(),
              TypeInfo::Struct {
                name: name.clone(),
                fields: field_types,
              },
            );
          }
          TypeDefinition::Alias(alias) => {
            let alias_type = TypeInfo::from_ast_type(alias);
            self.type_env.insert(name.clone(), alias_type);
          }
          TypeDefinition::Enum { .. } => {
            // Enums are part of the planned type system; keep the name reserved for now
            self
              .type_env
              .insert(name.clone(), TypeInfo::Generic(name.clone()));
          }
        }
        Ok(TypeInfo::None)
      }

      _ => Ok(TypeInfo::None),
    }
  }

  fn infer_expr_type(&mut self, expr: &Expr) -> Result<TypeInfo, String> {
    match expr {
      Expr::Int(_) => Ok(TypeInfo::Int),
      Expr::Float(_) => Ok(TypeInfo::Float),
      Expr::String(_) => Ok(TypeInfo::String),
      Expr::Bool(_) => Ok(TypeInfo::Bool),

      Expr::Ident(name) => self
        .get_type(name)
        .ok_or_else(|| format!("Undefined variable: {}", name)),

      Expr::List(elements) => {
        if elements.is_empty() {
          return Ok(TypeInfo::List(Box::new(TypeInfo::Any)));
        }

        let first_type = self.infer_expr_type(&elements[0])?;

        for elem in &elements[1..] {
          let elem_type = self.infer_expr_type(elem)?;
          if !elem_type.is_compatible_with(&first_type) {
            return Err(format!(
              "List elements must have same type, found {} and {}",
              &first_type, &elem_type
            ));
          }
        }

        Ok(TypeInfo::List(Box::new(first_type)))
      }

      Expr::Map(pairs) => {
        if pairs.is_empty() {
          return Ok(TypeInfo::Map(
            Box::new(TypeInfo::Any),
            Box::new(TypeInfo::Any),
          ));
        }

        let (first_key, first_value) = &pairs[0];
        let key_type = self.infer_expr_type(first_key)?;
        let value_type = self.infer_expr_type(first_value)?;

        for (key_expr, value_expr) in &pairs[1..] {
          let current_key = self.infer_expr_type(key_expr)?;
          let current_value = self.infer_expr_type(value_expr)?;

          if !current_key.is_compatible_with(&key_type) {
            return Err(format!(
              "Map keys must have same type, found {} and {}",
              &key_type, &current_key
            ));
          }

          if !current_value.is_compatible_with(&value_type) {
            return Err(format!(
              "Map values must have same type, found {} and {}",
              &value_type, &current_value
            ));
          }
        }

        Ok(TypeInfo::Map(Box::new(key_type), Box::new(value_type)))
      }

      Expr::Binary { left, op, right } => {
        let left_type = self.infer_expr_type(left)?;
        let right_type = self.infer_expr_type(right)?;

        match op {
          BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
            match (&left_type, &right_type) {
              (TypeInfo::Int, TypeInfo::Int) => Ok(TypeInfo::Int),
              (TypeInfo::Float, TypeInfo::Float) => Ok(TypeInfo::Float),
              (TypeInfo::Int, TypeInfo::Float) | (TypeInfo::Float, TypeInfo::Int) => {
                Ok(TypeInfo::Float)
              }
              (TypeInfo::String, TypeInfo::String) if matches!(op, BinaryOp::Add) => {
                Ok(TypeInfo::String)
              }
              _ => Err(format!(
                "Cannot apply {:?} to {} and {}",
                op, &left_type, &right_type
              )),
            }
          }

          BinaryOp::Equal
          | BinaryOp::NotEqual
          | BinaryOp::Less
          | BinaryOp::Greater
          | BinaryOp::LessEqual
          | BinaryOp::GreaterEqual => Ok(TypeInfo::Bool),

          BinaryOp::And | BinaryOp::Or => Ok(TypeInfo::Bool),

          _ => Ok(TypeInfo::Any),
        }
      }

      Expr::Unary { op, expr } => {
        let expr_type = self.infer_expr_type(expr)?;
        match op {
          UnaryOp::Minus => match expr_type {
            TypeInfo::Int => Ok(TypeInfo::Int),
            TypeInfo::Float => Ok(TypeInfo::Float),
            _ => Err(format!("Cannot negate {}", expr_type)),
          },
          UnaryOp::Not => Ok(TypeInfo::Bool),
        }
      }

      Expr::Call { callee, args } => {
        let func_type = self.infer_expr_type(callee)?;

        match func_type {
          TypeInfo::Function {
            params,
            return_type,
          } => {
            if args.len() != params.len() {
              return Err(format!(
                "Function expects {} arguments, got {}",
                params.len(),
                args.len()
              ));
            }

            for (arg, param_type) in args.iter().zip(params.iter()) {
              let arg_type = self.infer_expr_type(arg)?;
              if !arg_type.is_compatible_with(param_type) {
                return Err(format!(
                  "Argument type mismatch: expected {}, got {}",
                  param_type, arg_type
                ));
              }
            }

            Ok(*return_type)
          }
          _ => Err(format!("Cannot call non-function type: {}", func_type)),
        }
      }

      Expr::If {
        condition,
        then_branch,
        else_branch,
      } => {
        let cond_type = self.infer_expr_type(condition)?;
        if !matches!(cond_type, TypeInfo::Bool | TypeInfo::Any) {
          return Err(format!("if condition must be Bool, got {}", cond_type));
        }

        let then_type = self.check_block(then_branch)?;
        let else_type = if let Some(branch) = else_branch {
          self.check_block(branch)?
        } else {
          TypeInfo::None
        };

        self.unify_types(then_type, else_type)
      }

      Expr::Match { expr, arms } => {
        let match_type = self.infer_expr_type(expr)?;
        if matches!(match_type, TypeInfo::Any) {
          // Allow matches on any type but still type-check arms
        }

        let mut result_type: Option<TypeInfo> = None;

        for arm in arms {
          if let Some(guard) = &arm.guard {
            let guard_type = self.infer_expr_type(guard)?;
            if !matches!(guard_type, TypeInfo::Bool | TypeInfo::Any) {
              return Err(format!("Match guard must be Bool, got {}", guard_type));
            }
          }

          let arm_type = self.check_block(&arm.body)?;

          result_type = Some(match result_type {
            Option::None => arm_type,
            Some(existing) => self.unify_types(existing, arm_type)?,
          });
        }

        Ok(result_type.unwrap_or(TypeInfo::None))
      }

      Expr::Lambda { params, body } => {
        let param_types = vec![TypeInfo::Any; params.len()];
        let return_type = self.with_scope(|checker| {
          for param in params {
            checker.define_type(param.clone(), TypeInfo::Any);
          }
          let mut last = TypeInfo::None;
          for stmt in body {
            last = checker.check_stmt(stmt)?;
          }
          Ok(last)
        })?;

        Ok(TypeInfo::Function {
          params: param_types,
          return_type: Box::new(return_type),
        })
      }

      Expr::MethodCall {
        receiver, method, ..
      } => {
        let receiver_type = self.infer_expr_type(receiver)?;

        match (receiver_type, method.as_str()) {
          (TypeInfo::List(inner), "map") => Ok(TypeInfo::List(Box::new(*inner))),
          (TypeInfo::List(inner), "filter") => Ok(TypeInfo::List(Box::new(*inner))),
          (TypeInfo::List(_), "sum") => Ok(TypeInfo::Int),
          (TypeInfo::List(inner), "first") | (TypeInfo::List(inner), "last") => Ok(*inner),
          (TypeInfo::List(_), "len") => Ok(TypeInfo::Int),
          (TypeInfo::Map(_, _), "len") => Ok(TypeInfo::Int),
          (TypeInfo::String, "len") => Ok(TypeInfo::Int),
          (TypeInfo::String, "trim")
          | (TypeInfo::String, "uppercase")
          | (TypeInfo::String, "lowercase")
          | (TypeInfo::String, "replace")
          | (TypeInfo::List(_), "join") => Ok(TypeInfo::String),
          (TypeInfo::String, "split") => Ok(TypeInfo::List(Box::new(TypeInfo::String))),
          (TypeInfo::String, "contains")
          | (TypeInfo::String, "startsWith")
          | (TypeInfo::String, "endsWith") => Ok(TypeInfo::Bool),
          _ => Ok(TypeInfo::Any),
        }
      }

      Expr::Assign { name, value } => {
        let value_type = self.infer_expr_type(value)?;

        // Check if variable exists and types match
        if let Some(existing_type) = self.get_type(name) {
          if !value_type.is_compatible_with(&existing_type) {
            return Err(format!(
              "Cannot assign {} to variable '{}' of type {}",
              &value_type, name, &existing_type
            ));
          }
        }
        // If variable doesn't exist, allow assignment (will be defined at runtime)

        Ok(value_type)
      }

      Expr::Index { object, index } => {
        let obj_type = self.infer_expr_type(object)?;
        let index_type = self.infer_expr_type(index)?;

        match (obj_type, index_type) {
          (TypeInfo::List(elem_type), TypeInfo::Int) => Ok(*elem_type),
          (TypeInfo::Map(_, value_type), _) => Ok(*value_type),
          (TypeInfo::String, TypeInfo::Int) => Ok(TypeInfo::String),
          _ => Ok(TypeInfo::Any),
        }
      }

      Expr::Range { .. } => Ok(TypeInfo::List(Box::new(TypeInfo::Int))),

      _ => Ok(TypeInfo::Any),
    }
  }
}
