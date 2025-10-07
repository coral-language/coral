pub mod checker;

use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
  Int,
  Float,
  String,
  Bool,
  List(Box<TypeInfo>),
  Map(Box<TypeInfo>, Box<TypeInfo>),
  Function {
    params: Vec<TypeInfo>,
    return_type: Box<TypeInfo>,
  },
  Option(Box<TypeInfo>),
  Result(Box<TypeInfo>, Box<TypeInfo>),
  Struct {
    name: String,
    fields: HashMap<String, TypeInfo>,
  },
  Generic(String),
  Any,
  None,
}

impl TypeInfo {
  pub fn from_ast_type(ast_type: &crate::parser::ast::Type) -> Self {
    match ast_type {
      crate::parser::ast::Type::Simple(name) => match name.as_str() {
        "Int" => TypeInfo::Int,
        "Float" => TypeInfo::Float,
        "String" => TypeInfo::String,
        "Bool" => TypeInfo::Bool,
        "None" => TypeInfo::None,
        _ => TypeInfo::Generic(name.clone()),
      },
      crate::parser::ast::Type::Generic { name, args } => match name.as_str() {
        "List" => {
          if args.len() == 1 {
            TypeInfo::List(Box::new(TypeInfo::from_ast_type(&args[0])))
          } else {
            TypeInfo::Any
          }
        }
        "Map" => {
          if args.len() == 2 {
            TypeInfo::Map(
              Box::new(TypeInfo::from_ast_type(&args[0])),
              Box::new(TypeInfo::from_ast_type(&args[1])),
            )
          } else {
            TypeInfo::Any
          }
        }
        "Option" => {
          if args.len() == 1 {
            TypeInfo::Option(Box::new(TypeInfo::from_ast_type(&args[0])))
          } else {
            TypeInfo::Any
          }
        }
        "Result" => {
          if args.len() == 2 {
            TypeInfo::Result(
              Box::new(TypeInfo::from_ast_type(&args[0])),
              Box::new(TypeInfo::from_ast_type(&args[1])),
            )
          } else {
            TypeInfo::Any
          }
        }
        _ => TypeInfo::Generic(name.clone()),
      },
      crate::parser::ast::Type::Function {
        params,
        return_type,
      } => TypeInfo::Function {
        params: params.iter().map(TypeInfo::from_ast_type).collect(),
        return_type: Box::new(TypeInfo::from_ast_type(return_type)),
      },
    }
  }

  pub fn is_compatible_with(&self, other: &TypeInfo) -> bool {
    match (self, other) {
      (TypeInfo::Any, _) | (_, TypeInfo::Any) => true,
      (TypeInfo::Int, TypeInfo::Int) => true,
      (TypeInfo::Float, TypeInfo::Float) => true,
      (TypeInfo::String, TypeInfo::String) => true,
      (TypeInfo::Bool, TypeInfo::Bool) => true,
      (TypeInfo::None, TypeInfo::None) => true,
      (TypeInfo::List(a), TypeInfo::List(b)) => a.is_compatible_with(b),
      (TypeInfo::Map(ka, va), TypeInfo::Map(kb, vb)) => {
        ka.is_compatible_with(kb) && va.is_compatible_with(vb)
      }
      (TypeInfo::Option(a), TypeInfo::Option(b)) => a.is_compatible_with(b),
      (TypeInfo::Result(a1, a2), TypeInfo::Result(b1, b2)) => {
        a1.is_compatible_with(b1) && a2.is_compatible_with(b2)
      }
      _ => false,
    }
  }
}

impl fmt::Display for TypeInfo {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeInfo::Int => write!(f, "Int"),
      TypeInfo::Float => write!(f, "Float"),
      TypeInfo::String => write!(f, "String"),
      TypeInfo::Bool => write!(f, "Bool"),
      TypeInfo::List(inner) => write!(f, "List<{}>", inner),
      TypeInfo::Map(key, value) => write!(f, "Map<{}, {}>", key, value),
      TypeInfo::Function {
        params,
        return_type,
      } => {
        let param_str = params
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<_>>()
          .join(", ");
        write!(f, "({}) -> {}", param_str, return_type)
      }
      TypeInfo::Option(inner) => write!(f, "Option<{}>", inner),
      TypeInfo::Result(ok, err) => write!(f, "Result<{}, {}>", ok, err),
      TypeInfo::Struct { name, .. } => write!(f, "{}", name),
      TypeInfo::Generic(name) => write!(f, "{}", name),
      TypeInfo::Any => write!(f, "Any"),
      TypeInfo::None => write!(f, "None"),
    }
  }
}
