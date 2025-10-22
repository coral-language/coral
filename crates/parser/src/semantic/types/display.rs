// Type display and formatting

use super::context::Type;
use std::fmt;

/// Display trait implementation for types (already in context.rs via display_name())
/// This module provides additional formatting utilities
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

/// Pretty print a type with detailed information
pub fn pretty_print_type(ty: &Type, indent: usize) -> String {
    let prefix = "  ".repeat(indent);
    match ty {
        Type::Function {
            params,
            returns,
            captures: _,
        } => {
            let mut result = format!("{}Function:\n", prefix);
            result.push_str(&format!("{}  Parameters:\n", prefix));
            for (i, param) in params.iter().enumerate() {
                result.push_str(&format!(
                    "{}    [{}]: {}\n",
                    prefix,
                    i,
                    pretty_print_type(param, indent + 2)
                ));
            }
            result.push_str(&format!(
                "{}  Returns: {}\n",
                prefix,
                pretty_print_type(returns, indent + 1)
            ));
            result
        }
        Type::Union(types) => {
            let mut result = format!("{}Union:\n", prefix);
            for ty in types {
                result.push_str(&format!(
                    "{}{}\n",
                    prefix,
                    pretty_print_type(ty, indent + 1)
                ));
            }
            result
        }
        Type::Generic { base, params } => {
            let mut result = format!("{}Generic {}:\n", prefix, base.display_name());
            for param in params {
                result.push_str(&format!(
                    "{}{}\n",
                    prefix,
                    pretty_print_type(param, indent + 1)
                ));
            }
            result
        }
        _ => format!("{}{}", prefix, ty.display_name()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(Type::Int.to_string(), "int");
        assert_eq!(Type::list(Type::Str).to_string(), "list[str]");
        assert_eq!(
            Type::function(vec![Type::Int, Type::Str], Type::Bool).to_string(),
            "(int, str) -> bool"
        );
    }

    #[test]
    fn test_pretty_print() {
        let func_ty = Type::function(vec![Type::Int], Type::Str);
        let output = pretty_print_type(&func_ty, 0);
        assert!(output.contains("Function"));
        assert!(output.contains("Parameters"));
        assert!(output.contains("Returns"));
    }
}
