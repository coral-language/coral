// Generic types and constraints

use super::context::Type;
use std::collections::HashMap;

/// Context for managing generic type variables and their substitutions
#[derive(Debug, Clone, PartialEq)]
pub struct GenericContext {
    /// Map from type variable names to their concrete types
    substitutions: HashMap<String, Type>,
}

impl GenericContext {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
        }
    }

    /// Bind a type variable to a concrete type
    pub fn bind(&mut self, var_name: String, ty: Type) {
        self.substitutions.insert(var_name, ty);
    }

    /// Lookup a type variable's binding
    pub fn lookup(&self, var_name: &str) -> Option<&Type> {
        self.substitutions.get(var_name)
    }

    /// Apply substitutions to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::TypeVar { name, bounds } => {
                if let Some(concrete_ty) = self.lookup(name) {
                    // Recursively apply in case the substitution contains type vars
                    self.apply(concrete_ty)
                } else {
                    Type::TypeVar {
                        name: name.clone(),
                        bounds: bounds.clone(),
                    }
                }
            }
            Type::List(elem_ty) => Type::list(self.apply(elem_ty)),
            Type::Set(elem_ty) => Type::set(self.apply(elem_ty)),
            Type::Dict(key_ty, val_ty) => Type::dict(self.apply(key_ty), self.apply(val_ty)),
            Type::Tuple(types) => {
                let new_types: Vec<Type> = types.iter().map(|t| self.apply(t)).collect();
                Type::tuple(new_types)
            }
            Type::Optional(inner) => Type::optional(self.apply(inner)),
            Type::Union(types) => {
                let new_types: Vec<Type> = types.iter().map(|t| self.apply(t)).collect();
                Type::union(new_types)
            }
            Type::Function { params, returns } => {
                let new_params: Vec<Type> = params.iter().map(|t| self.apply(t)).collect();
                let new_returns = self.apply(returns);
                Type::function(new_params, new_returns)
            }
            Type::Generic { base, params } => {
                let new_base = self.apply(base);
                let new_params: Vec<Type> = params.iter().map(|t| self.apply(t)).collect();
                Type::Generic {
                    base: Box::new(new_base),
                    params: new_params,
                }
            }
            // All other types are returned as-is
            _ => ty.clone(),
        }
    }

    /// Check if a type satisfies the bounds of a type variable
    pub fn satisfies_bounds(ty: &Type, bounds: &[Type]) -> bool {
        if bounds.is_empty() {
            return true;
        }
        // Type must be a subtype of all bounds
        bounds.iter().all(|bound| ty.is_subtype_of(bound))
    }
}

impl Default for GenericContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_var_substitution() {
        let mut ctx = GenericContext::new();
        ctx.bind("T".to_string(), Type::Int);

        let type_var = Type::type_var("T");
        let result = ctx.apply(&type_var);
        assert_eq!(result, Type::Int);
    }

    #[test]
    fn test_list_substitution() {
        let mut ctx = GenericContext::new();
        ctx.bind("T".to_string(), Type::Str);

        let list_ty = Type::list(Type::type_var("T"));
        let result = ctx.apply(&list_ty);
        assert_eq!(result, Type::list(Type::Str));
    }

    #[test]
    fn test_function_substitution() {
        let mut ctx = GenericContext::new();
        ctx.bind("T".to_string(), Type::Int);
        ctx.bind("U".to_string(), Type::Str);

        let func_ty = Type::function(vec![Type::type_var("T")], Type::type_var("U"));
        let result = ctx.apply(&func_ty);
        assert_eq!(result, Type::function(vec![Type::Int], Type::Str));
    }

    #[test]
    fn test_bounds_checking() {
        let bounds = vec![Type::Int];
        assert!(GenericContext::satisfies_bounds(&Type::Bool, &bounds)); // Bool <: Int
        assert!(!GenericContext::satisfies_bounds(&Type::Str, &bounds));
    }
}
