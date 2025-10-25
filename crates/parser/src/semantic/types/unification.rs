use super::context::Type;
use super::generics::GenericContext;

/// Result of type unification
#[derive(Debug, Clone, PartialEq)]
pub enum UnificationResult {
    /// Types unified successfully
    Success(GenericContext),
    /// Types cannot be unified
    Failure(String),
}

/// Unify two types, finding substitutions for type variables
pub fn unify(ty1: &Type, ty2: &Type) -> UnificationResult {
    let mut ctx = GenericContext::new();
    if unify_with_context(ty1, ty2, &mut ctx) {
        UnificationResult::Success(ctx)
    } else {
        UnificationResult::Failure(format!(
            "Cannot unify {} with {}",
            ty1.display_name(),
            ty2.display_name()
        ))
    }
}

/// Internal unification with context
fn unify_with_context(ty1: &Type, ty2: &Type, ctx: &mut GenericContext) -> bool {
    let ty1 = ctx.apply(ty1);
    let ty2 = ctx.apply(ty2);

    match (&ty1, &ty2) {
        (a, b) if a == b => true,

        (Type::Unknown, _) | (_, Type::Unknown) => true,

        (
            Type::TypeVar {
                name,
                bounds,
                variance: _,
            },
            ty,
        )
        | (
            ty,
            Type::TypeVar {
                name,
                bounds,
                variance: _,
            },
        ) => {
            if !GenericContext::satisfies_bounds(ty, bounds) {
                return false;
            }

            if occurs_check(name, ty) {
                return false;
            }
            ctx.bind(name.clone(), ty.clone());
            true
        }

        (Type::List(t1), Type::List(t2)) => unify_with_context(t1, t2, ctx),

        (Type::Set(t1), Type::Set(t2)) => unify_with_context(t1, t2, ctx),

        (Type::Dict(k1, v1), Type::Dict(k2, v2)) => {
            unify_with_context(k1, k2, ctx) && unify_with_context(v1, v2, ctx)
        }

        (Type::Tuple(types1), Type::Tuple(types2)) => {
            if types1.len() != types2.len() {
                return false;
            }
            types1
                .iter()
                .zip(types2.iter())
                .all(|(t1, t2)| unify_with_context(t1, t2, ctx))
        }

        (Type::Optional(t1), Type::Optional(t2)) => unify_with_context(t1, t2, ctx),

        (
            Type::Function {
                params: p1,
                returns: r1,
                captures: _,
            },
            Type::Function {
                params: p2,
                returns: r2,
                captures: _,
            },
        ) => {
            if p1.len() != p2.len() {
                return false;
            }
            let params_unify = p1
                .iter()
                .zip(p2.iter())
                .all(|((_n1, t1), (_n2, t2))| unify_with_context(t1, t2, ctx));
            params_unify && unify_with_context(r1, r2, ctx)
        }

        (t1, t2) if t1.is_subtype_of(t2) || t2.is_subtype_of(t1) => true,

        _ => false,
    }
}

/// Check if a type variable occurs in a type (prevents infinite types)
fn occurs_check(var_name: &str, ty: &Type) -> bool {
    match ty {
        Type::TypeVar { name, .. } => name == var_name,
        Type::List(t) | Type::Set(t) | Type::Optional(t) => occurs_check(var_name, t),
        Type::Dict(k, v) => occurs_check(var_name, k) || occurs_check(var_name, v),
        Type::Tuple(types) | Type::Union(types) => types.iter().any(|t| occurs_check(var_name, t)),
        Type::Function {
            params,
            returns,
            captures: _,
        } => {
            params.iter().any(|(_name, t)| occurs_check(var_name, t))
                || occurs_check(var_name, returns)
        }
        Type::Generic { base, params } => {
            occurs_check(var_name, base) || params.iter().any(|t| occurs_check(var_name, t))
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unify_identical() {
        let result = unify(&Type::Int, &Type::Int);
        assert!(matches!(result, UnificationResult::Success(_)));
    }

    #[test]
    fn test_unify_type_var() {
        let ty1 = Type::type_var("T");
        let ty2 = Type::Int;

        if let UnificationResult::Success(ctx) = unify(&ty1, &ty2) {
            assert_eq!(ctx.lookup("T"), Some(&Type::Int));
        } else {
            panic!("Unification should succeed");
        }
    }

    #[test]
    fn test_unify_list() {
        let ty1 = Type::list(Type::type_var("T"));
        let ty2 = Type::list(Type::Str);

        if let UnificationResult::Success(ctx) = unify(&ty1, &ty2) {
            assert_eq!(ctx.lookup("T"), Some(&Type::Str));
        } else {
            panic!("Unification should succeed");
        }
    }

    #[test]
    fn test_unify_function() {
        let ty1 = Type::function(vec![Type::type_var("T")], Type::type_var("U"));
        let ty2 = Type::function(vec![Type::Int], Type::Str);

        if let UnificationResult::Success(ctx) = unify(&ty1, &ty2) {
            assert_eq!(ctx.lookup("T"), Some(&Type::Int));
            assert_eq!(ctx.lookup("U"), Some(&Type::Str));
        } else {
            panic!("Unification should succeed");
        }
    }

    #[test]
    fn test_unify_failure() {
        let result = unify(&Type::Int, &Type::Str);
        assert!(matches!(result, UnificationResult::Failure(_)));
    }

    #[test]
    fn test_occurs_check() {
        assert!(occurs_check("T", &Type::type_var("T")));
        assert!(occurs_check("T", &Type::list(Type::type_var("T"))));
        assert!(!occurs_check("T", &Type::Int));
    }
}
