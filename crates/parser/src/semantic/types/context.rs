// Type checking context and core type representation

/// Type identifier for fast equality checks
pub type TypeId = usize;

/// Represents a type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Any type (top type) - accepts all values
    Any,

    /// Never type (bottom type) - no values
    Never,

    /// None type
    None,

    /// Boolean type
    Bool,

    /// Integer type
    Int,

    /// Float type
    Float,

    /// Complex type
    Complex,

    /// String type
    Str,

    /// Bytes type
    Bytes,

    /// List type with element type
    List(Box<Type>),

    /// Tuple type with element types (fixed size)
    Tuple(Vec<Type>),

    /// Set type with element type
    Set(Box<Type>),

    /// Dictionary type with key and value types
    Dict(Box<Type>, Box<Type>),

    /// Function type with parameter types and return type
    Function {
        params: Vec<Type>,
        returns: Box<Type>,
    },

    /// Class type with name
    Class(String),

    /// Instance of a class
    Instance(String),

    /// Union type (multiple possible types)
    Union(Vec<Type>),

    /// Optional type (shorthand for Union[T, None])
    Optional(Box<Type>),

    /// Type variable for generics
    TypeVar { name: String, bounds: Vec<Type> },

    /// Generic type with type parameters
    Generic { base: Box<Type>, params: Vec<Type> },

    /// Module type
    Module(String),

    /// Unknown type (for inference)
    Unknown,
}

impl Type {
    /// Create a new type variable
    pub fn type_var(name: impl Into<String>) -> Self {
        Type::TypeVar {
            name: name.into(),
            bounds: Vec::new(),
        }
    }

    /// Create a new type variable with bounds
    pub fn type_var_with_bounds(name: impl Into<String>, bounds: Vec<Type>) -> Self {
        Type::TypeVar {
            name: name.into(),
            bounds,
        }
    }

    /// Create an optional type
    pub fn optional(ty: Type) -> Self {
        Type::Optional(Box::new(ty))
    }

    /// Create a union type
    pub fn union(types: Vec<Type>) -> Self {
        if types.is_empty() {
            return Type::Never;
        }
        if types.len() == 1 {
            return types.into_iter().next().unwrap();
        }
        Type::Union(types)
    }

    /// Create a function type
    pub fn function(params: Vec<Type>, returns: Type) -> Self {
        Type::Function {
            params,
            returns: Box::new(returns),
        }
    }

    /// Create a list type
    pub fn list(element: Type) -> Self {
        Type::List(Box::new(element))
    }

    /// Create a set type
    pub fn set(element: Type) -> Self {
        Type::Set(Box::new(element))
    }

    /// Create a dict type
    pub fn dict(key: Type, value: Type) -> Self {
        Type::Dict(Box::new(key), Box::new(value))
    }

    /// Create a tuple type
    pub fn tuple(elements: Vec<Type>) -> Self {
        Type::Tuple(elements)
    }

    /// Check if this is a subtype of another type
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            // Any is supertype of everything except Never
            (_, Type::Any) => !matches!(self, Type::Never),

            // Never is subtype of everything
            (Type::Never, _) => true,

            // Unknown is compatible with everything (gradual typing)
            (Type::Unknown, _) | (_, Type::Unknown) => true,

            // Exact match
            (a, b) if a == b => true,

            // Numeric hierarchy: Bool -> Int -> Float -> Complex
            (Type::Bool, Type::Int | Type::Float | Type::Complex) => true,
            (Type::Int, Type::Float | Type::Complex) => true,
            (Type::Float, Type::Complex) => true,

            // Optional[T] is Union[T, None]
            (Type::Optional(t), Type::Union(types)) => {
                types.contains(&Type::None) && types.iter().any(|ty| t.is_subtype_of(ty))
            }

            // T is subtype of Optional[T]
            (t, Type::Optional(opt_t)) => t.is_subtype_of(opt_t) || *t == Type::None,

            // Union subtyping: Union[A, B] <: Union[C, D] if A <: C|D and B <: C|D
            (Type::Union(types1), Type::Union(types2)) => types1
                .iter()
                .all(|t1| types2.iter().any(|t2| t1.is_subtype_of(t2))),

            // T <: Union[A, B] if T <: A or T <: B
            (t, Type::Union(types)) => types.iter().any(|ty| t.is_subtype_of(ty)),

            // List covariance (simplified - lists are actually invariant)
            (Type::List(t1), Type::List(t2)) => t1.is_subtype_of(t2),

            // Set covariance (simplified - sets are actually invariant)
            (Type::Set(t1), Type::Set(t2)) => t1.is_subtype_of(t2),

            // Dict covariance (simplified - dicts are actually invariant)
            (Type::Dict(k1, v1), Type::Dict(k2, v2)) => {
                k1.is_subtype_of(k2) && v1.is_subtype_of(v2)
            }

            // Tuple structural subtyping
            (Type::Tuple(t1), Type::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(a, b)| a.is_subtype_of(b))
            }

            // Function subtyping (contravariant in parameters, covariant in return)
            (
                Type::Function {
                    params: p1,
                    returns: r1,
                },
                Type::Function {
                    params: p2,
                    returns: r2,
                },
            ) => {
                p1.len() == p2.len() &&
                p1.iter().zip(p2.iter()).all(|(a, b)| b.is_subtype_of(a)) && // contravariant
                r1.is_subtype_of(r2) // covariant
            }

            // Instance of class
            (Type::Instance(name1), Type::Class(name2)) => name1 == name2,

            _ => false,
        }
    }

    /// Check if this type is compatible with another (weaker than subtyping)
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        self.is_subtype_of(other) || other.is_subtype_of(self)
    }

    /// Get a simple string representation for error messages
    pub fn display_name(&self) -> String {
        match self {
            Type::Any => "Any".to_string(),
            Type::Never => "Never".to_string(),
            Type::None => "None".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Complex => "complex".to_string(),
            Type::Str => "str".to_string(),
            Type::Bytes => "bytes".to_string(),
            Type::List(t) => format!("list[{}]", t.display_name()),
            Type::Tuple(types) => {
                let names: Vec<_> = types.iter().map(|t| t.display_name()).collect();
                format!("tuple[{}]", names.join(", "))
            }
            Type::Set(t) => format!("set[{}]", t.display_name()),
            Type::Dict(k, v) => format!("dict[{}, {}]", k.display_name(), v.display_name()),
            Type::Function { params, returns } => {
                let param_names: Vec<_> = params.iter().map(|t| t.display_name()).collect();
                format!("({}) -> {}", param_names.join(", "), returns.display_name())
            }
            Type::Class(name) => format!("type[{}]", name),
            Type::Instance(name) => name.clone(),
            Type::Union(types) => {
                let names: Vec<_> = types.iter().map(|t| t.display_name()).collect();
                names.join(" | ")
            }
            Type::Optional(t) => format!("{} | None", t.display_name()),
            Type::TypeVar { name, bounds } => {
                if bounds.is_empty() {
                    name.clone()
                } else {
                    let bound_names: Vec<_> = bounds.iter().map(|t| t.display_name()).collect();
                    format!("{}[{}]", name, bound_names.join(", "))
                }
            }
            Type::Generic { base, params } => {
                let param_names: Vec<_> = params.iter().map(|t| t.display_name()).collect();
                format!("{}[{}]", base.display_name(), param_names.join(", "))
            }
            Type::Module(name) => format!("module[{}]", name),
            Type::Unknown => "?".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_subtypes() {
        assert!(Type::Bool.is_subtype_of(&Type::Int));
        assert!(Type::Int.is_subtype_of(&Type::Float));
        assert!(Type::Float.is_subtype_of(&Type::Complex));
        assert!(Type::Bool.is_subtype_of(&Type::Float)); // transitive
    }

    #[test]
    fn test_any_never() {
        assert!(Type::Int.is_subtype_of(&Type::Any));
        assert!(Type::Never.is_subtype_of(&Type::Int));
        assert!(!Type::Any.is_subtype_of(&Type::Int));
    }

    #[test]
    fn test_optional() {
        let opt_int = Type::optional(Type::Int);
        assert!(Type::Int.is_subtype_of(&opt_int));
        assert!(Type::None.is_subtype_of(&opt_int));
        assert!(!Type::Str.is_subtype_of(&opt_int));
    }

    #[test]
    fn test_union() {
        let union = Type::union(vec![Type::Int, Type::Str]);
        assert!(Type::Int.is_subtype_of(&union));
        assert!(Type::Str.is_subtype_of(&union));
        assert!(Type::Bool.is_subtype_of(&union)); // Bool <: Int, and Int is in union
        assert!(!Type::Float.is_subtype_of(&union)); // Float is not in union
    }

    #[test]
    fn test_display_names() {
        assert_eq!(Type::Int.display_name(), "int");
        assert_eq!(Type::list(Type::Str).display_name(), "list[str]");
        assert_eq!(
            Type::dict(Type::Str, Type::Int).display_name(),
            "dict[str, int]"
        );
        assert_eq!(Type::optional(Type::Int).display_name(), "int | None");
    }
}
