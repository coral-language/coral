

/// Type identifier for fast equality checks
pub type TypeId = usize;

/// Variance of type parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variance {
    /// Covariant (+T): preserves subtyping order
    /// If A <: B then Container[A] <: Container[B]
    /// Used for immutable containers and return types
    Covariant,

    /// Contravariant (-T): reverses subtyping order
    /// If A <: B then Container[B] <: Container[A]
    /// Used for function parameters
    Contravariant,

    /// Invariant (T): requires exact type match
    /// Container[A] <: Container[B] only if A == B
    /// Used for mutable containers
    Invariant,
}

/// Kind of attribute descriptor
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AttributeKind {
    /// Instance attribute
    InstanceAttribute,
    /// Class attribute
    ClassAttribute,
    /// Property descriptor
    Property,
    /// Static method
    StaticMethod,
    /// Class method
    ClassMethod,
    /// Instance method
    InstanceMethod,
}

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

    /// Function type with parameter names and types
    Function {
        /// Parameters as (optional_name, type) tuples
        /// None for anonymous parameters (e.g., from expressions)
        params: Vec<(Option<String>, Type)>,
        returns: Box<Type>,
        /// Captured variables from enclosing scopes (for closures)
        captures: Vec<(String, Type)>,
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
    TypeVar {
        name: String,
        bounds: Vec<Type>,
        variance: Variance,
    },

    /// Generic type with type parameters
    Generic { base: Box<Type>, params: Vec<Type> },

    /// Module type
    Module(String),

    /// Generator type with yielded element type
    Generator(Box<Type>),

    /// Coroutine type (returned by async functions)
    /// Represents an awaitable future that yields a value of the inner type
    Coroutine(Box<Type>),

    /// Slice type
    Slice,

    /// Template string type (t-strings)
    TemplateString,

    /// Attribute descriptor type (property, static method, class method)
    AttributeDescriptor {
        kind: AttributeKind,
        getter_type: Box<Type>,
        setter_type: Option<Box<Type>>,
    },

    /// Unknown type (for inference)
    Unknown,
}

impl Type {
    /// Create a new invariant type variable (default)
    pub fn type_var(name: impl Into<String>) -> Self {
        Type::TypeVar {
            name: name.into(),
            bounds: Vec::new(),
            variance: Variance::Invariant,
        }
    }

    /// Create a new type variable with bounds and default invariance
    pub fn type_var_with_bounds(name: impl Into<String>, bounds: Vec<Type>) -> Self {
        Type::TypeVar {
            name: name.into(),
            bounds,
            variance: Variance::Invariant,
        }
    }

    /// Create a new type variable with variance and bounds
    pub fn type_var_with_variance(
        name: impl Into<String>,
        bounds: Vec<Type>,
        variance: Variance,
    ) -> Self {
        Type::TypeVar {
            name: name.into(),
            bounds,
            variance,
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

    /// Create a function type without parameter names
    pub fn function(params: Vec<Type>, returns: Type) -> Self {
        Type::Function {
            params: params.into_iter().map(|t| (None, t)).collect(),
            returns: Box::new(returns),
            captures: Vec::new(), // No captures by default
        }
    }

    /// Create a function type with named parameters
    pub fn function_with_names(params: Vec<(Option<String>, Type)>, returns: Type) -> Self {
        Type::Function {
            params,
            returns: Box::new(returns),
            captures: Vec::new(),
        }
    }

    pub fn function_with_captures(
        params: Vec<Type>,
        returns: Type,
        captures: Vec<(String, Type)>,
    ) -> Self {
        Type::Function {
            params: params.into_iter().map(|t| (None, t)).collect(),
            returns: Box::new(returns),
            captures,
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

    /// Check if this type is a function type
    pub fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }

    /// Create a dict type
    pub fn dict(key: Type, value: Type) -> Self {
        Type::Dict(Box::new(key), Box::new(value))
    }

    /// Create a tuple type
    pub fn tuple(elements: Vec<Type>) -> Self {
        Type::Tuple(elements)
    }

    /// Create a generator type
    pub fn generator(element: Type) -> Self {
        Type::Generator(Box::new(element))
    }

    /// Create a coroutine type
    pub fn coroutine(returns: Type) -> Self {
        Type::Coroutine(Box::new(returns))
    }

    /// Check if this type is awaitable (coroutine or known async library types)
    pub fn is_awaitable(&self) -> bool {
        matches!(self, Type::Coroutine(_))
            || matches!(self, Type::Instance(name) if name == "Future" || name == "Task" || name == "Coroutine")
            || matches!(self, Type::Unknown) // Unknown types are conservatively awaitable
    }

    /// Check if this is a subtype of another type
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {

            (_, Type::Any) => !matches!(self, Type::Never),


            (Type::Never, _) => true,


            (Type::Unknown, _) | (_, Type::Unknown) => true,


            (a, b) if a == b => true,


            (Type::Bool, Type::Int | Type::Float | Type::Complex) => true,
            (Type::Int, Type::Float | Type::Complex) => true,
            (Type::Float, Type::Complex) => true,


            (Type::Optional(t), Type::Union(types)) => {
                types.contains(&Type::None) && types.iter().any(|ty| t.is_subtype_of(ty))
            }


            (t, Type::Optional(opt_t)) => t.is_subtype_of(opt_t) || *t == Type::None,


            (Type::Union(types1), Type::Union(types2)) => types1
                .iter()
                .all(|t1| types2.iter().any(|t2| t1.is_subtype_of(t2))),


            (t, Type::Union(types)) => types.iter().any(|ty| t.is_subtype_of(ty)),



            (Type::List(t1), Type::List(t2)) => t1 == t2,



            (Type::Set(t1), Type::Set(t2)) => t1 == t2,



            (Type::Dict(k1, v1), Type::Dict(k2, v2)) => k1 == k2 && v1 == v2,



            (Type::Tuple(t1), Type::Tuple(t2)) => {
                t1.len() == t2.len() && t1.iter().zip(t2.iter()).all(|(a, b)| a.is_subtype_of(b))
            }


            (
                Type::Function {
                    params: p1,
                    returns: r1,
                    captures: _c1,
                },
                Type::Function {
                    params: p2,
                    returns: r2,
                    captures: _c2,
                },
            ) => {



                p1.len() == p2.len() &&
                p1.iter().zip(p2.iter()).all(|((_n1, t1), (_n2, t2))| t2.is_subtype_of(t1)) && // contravariant
                r1.is_subtype_of(r2) // covariant
            }


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
            Type::Function {
                params,
                returns,
                captures,
            } => {
                let param_names: Vec<_> = params
                    .iter()
                    .map(|(name, ty)| {
                        if let Some(n) = name {
                            format!("{}: {}", n, ty.display_name())
                        } else {
                            ty.display_name()
                        }
                    })
                    .collect();
                let sig = format!("({}) -> {}", param_names.join(", "), returns.display_name());
                if !captures.is_empty() {
                    format!("{} [captures: {}]", sig, captures.len())
                } else {
                    sig
                }
            }
            Type::Class(name) => format!("type[{}]", name),
            Type::Instance(name) => name.clone(),
            Type::Union(types) => {
                let names: Vec<_> = types.iter().map(|t| t.display_name()).collect();
                names.join(" | ")
            }
            Type::Optional(t) => format!("{} | None", t.display_name()),
            Type::TypeVar {
                name,
                bounds,
                variance: _,
            } => {
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
            Type::Generator(element) => format!("generator[{}]", element.display_name()),
            Type::Coroutine(returns) => format!("coroutine[{}]", returns.display_name()),
            Type::Slice => "slice".to_string(),
            Type::TemplateString => "tstring".to_string(),
            Type::AttributeDescriptor {
                kind,
                getter_type,
                setter_type,
            } => {
                let kind_str = match kind {
                    AttributeKind::InstanceAttribute => "instance_attr",
                    AttributeKind::ClassAttribute => "class_attr",
                    AttributeKind::Property => "property",
                    AttributeKind::StaticMethod => "staticmethod",
                    AttributeKind::ClassMethod => "classmethod",
                    AttributeKind::InstanceMethod => "method",
                };
                let setter_info = if setter_type.is_some() {
                    " (rw)"
                } else {
                    " (ro)"
                };
                format!(
                    "{} -> {}{}",
                    kind_str,
                    getter_type.display_name(),
                    setter_info
                )
            }
            Type::Unknown => "?".to_string(),
        }
    }

    /// Check if this type implements the iterator protocol
    pub fn is_iterable(&self) -> bool {
        matches!(
            self,
            Type::List(_)
                | Type::Tuple(_)
                | Type::Set(_)
                | Type::Dict(_, _)
                | Type::Str
                | Type::Bytes
                | Type::Generator(_)
                | Type::Coroutine(_)
        )
    }

    /// Check if this is a generator type
    pub fn is_generator(&self) -> bool {
        matches!(self, Type::Generator(_))
    }

    /// Check if this is a coroutine type
    pub fn is_coroutine(&self) -> bool {
        matches!(self, Type::Coroutine(_))
    }

    /// Get the element type from an iterable/generator
    pub fn get_element_type(&self) -> Option<Type> {
        match self {
            Type::List(elem) | Type::Set(elem) | Type::Generator(elem) => Some((**elem).clone()),
            Type::Coroutine(returns) => Some((**returns).clone()),
            Type::Tuple(elems) if !elems.is_empty() => Some(elems[0].clone()),
            Type::Dict(key, _) => Some((**key).clone()),
            Type::Str => Some(Type::Str),
            Type::Bytes => Some(Type::Int),
            _ => None,
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
