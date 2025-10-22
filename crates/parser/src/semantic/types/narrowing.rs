//! Type narrowing for flow-sensitive type analysis.
//!
//! This module provides infrastructure for narrowing types based on runtime checks:
//! - isinstance() checks
//! - is None / is not None checks
//! - Truthiness checks
//! - Type guards

use super::Type;
use crate::semantic::passes::control_flow::BlockId;
use std::collections::HashMap;

/// Context for type narrowing within control flow blocks
#[derive(Debug, Clone)]
pub struct TypeNarrowingContext {
    /// Maps variable names to narrowed types per CFG block
    narrowed_types: HashMap<BlockId, HashMap<String, Type>>,
    /// Original types before narrowing (for restoration at join points)
    original_types: HashMap<String, Type>,
}

impl TypeNarrowingContext {
    /// Create a new type narrowing context
    pub fn new() -> Self {
        Self {
            narrowed_types: HashMap::new(),
            original_types: HashMap::new(),
        }
    }

    /// Set the original type for a variable (before any narrowing)
    pub fn set_original_type(&mut self, var_name: String, ty: Type) {
        self.original_types.insert(var_name, ty);
    }

    /// Get the original type for a variable
    pub fn get_original_type(&self, var_name: &str) -> Option<&Type> {
        self.original_types.get(var_name)
    }

    /// Set a narrowed type for a variable in a specific block
    pub fn set_narrowed_type(&mut self, block: BlockId, var_name: String, ty: Type) {
        self.narrowed_types
            .entry(block)
            .or_default()
            .insert(var_name, ty);
    }

    /// Get the narrowed type for a variable in a specific block
    pub fn get_narrowed_type(&self, block: &BlockId, var_name: &str) -> Option<&Type> {
        self.narrowed_types
            .get(block)
            .and_then(|block_types| block_types.get(var_name))
    }

    /// Get the effective type for a variable in a block (narrowed if available, otherwise original)
    pub fn get_effective_type(&self, block: &BlockId, var_name: &str) -> Option<Type> {
        self.get_narrowed_type(block, var_name)
            .cloned()
            .or_else(|| self.get_original_type(var_name).cloned())
    }

    /// Merge types from multiple predecessor blocks at a join point
    pub fn merge_types_from_predecessors(
        &mut self,
        target_block: BlockId,
        predecessors: &[BlockId],
        var_name: &str,
    ) {
        let mut types = Vec::new();

        for pred in predecessors {
            if let Some(ty) = self.get_effective_type(pred, var_name) {
                types.push(ty);
            }
        }

        if types.is_empty() {
            return;
        }

        let merged_type = if types.len() == 1 {
            types.into_iter().next().unwrap()
        } else {
            merge_types(types)
        };

        self.set_narrowed_type(target_block, var_name.to_string(), merged_type);
    }
}

impl Default for TypeNarrowingContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Narrow a type based on isinstance() check
///
/// # Arguments
/// * `original_ty` - The original type before narrowing
/// * `check_ty` - The type being checked in isinstance()
/// * `is_true_branch` - Whether this is the true or false branch
///
/// # Returns
/// The narrowed type
pub fn narrow_isinstance(original_ty: &Type, check_ty: &Type, is_true_branch: bool) -> Type {
    if is_true_branch {
        // In the true branch, narrow to the check type if possible
        match original_ty {
            Type::Union(types) => {
                // Filter union to only types that could match isinstance check
                // A type matches if it's a subtype of check_ty (more specific)
                let narrowed: Vec<Type> = types
                    .iter()
                    .filter(|ty| {
                        // Keep types that are subtypes of check_ty
                        ty.is_subtype_of(check_ty) || *ty == check_ty
                    })
                    .cloned()
                    .collect();

                if narrowed.is_empty() {
                    // If no exact matches, return check_ty (could be a parent type)
                    check_ty.clone()
                } else if narrowed.len() == 1 {
                    narrowed.into_iter().next().unwrap()
                } else {
                    Type::Union(narrowed)
                }
            }
            Type::Any | Type::Unknown => check_ty.clone(),
            ty if ty.is_subtype_of(check_ty) || ty == check_ty => ty.clone(),
            ty if check_ty.is_subtype_of(ty) => check_ty.clone(),
            _ => Type::Never, // isinstance check will fail
        }
    } else {
        // In the false branch, exclude the check type and its subtypes
        match original_ty {
            Type::Union(types) => {
                // Remove types that would match isinstance check
                let remaining: Vec<Type> = types
                    .iter()
                    .filter(|ty| {
                        // Keep types that are not subtypes of check_ty and not equal
                        !ty.is_subtype_of(check_ty) && *ty != check_ty
                    })
                    .cloned()
                    .collect();

                if remaining.is_empty() {
                    Type::Never
                } else if remaining.len() == 1 {
                    remaining.into_iter().next().unwrap()
                } else {
                    Type::Union(remaining)
                }
            }
            ty if ty.is_subtype_of(check_ty) || ty == check_ty => Type::Never,
            ty => ty.clone(),
        }
    }
}

/// Narrow a type based on "is None" or "is not None" check
///
/// # Arguments
/// * `original_ty` - The original type before narrowing
/// * `is_true_branch` - Whether this is checking "is None" (true) or "is not None" (false)
///
/// # Returns
/// The narrowed type
pub fn narrow_is_none(original_ty: &Type, is_true_branch: bool) -> Type {
    if is_true_branch {
        // In "is None" true branch, type must be None
        match original_ty {
            Type::None => Type::None,
            Type::Optional(_inner) => Type::None,
            Type::Union(types) if types.contains(&Type::None) => Type::None,
            Type::Any | Type::Unknown => Type::None,
            _ => Type::Never, // Will never be None
        }
    } else {
        // In "is not None" branch, remove None from type
        match original_ty {
            Type::None => Type::Never,
            Type::Optional(inner) => (**inner).clone(),
            Type::Union(types) => {
                let non_none: Vec<Type> = types
                    .iter()
                    .filter(|ty| !matches!(ty, Type::None))
                    .cloned()
                    .collect();

                if non_none.is_empty() {
                    Type::Never
                } else if non_none.len() == 1 {
                    non_none.into_iter().next().unwrap()
                } else {
                    Type::Union(non_none)
                }
            }
            ty => ty.clone(), // Already not None
        }
    }
}

/// Narrow a type based on truthiness check
///
/// # Arguments
/// * `original_ty` - The original type before narrowing
/// * `is_true_branch` - Whether this is the truthy (true) or falsy (false) branch
///
/// # Returns
/// The narrowed type
pub fn narrow_truthiness(original_ty: &Type, is_true_branch: bool) -> Type {
    if is_true_branch {
        // In truthy branch, remove None and empty container types
        match original_ty {
            Type::None => Type::Never,
            Type::Optional(inner) => (**inner).clone(),
            Type::Union(types) => {
                let truthy: Vec<Type> = types
                    .iter()
                    .filter(|ty| !matches!(ty, Type::None))
                    .cloned()
                    .collect();

                if truthy.is_empty() {
                    Type::Never
                } else if truthy.len() == 1 {
                    truthy.into_iter().next().unwrap()
                } else {
                    Type::Union(truthy)
                }
            }
            ty => ty.clone(), // Keep as-is for other types
        }
    } else {
        // In falsy branch, narrow to None or empty types
        match original_ty {
            Type::Optional(_) => Type::None,
            Type::Union(types) if types.contains(&Type::None) => Type::None,
            ty => ty.clone(), // Can't narrow further without more info
        }
    }
}

/// Merge multiple types into a single type (for join points)
///
/// # Arguments
/// * `types` - Types from different control flow paths
///
/// # Returns
/// The merged type (usually a Union)
pub fn merge_types(types: Vec<Type>) -> Type {
    if types.is_empty() {
        return Type::Unknown;
    }

    if types.len() == 1 {
        return types.into_iter().next().unwrap();
    }

    // Flatten nested unions
    let mut flattened = Vec::new();
    for ty in types {
        match ty {
            Type::Union(inner) => flattened.extend(inner),
            ty => flattened.push(ty),
        }
    }

    // Deduplicate types while preserving order
    let mut seen = std::collections::HashSet::new();
    let mut deduplicated = Vec::new();
    for ty in flattened {
        let key = format!("{:?}", ty);
        if seen.insert(key) {
            deduplicated.push(ty);
        }
    }

    // If all types are the same, return a single type
    if deduplicated.len() == 1 {
        deduplicated.into_iter().next().unwrap()
    } else {
        Type::Union(deduplicated)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_narrow_isinstance_true_branch() {
        let union_ty = Type::Union(vec![Type::Int, Type::Str, Type::Float]);
        let narrowed = narrow_isinstance(&union_ty, &Type::Int, true);
        assert_eq!(narrowed, Type::Int);
    }

    #[test]
    fn test_narrow_isinstance_false_branch() {
        let union_ty = Type::Union(vec![Type::Int, Type::Str, Type::Float]);
        let narrowed = narrow_isinstance(&union_ty, &Type::Int, false);
        assert_eq!(narrowed, Type::Union(vec![Type::Str, Type::Float]));
    }

    #[test]
    fn test_narrow_is_none_true_branch() {
        let optional_ty = Type::Optional(Box::new(Type::Int));
        let narrowed = narrow_is_none(&optional_ty, true);
        assert_eq!(narrowed, Type::None);
    }

    #[test]
    fn test_narrow_is_none_false_branch() {
        let optional_ty = Type::Optional(Box::new(Type::Int));
        let narrowed = narrow_is_none(&optional_ty, false);
        assert_eq!(narrowed, Type::Int);
    }

    #[test]
    fn test_narrow_truthiness_true_branch() {
        let optional_ty = Type::Optional(Box::new(Type::Str));
        let narrowed = narrow_truthiness(&optional_ty, true);
        assert_eq!(narrowed, Type::Str);
    }

    #[test]
    fn test_narrow_truthiness_false_branch() {
        let optional_ty = Type::Optional(Box::new(Type::Str));
        let narrowed = narrow_truthiness(&optional_ty, false);
        assert_eq!(narrowed, Type::None);
    }

    #[test]
    fn test_merge_types() {
        let types = vec![Type::Int, Type::Str, Type::Float];
        let merged = merge_types(types.clone());
        assert_eq!(merged, Type::Union(types));
    }

    #[test]
    fn test_merge_single_type() {
        let types = vec![Type::Int];
        let merged = merge_types(types);
        assert_eq!(merged, Type::Int);
    }
}
