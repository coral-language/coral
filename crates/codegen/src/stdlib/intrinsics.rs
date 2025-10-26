//! Intrinsic operations registry - builtins that compile to specialized opcodes

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicOpcode {
    Print,
    Len,
    Range,
    Isinstance,
    Abs,
    Min,
    Max,
    Sum,
    Reversed,
    Enumerate,
    Zip,
    Filter,
    Map,
    Sorted,
    Str,
    Int,
    Float,
    Bool,
    List,
    Dict,
    Set,
    Tuple,
    Type,
    Callable,
    Hasattr,
    Getattr,
    Setattr,
    Delattr,
    Dir,
    Vars,
    Iter,
    Next,
    All,
    Any,
    Ord,
    Chr,
    Bin,
    Hex,
    Oct,
    Round,
    Pow,
    Divmod,
    Open,
    Input,
    Format,
    Hash,
    Id,
    Super,
}

pub struct IntrinsicRegistry {
    intrinsics: HashMap<&'static str, IntrinsicOpcode>,
}

impl IntrinsicRegistry {
    pub fn new() -> Self {
        let mut intrinsics = HashMap::new();

        intrinsics.insert("print", IntrinsicOpcode::Print);
        intrinsics.insert("len", IntrinsicOpcode::Len);
        intrinsics.insert("range", IntrinsicOpcode::Range);
        intrinsics.insert("isinstance", IntrinsicOpcode::Isinstance);
        intrinsics.insert("abs", IntrinsicOpcode::Abs);
        intrinsics.insert("min", IntrinsicOpcode::Min);
        intrinsics.insert("max", IntrinsicOpcode::Max);
        intrinsics.insert("sum", IntrinsicOpcode::Sum);
        intrinsics.insert("reversed", IntrinsicOpcode::Reversed);
        intrinsics.insert("enumerate", IntrinsicOpcode::Enumerate);
        intrinsics.insert("zip", IntrinsicOpcode::Zip);
        intrinsics.insert("filter", IntrinsicOpcode::Filter);
        intrinsics.insert("map", IntrinsicOpcode::Map);
        intrinsics.insert("sorted", IntrinsicOpcode::Sorted);
        intrinsics.insert("str", IntrinsicOpcode::Str);
        intrinsics.insert("int", IntrinsicOpcode::Int);
        intrinsics.insert("float", IntrinsicOpcode::Float);
        intrinsics.insert("bool", IntrinsicOpcode::Bool);
        intrinsics.insert("list", IntrinsicOpcode::List);
        intrinsics.insert("dict", IntrinsicOpcode::Dict);
        intrinsics.insert("set", IntrinsicOpcode::Set);
        intrinsics.insert("tuple", IntrinsicOpcode::Tuple);
        intrinsics.insert("type", IntrinsicOpcode::Type);
        intrinsics.insert("callable", IntrinsicOpcode::Callable);
        intrinsics.insert("hasattr", IntrinsicOpcode::Hasattr);
        intrinsics.insert("getattr", IntrinsicOpcode::Getattr);
        intrinsics.insert("setattr", IntrinsicOpcode::Setattr);
        intrinsics.insert("delattr", IntrinsicOpcode::Delattr);
        intrinsics.insert("dir", IntrinsicOpcode::Dir);
        intrinsics.insert("vars", IntrinsicOpcode::Vars);
        intrinsics.insert("iter", IntrinsicOpcode::Iter);
        intrinsics.insert("next", IntrinsicOpcode::Next);
        intrinsics.insert("all", IntrinsicOpcode::All);
        intrinsics.insert("any", IntrinsicOpcode::Any);
        intrinsics.insert("ord", IntrinsicOpcode::Ord);
        intrinsics.insert("chr", IntrinsicOpcode::Chr);
        intrinsics.insert("bin", IntrinsicOpcode::Bin);
        intrinsics.insert("hex", IntrinsicOpcode::Hex);
        intrinsics.insert("oct", IntrinsicOpcode::Oct);
        intrinsics.insert("round", IntrinsicOpcode::Round);
        intrinsics.insert("pow", IntrinsicOpcode::Pow);
        intrinsics.insert("divmod", IntrinsicOpcode::Divmod);
        intrinsics.insert("open", IntrinsicOpcode::Open);
        intrinsics.insert("input", IntrinsicOpcode::Input);
        intrinsics.insert("format", IntrinsicOpcode::Format);
        intrinsics.insert("hash", IntrinsicOpcode::Hash);
        intrinsics.insert("id", IntrinsicOpcode::Id);
        intrinsics.insert("super", IntrinsicOpcode::Super);

        Self { intrinsics }
    }

    pub fn is_intrinsic(&self, name: &str) -> bool {
        self.intrinsics.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<IntrinsicOpcode> {
        self.intrinsics.get(name).copied()
    }

    pub fn all_intrinsics(&self) -> impl Iterator<Item = (&'static str, IntrinsicOpcode)> {
        self.intrinsics.iter().map(|(&k, &v)| (k, v))
    }
}

impl Default for IntrinsicRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_has_common_builtins() {
        let registry = IntrinsicRegistry::new();
        assert!(registry.is_intrinsic("print"));
        assert!(registry.is_intrinsic("len"));
        assert!(registry.is_intrinsic("range"));
        assert!(registry.is_intrinsic("isinstance"));
    }

    #[test]
    fn test_registry_get_intrinsic() {
        let registry = IntrinsicRegistry::new();
        assert_eq!(registry.get("print"), Some(IntrinsicOpcode::Print));
        assert_eq!(registry.get("len"), Some(IntrinsicOpcode::Len));
        assert_eq!(registry.get("nonexistent"), None);
    }

    #[test]
    fn test_registry_count() {
        let registry = IntrinsicRegistry::new();
        assert!(registry.all_intrinsics().count() > 40);
    }
}
