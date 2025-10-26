//! Builtin intrinsic operations that compile to direct opcodes

use std::collections::HashMap;
use crate::bytecode::Opcode;

pub struct IntrinsicRegistry {
    intrinsics: HashMap<&'static str, Opcode>,
}

impl IntrinsicRegistry {
    pub fn new() -> Self {
        let mut registry = Self {
            intrinsics: HashMap::new(),
        };
        registry.register_all();
        registry
    }

    fn register_all(&mut self) {
        self.intrinsics.insert("len", Opcode::Len);
        self.intrinsics.insert("print", Opcode::PrintDebug);
        self.intrinsics.insert("type", Opcode::TypeOf);
        self.intrinsics.insert("isinstance", Opcode::IsInstance);
    }

    pub fn get_opcode(&self, name: &str) -> Option<Opcode> {
        self.intrinsics.get(name).copied()
    }

    pub fn is_intrinsic(&self, name: &str) -> bool {
        self.intrinsics.contains_key(name)
    }
}

impl Default for IntrinsicRegistry {
    fn default() -> Self {
        Self::new()
    }
}
