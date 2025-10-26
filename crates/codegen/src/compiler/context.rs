//! Compilation context holding state during bytecode generation

use crate::bytecode::{RegisterAllocator, ConstantPool};
use std::collections::HashMap;

pub struct CompilationContext {
    pub register_allocator: RegisterAllocator,
    pub constant_pool: ConstantPool,
    pub symbol_table: HashMap<String, u32>,
    pub current_scope_depth: u32,
}

impl CompilationContext {
    pub fn new(max_registers: u16) -> Self {
        Self {
            register_allocator: RegisterAllocator::new(max_registers),
            constant_pool: ConstantPool::new(),
            symbol_table: HashMap::new(),
            current_scope_depth: 0,
        }
    }
}
