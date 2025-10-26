//! Compilation context holding state during bytecode generation

use crate::bytecode::{ConstantPool, Instruction, RegisterAllocator};
use coral_parser::OwnershipRecommendations;
use coral_parser::semantic::symbol::table::SymbolTable as ParserSymbolTable;
use coral_parser::semantic::types::Type as ParserType;
use std::collections::HashMap;

pub struct CompilationContext {
    pub register_allocator: RegisterAllocator,
    pub constant_pool: ConstantPool,
    pub symbol_table: HashMap<String, u16>,
    pub current_scope_depth: u32,
    pub instructions: Vec<Instruction>,
    pub scope_stack: Vec<ScopeFrame>,
    pub next_generator_id: u32,
    // Semantic data from parser
    pub parser_symbol_table: Option<ParserSymbolTable>,
    pub ownership_recommendations: Option<OwnershipRecommendations>,
}

pub struct ScopeFrame {
    pub depth: u32,
    pub locals: HashMap<String, u16>,
    pub parent_locals: usize,
}

impl CompilationContext {
    pub fn new(max_registers: u16) -> Self {
        Self::with_parser_symbols(max_registers, None, None)
    }

    /// Create a CompilationContext with parsed semantic information
    pub fn with_parser_symbols(
        max_registers: u16,
        parser_symbol_table: Option<ParserSymbolTable>,
        ownership_recommendations: Option<OwnershipRecommendations>,
    ) -> Self {
        Self {
            register_allocator: RegisterAllocator::new(max_registers),
            constant_pool: ConstantPool::new(),
            symbol_table: HashMap::new(),
            current_scope_depth: 0,
            instructions: Vec::new(),
            scope_stack: vec![ScopeFrame {
                depth: 0,
                locals: HashMap::new(),
                parent_locals: 0,
            }],
            next_generator_id: 1,
            parser_symbol_table,
            ownership_recommendations,
        }
    }

    /// Look up a symbol using parser's symbol table if available
    pub fn lookup_parser_symbol(
        &self,
        name: &str,
    ) -> Option<(coral_parser::semantic::symbol::Symbol, usize)> {
        self.parser_symbol_table
            .as_ref()
            .and_then(|st| st.lookup(name))
    }

    /// Get the type of a symbol from parser's symbol table
    pub fn get_symbol_type(&self, name: &str) -> Option<ParserType> {
        self.parser_symbol_table
            .as_ref()
            .and_then(|st| st.get_symbol_type(name))
    }

    /// Check if a variable is a move candidate (safe to move)
    pub fn is_move_candidate(&self, var_name: &str) -> bool {
        self.ownership_recommendations
            .as_ref()
            .map(|recs| recs.move_candidates.contains_key(var_name))
            .unwrap_or(false)
    }

    /// Check if a variable is stack-safe (doesn't escape function)
    pub fn is_stack_safe(&self, var_name: &str) -> bool {
        self.ownership_recommendations
            .as_ref()
            .map(|recs| recs.stack_safe_vars.contains(var_name))
            .unwrap_or(false)
    }

    pub fn allocate_register(&mut self, type_id: u32) -> Result<u16, String> {
        self.register_allocator.allocate(type_id)
    }

    pub fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    pub fn emit_many(&mut self, instrs: Vec<Instruction>) {
        self.instructions.extend(instrs);
    }

    pub fn current_instruction_offset(&self) -> u32 {
        self.instructions.len() as u32
    }

    pub fn push_scope(&mut self) {
        self.current_scope_depth += 1;
        let parent_locals = self.scope_stack.last().map(|s| s.locals.len()).unwrap_or(0);
        self.scope_stack.push(ScopeFrame {
            depth: self.current_scope_depth,
            locals: HashMap::new(),
            parent_locals,
        });
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.pop();
        self.current_scope_depth = self.current_scope_depth.saturating_sub(1);
    }

    pub fn define_local(&mut self, name: String, reg_id: u16) -> Result<(), String> {
        if let Some(frame) = self.scope_stack.last_mut() {
            if frame.locals.contains_key(&name) {
                return Err(format!(
                    "Local variable '{}' already defined in scope",
                    name
                ));
            }
            frame.locals.insert(name, reg_id);
            Ok(())
        } else {
            Err("No active scope".to_string())
        }
    }

    pub fn resolve_local(&self, name: &str) -> Option<u16> {
        for frame in self.scope_stack.iter().rev() {
            if let Some(&reg_id) = frame.locals.get(name) {
                return Some(reg_id);
            }
        }
        None
    }

    pub fn add_constant_string(&mut self, value: String) -> u32 {
        self.constant_pool.add_string(value)
    }

    pub fn add_constant_integer(&mut self, value: i64) -> u32 {
        self.constant_pool.add_integer(value)
    }

    pub fn add_constant_float(&mut self, value: f64) -> u32 {
        self.constant_pool.add_float(value)
    }

    pub fn add_constant_bool(&mut self, value: bool) -> u32 {
        self.constant_pool.add_bool(value)
    }

    pub fn add_constant_none(&mut self) -> u32 {
        self.constant_pool.add_none()
    }

    pub fn register_global_symbol(&mut self, name: String, type_id: u32) -> Result<u16, String> {
        let reg_id = self.allocate_register(type_id)?;
        self.symbol_table.insert(name, reg_id);
        Ok(reg_id)
    }

    pub fn resolve_global(&self, name: &str) -> Option<u16> {
        self.symbol_table.get(name).copied()
    }

    pub fn allocate_generator_id(&mut self) -> u32 {
        let id = self.next_generator_id;
        self.next_generator_id += 1;
        id
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_creation() {
        let ctx = CompilationContext::new(256);
        assert_eq!(ctx.current_scope_depth, 0);
        assert_eq!(ctx.scope_stack.len(), 1);
    }

    #[test]
    fn test_register_allocation() {
        let mut ctx = CompilationContext::new(16);
        let r0 = ctx.allocate_register(0).unwrap();
        let r1 = ctx.allocate_register(1).unwrap();

        assert_eq!(r0, 0);
        assert_eq!(r1, 1);
    }

    #[test]
    fn test_scope_management() {
        let mut ctx = CompilationContext::new(16);
        assert_eq!(ctx.current_scope_depth, 0);

        ctx.push_scope();
        assert_eq!(ctx.current_scope_depth, 1);

        ctx.pop_scope();
        assert_eq!(ctx.current_scope_depth, 0);
    }

    #[test]
    fn test_local_definition() {
        let mut ctx = CompilationContext::new(16);
        ctx.push_scope();

        let r0 = ctx.allocate_register(0).unwrap();
        ctx.define_local("x".to_string(), r0).unwrap();

        assert_eq!(ctx.resolve_local("x"), Some(r0));
    }

    #[test]
    fn test_constants() {
        let mut ctx = CompilationContext::new(16);
        let str_id = ctx.add_constant_string("hello".to_string());
        let int_id = ctx.add_constant_integer(42);
        let bool_id = ctx.add_constant_bool(true);

        assert_eq!(str_id, 0);
        assert_eq!(int_id, 1);
        assert_eq!(bool_id, 2);
    }

    #[test]
    fn test_instruction_offset() {
        let mut ctx = CompilationContext::new(16);
        assert_eq!(ctx.current_instruction_offset(), 0);

        ctx.emit(Instruction::Nop);
        assert_eq!(ctx.current_instruction_offset(), 1);

        ctx.emit(Instruction::Nop);
        assert_eq!(ctx.current_instruction_offset(), 2);
    }
}
