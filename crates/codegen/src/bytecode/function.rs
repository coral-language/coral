//! Function bytecode representation
//!
//! Each function is compiled to a bytecode chunk containing:
//! - Function metadata (name, params, return type)
//! - Bytecode instructions
//! - Exception tables
//! - Async state machine info (if async)

use super::instruction::Instruction;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub num_registers: u16,
    pub params: Vec<(String, u32)>,
    pub return_type: u32,
    pub is_async: bool,
    pub state_machine_states: Option<u32>,
    pub exception_handlers: Vec<ExceptionHandler>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExceptionHandler {
    pub start_offset: u32,
    pub end_offset: u32,
    pub handler_offset: u32,
    pub exception_type: u32,
}

impl Function {
    pub fn new(
        name: String,
        num_registers: u16,
        params: Vec<(String, u32)>,
        return_type: u32,
    ) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            num_registers,
            params,
            return_type,
            is_async: false,
            state_machine_states: None,
            exception_handlers: Vec::new(),
        }
    }

    pub fn new_async(
        name: String,
        num_registers: u16,
        params: Vec<(String, u32)>,
        return_type: u32,
        states: u32,
    ) -> Self {
        Self {
            name,
            instructions: Vec::new(),
            num_registers,
            params,
            return_type,
            is_async: true,
            state_machine_states: Some(states),
            exception_handlers: Vec::new(),
        }
    }

    pub fn push_instruction(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    pub fn add_exception_handler(
        &mut self,
        start_offset: u32,
        end_offset: u32,
        handler_offset: u32,
        exception_type: u32,
    ) {
        self.exception_handlers.push(ExceptionHandler {
            start_offset,
            end_offset,
            handler_offset,
            exception_type,
        });
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_function() {
        let func = Function::new(
            "test_func".to_string(),
            10,
            vec![("x".to_string(), 0), ("y".to_string(), 0)],
            1,
        );

        assert_eq!(func.name, "test_func");
        assert_eq!(func.num_registers, 10);
        assert_eq!(func.params.len(), 2);
        assert!(!func.is_async);
    }

    #[test]
    fn test_create_async_function() {
        let func = Function::new_async(
            "async_func".to_string(),
            16,
            vec![("url".to_string(), 2)],
            2,
            3,
        );

        assert!(func.is_async);
        assert_eq!(func.state_machine_states, Some(3));
    }

    #[test]
    fn test_push_instructions() {
        let mut func = Function::new("test".to_string(), 5, vec![], 0);

        func.push_instruction(Instruction::Nop);
        func.push_instruction(Instruction::Nop);

        assert_eq!(func.len(), 2);
    }
}
