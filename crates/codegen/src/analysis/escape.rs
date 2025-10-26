//! Escape analysis for memory allocation optimization
//!
//! Determines which values can be safely stack-allocated versus requiring heap allocation.
//! Values that "escape" (are returned, stored in global state, or passed to unanalyzed functions)
//! must be heap-allocated. Stack allocation is only safe for values whose lifetime is contained
//! within the current function.

use crate::bytecode::Instruction;
use std::collections::HashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AllocationKind {
    Stack,
    Heap,
}

#[derive(Debug, Clone)]
pub struct EscapeInfo {
    escaping: HashSet<u16>,
    stack_allocatable: HashSet<u16>,
}

impl EscapeInfo {
    pub fn new() -> Self {
        Self {
            escaping: HashSet::new(),
            stack_allocatable: HashSet::new(),
        }
    }

    pub fn mark_escaping(&mut self, register: u16) {
        self.escaping.insert(register);
        self.stack_allocatable.remove(&register);
    }

    pub fn mark_stack_allocatable(&mut self, register: u16) {
        if !self.escaping.contains(&register) {
            self.stack_allocatable.insert(register);
        }
    }

    pub fn allocate(&self, register: u16) -> AllocationKind {
        if self.escaping.contains(&register) {
            AllocationKind::Heap
        } else if self.stack_allocatable.contains(&register) {
            AllocationKind::Stack
        } else {
            AllocationKind::Heap
        }
    }

    pub fn get_escape_info(&self) -> &HashSet<u16> {
        &self.escaping
    }

    pub fn get_stack_allocatable(&self) -> &HashSet<u16> {
        &self.stack_allocatable
    }

    pub fn stack_allocatable_count(&self) -> usize {
        self.stack_allocatable.len()
    }

    pub fn escaping_count(&self) -> usize {
        self.escaping.len()
    }
}

impl Default for EscapeInfo {
    fn default() -> Self {
        Self::new()
    }
}

pub struct EscapeAnalyzer;

impl EscapeAnalyzer {
    pub fn analyze(instructions: &[Instruction]) -> EscapeInfo {
        let mut info = EscapeInfo::new();

        for instr in instructions {
            match instr {
                Instruction::Return { value } => {
                    info.mark_escaping(*value);
                }
                Instruction::StoreGlobal { src, .. } => {
                    info.mark_escaping(*src);
                }
                Instruction::SetAttr { value, .. } => {
                    info.mark_escaping(*value);
                }
                Instruction::SetItem { value, .. } => {
                    info.mark_escaping(*value);
                }
                Instruction::Call {
                    arg_base,
                    arg_count,
                    ..
                } => {
                    for i in 0..*arg_count {
                        let arg_reg = arg_base + i;
                        info.mark_escaping(arg_reg);
                    }
                }
                Instruction::CallNative {
                    arg_base,
                    arg_count,
                    ..
                } => {
                    for i in 0..*arg_count {
                        let arg_reg = arg_base + i;
                        info.mark_escaping(arg_reg);
                    }
                }
                Instruction::Raise { exception } => {
                    info.mark_escaping(*exception);
                }
                Instruction::NewList { dst, .. }
                | Instruction::NewDict { dst, .. }
                | Instruction::NewSet { dst, .. }
                | Instruction::NewTuple { dst, .. } => {
                    info.mark_stack_allocatable(*dst);
                }
                Instruction::Move { dst, src } => {
                    if info.escaping.contains(src) {
                        info.mark_escaping(*dst);
                    } else if !info.escaping.contains(dst) {
                        info.mark_stack_allocatable(*dst);
                    }
                }
                _ => {}
            }
        }

        info
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_info_creation() {
        let info = EscapeInfo::new();
        assert_eq!(info.escaping_count(), 0);
        assert_eq!(info.stack_allocatable_count(), 0);
    }

    #[test]
    fn test_mark_escaping() {
        let mut info = EscapeInfo::new();
        info.mark_stack_allocatable(0);
        assert_eq!(info.allocate(0), AllocationKind::Stack);

        info.mark_escaping(0);
        assert_eq!(info.allocate(0), AllocationKind::Heap);
    }

    #[test]
    fn test_escape_analysis_return() {
        let instructions = vec![Instruction::Return { value: 0 }];
        let info = EscapeAnalyzer::analyze(&instructions);

        assert!(info.get_escape_info().contains(&0));
    }

    #[test]
    fn test_escape_analysis_new_list() {
        let instructions = vec![Instruction::NewList {
            dst: 0,
            size_hint: 0,
        }];
        let info = EscapeAnalyzer::analyze(&instructions);

        assert_eq!(info.allocate(0), AllocationKind::Stack);
    }

    #[test]
    fn test_escape_analysis_call() {
        let instructions = vec![Instruction::Call {
            result: 0,
            func_reg: 1,
            arg_base: 2,
            arg_count: 2,
        }];
        let info = EscapeAnalyzer::analyze(&instructions);

        assert!(info.get_escape_info().contains(&2));
        assert!(info.get_escape_info().contains(&3));
    }

    #[test]
    fn test_escape_analysis_move() {
        let mut instructions = vec![Instruction::NewList {
            dst: 0,
            size_hint: 0,
        }];
        instructions.push(Instruction::Move { dst: 1, src: 0 });

        let info = EscapeAnalyzer::analyze(&instructions);

        assert_eq!(info.allocate(0), AllocationKind::Stack);
        assert_eq!(info.allocate(1), AllocationKind::Stack);
    }
}
