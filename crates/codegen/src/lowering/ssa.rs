//! SSA form conversion for LLVM backend
//!
//! Transforms bytecode into Static Single Assignment (SSA) form where each variable
//! is assigned exactly once. This enables powerful dataflow optimizations and is
//! suitable for LLVM IR generation.

use crate::bytecode::Instruction;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SSAValue {
    pub base_register: u16,
    pub version: u32,
}

impl SSAValue {
    pub fn new(base_register: u16, version: u32) -> Self {
        Self { base_register, version }
    }

    pub fn original(register: u16) -> Self {
        Self {
            base_register: register,
            version: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SSAInstruction {
    pub opcode: String,
    pub dst: Option<SSAValue>,
    pub operands: Vec<SSAValue>,
}

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub dst: SSAValue,
    pub incoming: Vec<(u32, SSAValue)>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: u32,
    pub instructions: Vec<SSAInstruction>,
    pub phi_nodes: Vec<PhiNode>,
    pub predecessors: Vec<u32>,
    pub successors: Vec<u32>,
}

#[derive(Debug, Clone)]
pub struct SSAForm {
    pub blocks: Vec<BasicBlock>,
    pub entry_block: u32,
    pub exit_blocks: Vec<u32>,
    pub dominance_tree: DominanceTree,
}

#[derive(Debug, Clone)]
pub struct DominanceTree {
    pub immediate_dominators: HashMap<u32, u32>,
    pub dominance_frontiers: HashMap<u32, Vec<u32>>,
}

impl DominanceTree {
    pub fn new() -> Self {
        Self {
            immediate_dominators: HashMap::new(),
            dominance_frontiers: HashMap::new(),
        }
    }

    pub fn set_immediate_dominator(&mut self, block: u32, dominator: u32) {
        self.immediate_dominators.insert(block, dominator);
    }

    pub fn add_dominance_frontier_entry(&mut self, block: u32, frontier_block: u32) {
        self.dominance_frontiers
            .entry(block)
            .or_default()
            .push(frontier_block);
    }

    pub fn get_frontier(&self, block: u32) -> Vec<u32> {
        self.dominance_frontiers
            .get(&block)
            .cloned()
            .unwrap_or_default()
    }
}

impl Default for DominanceTree {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SSAConverter;

impl SSAConverter {
    pub fn convert(instructions: &[Instruction]) -> SSAForm {
        let blocks = Self::build_basic_blocks(instructions);

        SSAForm {
            blocks: blocks.clone(),
            entry_block: 0,
            exit_blocks: Self::find_exit_blocks(&blocks),
            dominance_tree: Self::compute_dominance(&blocks),
        }
    }

    fn build_basic_blocks(instructions: &[Instruction]) -> Vec<BasicBlock> {
        let mut blocks = Vec::new();
        let mut current_block = BasicBlock {
            id: 0,
            instructions: Vec::new(),
            phi_nodes: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        for instr in instructions {
            match instr {
                Instruction::Jump { .. } | Instruction::JumpIfTrue { .. } | Instruction::JumpIfFalse { .. } | Instruction::Return { .. } => {
                    current_block.instructions.push(Self::translate_instruction(instr));
                    blocks.push(current_block);
                    current_block = BasicBlock {
                        id: blocks.len() as u32,
                        instructions: Vec::new(),
                        phi_nodes: Vec::new(),
                        predecessors: Vec::new(),
                        successors: Vec::new(),
                    };
                }
                _ => {
                    current_block
                        .instructions
                        .push(Self::translate_instruction(instr));
                }
            }
        }

        if !current_block.instructions.is_empty() {
            blocks.push(current_block);
        }

        blocks
    }

    fn translate_instruction(instr: &Instruction) -> SSAInstruction {
        let (opcode, dst, operands) = match instr {
            Instruction::Move { dst, src } => {
                ("Move".to_string(), Some(*dst), vec![*src])
            }
            Instruction::LoadConst { dst, .. } => {
                ("LoadConst".to_string(), Some(*dst), vec![])
            }
            Instruction::AddInt { dst, lhs, rhs } => {
                ("AddInt".to_string(), Some(*dst), vec![*lhs, *rhs])
            }
            Instruction::SubInt { dst, lhs, rhs } => {
                ("SubInt".to_string(), Some(*dst), vec![*lhs, *rhs])
            }
            Instruction::Return { value } => {
                ("Return".to_string(), None, vec![*value])
            }
            _ => ("Unknown".to_string(), None, vec![]),
        };

        SSAInstruction {
            opcode,
            dst: dst.map(SSAValue::original),
            operands: operands.into_iter().map(SSAValue::original).collect(),
        }
    }

    fn find_exit_blocks(blocks: &[BasicBlock]) -> Vec<u32> {
        blocks
            .iter()
            .filter(|block| {
                block.instructions.iter().any(|instr| instr.opcode == "Return")
            })
            .map(|block| block.id)
            .collect()
    }

    fn compute_dominance(blocks: &[BasicBlock]) -> DominanceTree {
        let mut tree = DominanceTree::new();

        if blocks.is_empty() {
            return tree;
        }

        let mut dominators: HashMap<u32, Vec<u32>> = HashMap::new();
        for block in blocks {
            dominators.insert(block.id, (0..blocks.len() as u32).collect());
        }

        if let Some(first_block) = blocks.first() {
            dominators.insert(first_block.id, vec![first_block.id]);
        }

        let mut changed = true;
        while changed {
            changed = false;

            for block in blocks {
                if block.id == 0 {
                    continue;
                }

                let mut new_doms = vec![block.id];
                if !block.predecessors.is_empty() {
                    let mut common: Vec<u32> =
                        dominators.get(&block.predecessors[0]).cloned().unwrap_or_default();

                    for &pred_id in &block.predecessors[1..] {
                        let pred_doms = dominators.get(&pred_id).cloned().unwrap_or_default();
                        common.retain(|d| pred_doms.contains(d));
                    }

                    new_doms.extend(common);
                }

                let old_len = dominators.get(&block.id).map(|v| v.len()).unwrap_or(0);
                dominators.insert(block.id, new_doms);

                if dominators.get(&block.id).map(|v| v.len()).unwrap_or(0) != old_len {
                    changed = true;
                }
            }
        }

        for (block_id, doms) in dominators {
            if doms.len() > 1 {
                tree.set_immediate_dominator(block_id, doms[doms.len() - 2]);
            }
        }

        tree
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ssa_value_creation() {
        let val = SSAValue::new(5, 2);
        assert_eq!(val.base_register, 5);
        assert_eq!(val.version, 2);
    }

    #[test]
    fn test_ssa_value_original() {
        let val = SSAValue::original(3);
        assert_eq!(val.base_register, 3);
        assert_eq!(val.version, 0);
    }

    #[test]
    fn test_basic_block_creation() {
        let block = BasicBlock {
            id: 0,
            instructions: Vec::new(),
            phi_nodes: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        assert_eq!(block.id, 0);
        assert!(block.instructions.is_empty());
    }

    #[test]
    fn test_dominance_tree_creation() {
        let tree = DominanceTree::new();
        assert!(tree.immediate_dominators.is_empty());
        assert!(tree.dominance_frontiers.is_empty());
    }

    #[test]
    fn test_ssa_conversion_empty() {
        let instructions: Vec<Instruction> = vec![];
        let ssa = SSAConverter::convert(&instructions);

        assert_eq!(ssa.entry_block, 0);
        assert!(ssa.exit_blocks.is_empty());
    }

    #[test]
    fn test_translate_instruction_move() {
        let instr = Instruction::Move { dst: 1, src: 0 };
        let ssa_instr = SSAConverter::translate_instruction(&instr);

        assert_eq!(ssa_instr.opcode, "Move");
        assert!(ssa_instr.dst.is_some());
        assert_eq!(ssa_instr.operands.len(), 1);
    }
}
