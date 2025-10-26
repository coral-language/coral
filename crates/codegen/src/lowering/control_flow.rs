//! Control flow graph construction
//!
//! Builds a control flow graph (CFG) from bytecode instructions by:
//! - Identifying basic block boundaries
//! - Creating edges between blocks for jumps and sequential flow
//! - Computing predecessor and successor relationships

use crate::bytecode::Instruction;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: u32,
    pub start_idx: u32,
    pub end_idx: u32,
    pub instructions: Vec<Instruction>,
    pub predecessors: Vec<u32>,
    pub successors: Vec<u32>,
    pub is_loop_header: bool,
}

impl BasicBlock {
    pub fn new(id: u32, start_idx: u32, end_idx: u32, instructions: Vec<Instruction>) -> Self {
        Self {
            id,
            start_idx,
            end_idx,
            instructions,
            predecessors: Vec::new(),
            successors: Vec::new(),
            is_loop_header: false,
        }
    }

    pub fn add_predecessor(&mut self, pred_id: u32) {
        if !self.predecessors.contains(&pred_id) {
            self.predecessors.push(pred_id);
        }
    }

    pub fn add_successor(&mut self, succ_id: u32) {
        if !self.successors.contains(&succ_id) {
            self.successors.push(succ_id);
        }
    }

    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    pub fn last_instruction(&self) -> Option<&Instruction> {
        self.instructions.last()
    }
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub blocks: Vec<BasicBlock>,
    pub entry_block: u32,
    pub exit_blocks: Vec<u32>,
    pub edges: Vec<(u32, u32)>,
    pub jump_targets: HashMap<u32, u32>,
}

impl ControlFlowGraph {
    pub fn new(entry_block: u32) -> Self {
        Self {
            blocks: Vec::new(),
            entry_block,
            exit_blocks: Vec::new(),
            edges: Vec::new(),
            jump_targets: HashMap::new(),
        }
    }

    pub fn add_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
    }

    pub fn add_edge(&mut self, from: u32, to: u32) {
        if !self.edges.contains(&(from, to)) {
            self.edges.push((from, to));
        }
    }

    pub fn add_exit_block(&mut self, block_id: u32) {
        if !self.exit_blocks.contains(&block_id) {
            self.exit_blocks.push(block_id);
        }
    }

    pub fn set_jump_target(&mut self, target_idx: u32, block_id: u32) {
        self.jump_targets.insert(target_idx, block_id);
    }

    pub fn get_block(&self, id: u32) -> Option<&BasicBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    pub fn block_count(&self) -> usize {
        self.blocks.len()
    }

    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    pub fn compute_dominators(&self) -> HashMap<u32, HashSet<u32>> {
        let mut dominators: HashMap<u32, HashSet<u32>> = HashMap::new();

        for block in &self.blocks {
            let all_blocks: HashSet<u32> = self.blocks.iter().map(|b| b.id).collect();
            dominators.insert(block.id, all_blocks.clone());
        }

        if let Some(entry) = self.blocks.first() {
            let mut entry_doms = HashSet::new();
            entry_doms.insert(entry.id);
            dominators.insert(entry.id, entry_doms);
        }

        let mut changed = true;
        while changed {
            changed = false;

            for block in &self.blocks {
                if block.id == self.entry_block {
                    continue;
                }

                if block.predecessors.is_empty() {
                    continue;
                }

                let mut new_doms = HashSet::new();
                new_doms.insert(block.id);

                let mut common_doms: Option<HashSet<u32>> = None;
                for pred_id in &block.predecessors {
                    let pred_doms = dominators.get(pred_id).cloned().unwrap_or_default();
                    if let Some(ref mut common) = common_doms {
                        common.retain(|d| pred_doms.contains(d));
                    } else {
                        common_doms = Some(pred_doms);
                    }
                }

                if let Some(common) = common_doms {
                    new_doms.extend(common);
                }

                let old_doms = dominators.get(&block.id).cloned().unwrap_or_default();
                if new_doms != old_doms {
                    changed = true;
                    dominators.insert(block.id, new_doms);
                }
            }
        }

        dominators
    }
}

pub struct CFGBuilder;

impl CFGBuilder {
    pub fn build(instructions: &[Instruction]) -> ControlFlowGraph {
        let mut cfg = ControlFlowGraph::new(0);

        if instructions.is_empty() {
            return cfg;
        }

        let block_boundaries = Self::identify_block_boundaries(instructions);
        let mut blocks = Vec::new();

        for (block_id, (start, end)) in block_boundaries.iter().enumerate() {
            let block_instructions = instructions[*start..=*end].to_vec();
            let block = BasicBlock::new(
                block_id as u32,
                *start as u32,
                *end as u32,
                block_instructions,
            );
            blocks.push(block);
        }

        for block in blocks {
            cfg.add_block(block);
        }

        Self::build_edges(&mut cfg, instructions);
        Self::identify_exit_blocks(&mut cfg);

        cfg
    }

    fn identify_block_boundaries(instructions: &[Instruction]) -> Vec<(usize, usize)> {
        let mut boundaries = vec![(0, 0)];

        for (idx, instr) in instructions.iter().enumerate() {
            match instr {
                Instruction::Jump { .. }
                | Instruction::JumpIfTrue { .. }
                | Instruction::JumpIfFalse { .. }
                | Instruction::Return { .. }
                | Instruction::Raise { .. } => {
                    boundaries.last_mut().unwrap().1 = idx;
                    if idx + 1 < instructions.len() {
                        boundaries.push((idx + 1, idx + 1));
                    }
                }
                _ => {
                    boundaries.last_mut().unwrap().1 = idx;
                }
            }
        }

        boundaries
    }

    fn build_edges(cfg: &mut ControlFlowGraph, _instructions: &[Instruction]) {
        let edges_to_add: Vec<(u32, u32)> = {
            let mut edges = Vec::new();
            for (block_idx, block) in cfg.blocks.iter().enumerate() {
                if let Some(last_instr) = block.last_instruction() {
                    match last_instr {
                        Instruction::Jump { .. } => {
                            if let Some(next_block) = cfg.blocks.get(block_idx + 1) {
                                edges.push((block.id, next_block.id));
                            }
                        }
                        Instruction::JumpIfTrue { .. } | Instruction::JumpIfFalse { .. } => {
                            if let Some(true_target) = cfg.blocks.get(block_idx + 1) {
                                edges.push((block.id, true_target.id));
                            }
                            if let Some(false_target) = cfg.blocks.get(block_idx + 2) {
                                edges.push((block.id, false_target.id));
                            }
                        }
                        Instruction::Return { .. } | Instruction::Raise { .. } => {}
                        _ => {
                            if let Some(next_block) = cfg.blocks.get(block_idx + 1) {
                                edges.push((block.id, next_block.id));
                            }
                        }
                    }
                } else if block_idx + 1 < cfg.blocks.len()
                    && let Some(next_block) = cfg.blocks.get(block_idx + 1)
                {
                    edges.push((block.id, next_block.id));
                }
            }
            edges
        };

        for (from_id, to_id) in edges_to_add {
            cfg.add_edge(from_id, to_id);
        }

        for (from_id, to_id) in &cfg.edges.clone() {
            if let Some(block) = cfg.blocks.iter_mut().find(|b| b.id == *to_id) {
                block.add_predecessor(*from_id);
            }
            if let Some(block) = cfg.blocks.iter_mut().find(|b| b.id == *from_id) {
                block.add_successor(*to_id);
            }
        }
    }

    fn identify_exit_blocks(cfg: &mut ControlFlowGraph) {
        let exit_ids: Vec<u32> = cfg
            .blocks
            .iter()
            .filter(|block| {
                if let Some(last_instr) = block.last_instruction() {
                    matches!(
                        last_instr,
                        Instruction::Return { .. } | Instruction::Raise { .. }
                    )
                } else {
                    false
                }
            })
            .map(|block| block.id)
            .collect();

        for block_id in exit_ids {
            cfg.add_exit_block(block_id);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_block_creation() {
        let instructions = vec![Instruction::Move { dst: 1, src: 0 }];
        let block = BasicBlock::new(0, 0, 0, instructions);

        assert_eq!(block.id, 0);
        assert_eq!(block.instruction_count(), 1);
    }

    #[test]
    fn test_cfg_creation() {
        let cfg = ControlFlowGraph::new(0);
        assert_eq!(cfg.entry_block, 0);
        assert_eq!(cfg.block_count(), 0);
    }

    #[test]
    fn test_cfg_add_block() {
        let mut cfg = ControlFlowGraph::new(0);
        let instructions = vec![Instruction::Nop];
        let block = BasicBlock::new(0, 0, 0, instructions);
        cfg.add_block(block);

        assert_eq!(cfg.block_count(), 1);
    }

    #[test]
    fn test_cfg_add_edge() {
        let mut cfg = ControlFlowGraph::new(0);
        cfg.add_edge(0, 1);
        cfg.add_edge(0, 1);

        assert_eq!(cfg.edge_count(), 1);
    }

    #[test]
    fn test_cfg_builder_empty() {
        let instructions: Vec<Instruction> = vec![];
        let cfg = CFGBuilder::build(&instructions);

        assert_eq!(cfg.block_count(), 0);
    }

    #[test]
    fn test_cfg_builder_single_instruction() {
        let instructions = vec![Instruction::Nop];
        let cfg = CFGBuilder::build(&instructions);

        assert_eq!(cfg.block_count(), 1);
    }

    #[test]
    fn test_dominators() {
        let mut cfg = ControlFlowGraph::new(0);
        let block0 = BasicBlock::new(0, 0, 0, vec![Instruction::Nop]);
        let block1 = BasicBlock::new(1, 1, 1, vec![Instruction::Nop]);
        cfg.add_block(block0);
        cfg.add_block(block1);
        cfg.add_edge(0, 1);

        let dominators = cfg.compute_dominators();
        assert!(dominators.get(&0).map(|d| d.contains(&0)).unwrap_or(false));
    }
}
