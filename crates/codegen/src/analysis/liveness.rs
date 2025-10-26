//! Liveness analysis for register optimization
//!
//! Performs backward dataflow analysis to determine which registers are live
//! (needed for future computation) at each instruction. This information enables:
//! - Dead code elimination
//! - Register reuse decisions
//! - Spill/fill insertion in memory-constrained environments

use crate::bytecode::Instruction;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct LivenessInfo {
    pub live_in: HashMap<u32, HashSet<u16>>,
    pub live_out: HashMap<u32, HashSet<u16>>,
}

impl LivenessInfo {
    pub fn new() -> Self {
        Self {
            live_in: HashMap::new(),
            live_out: HashMap::new(),
        }
    }

    pub fn set_live_in(&mut self, instr_id: u32, registers: HashSet<u16>) {
        self.live_in.insert(instr_id, registers);
    }

    pub fn set_live_out(&mut self, instr_id: u32, registers: HashSet<u16>) {
        self.live_out.insert(instr_id, registers);
    }

    pub fn get_live_in(&self, instr_id: u32) -> HashSet<u16> {
        self.live_in.get(&instr_id).cloned().unwrap_or_default()
    }

    pub fn get_live_out(&self, instr_id: u32) -> HashSet<u16> {
        self.live_out.get(&instr_id).cloned().unwrap_or_default()
    }

    pub fn is_register_live_at(&self, instr_id: u32, register: u16) -> bool {
        self.get_live_in(instr_id).contains(&register)
    }
}

impl Default for LivenessInfo {
    fn default() -> Self {
        Self::new()
    }
}

pub struct LivenessAnalyzer;

impl LivenessAnalyzer {
    pub fn analyze(instructions: &[Instruction]) -> LivenessInfo {
        let mut info = LivenessInfo::new();

        if instructions.is_empty() {
            return info;
        }

        let mut live_out: HashMap<u32, HashSet<u16>> = HashMap::new();
        for i in 0..instructions.len() {
            live_out.insert(i as u32, HashSet::new());
        }

        let mut changed = true;
        let max_iterations = 100;
        let mut iterations = 0;

        while changed && iterations < max_iterations {
            changed = false;
            iterations += 1;

            for i in (0..instructions.len()).rev() {
                let instr_id = i as u32;
                let mut live_in = live_out.get(&(instr_id + 1)).cloned().unwrap_or_default();

                let (used, defined) = Self::get_registers_for_instruction(&instructions[i]);

                for def_reg in &defined {
                    live_in.remove(def_reg);
                }

                for use_reg in &used {
                    live_in.insert(*use_reg);
                }

                let old_live_out = live_out.get(&instr_id).cloned().unwrap_or_default();

                if live_in != old_live_out {
                    changed = true;
                }

                live_out.insert(instr_id, live_in);
            }
        }

        for (instr_id, live_set) in live_out.iter() {
            info.set_live_in(*instr_id, live_set.clone());

            if *instr_id + 1 < instructions.len() as u32 {
                info.set_live_out(*instr_id, live_set.clone());
            }
        }

        info
    }

    fn get_registers_for_instruction(instr: &Instruction) -> (HashSet<u16>, HashSet<u16>) {
        let mut used = HashSet::new();
        let mut defined = HashSet::new();

        match instr {
            Instruction::Move { dst, src } => {
                used.insert(*src);
                defined.insert(*dst);
            }
            Instruction::LoadConst { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::LoadGlobal { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::StoreGlobal { src, .. } => {
                used.insert(*src);
            }
            Instruction::LoadLocal { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::StoreLocal { src, .. } => {
                used.insert(*src);
            }
            Instruction::AddInt { dst, lhs, rhs }
            | Instruction::SubInt { dst, lhs, rhs }
            | Instruction::MulInt { dst, lhs, rhs }
            | Instruction::DivInt { dst, lhs, rhs }
            | Instruction::ModInt { dst, lhs, rhs }
            | Instruction::AddFloat { dst, lhs, rhs }
            | Instruction::SubFloat { dst, lhs, rhs }
            | Instruction::MulFloat { dst, lhs, rhs }
            | Instruction::DivFloat { dst, lhs, rhs }
            | Instruction::ModFloat { dst, lhs, rhs }
            | Instruction::ConcatStr { dst, lhs, rhs } => {
                used.insert(*lhs);
                used.insert(*rhs);
                defined.insert(*dst);
            }
            Instruction::PowInt { dst, base, exp } | Instruction::PowFloat { dst, base, exp } => {
                used.insert(*base);
                used.insert(*exp);
                defined.insert(*dst);
            }
            Instruction::NegInt { dst, operand }
            | Instruction::NegFloat { dst, operand }
            | Instruction::Not { dst, operand } => {
                used.insert(*operand);
                defined.insert(*dst);
            }
            Instruction::EqInt { dst, lhs, rhs }
            | Instruction::NotEqInt { dst, lhs, rhs }
            | Instruction::LtInt { dst, lhs, rhs }
            | Instruction::LtEqInt { dst, lhs, rhs }
            | Instruction::GtInt { dst, lhs, rhs }
            | Instruction::GtEqInt { dst, lhs, rhs }
            | Instruction::EqFloat { dst, lhs, rhs }
            | Instruction::LtFloat { dst, lhs, rhs } => {
                used.insert(*lhs);
                used.insert(*rhs);
                defined.insert(*dst);
            }
            Instruction::And { dst, lhs, rhs } | Instruction::Or { dst, lhs, rhs } => {
                used.insert(*lhs);
                used.insert(*rhs);
                defined.insert(*dst);
            }
            Instruction::IsNone { dst, operand } => {
                used.insert(*operand);
                defined.insert(*dst);
            }
            Instruction::Jump { .. } => {}
            Instruction::JumpIfTrue { condition, .. } | Instruction::JumpIfFalse { condition, .. } => {
                used.insert(*condition);
            }
            Instruction::Call { result, func_reg, arg_base, arg_count } => {
                used.insert(*func_reg);
                for i in 0..*arg_count {
                    used.insert(arg_base + i);
                }
                defined.insert(*result);
            }
            Instruction::CallNative { result, .. } => {
                defined.insert(*result);
            }
            Instruction::Return { value } => {
                used.insert(*value);
            }
            Instruction::NewList { dst, .. }
            | Instruction::NewDict { dst, .. }
            | Instruction::NewSet { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::NewTuple { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::GetItem { dst, obj, key } => {
                used.insert(*obj);
                used.insert(*key);
                defined.insert(*dst);
            }
            Instruction::SetItem { obj, key, value } => {
                used.insert(*obj);
                used.insert(*key);
                used.insert(*value);
            }
            Instruction::GetAttr { dst, obj, .. } => {
                used.insert(*obj);
                defined.insert(*dst);
            }
            Instruction::SetAttr { obj, value, .. } => {
                used.insert(*obj);
                used.insert(*value);
            }
            Instruction::Len { dst, obj } => {
                used.insert(*obj);
                defined.insert(*dst);
            }
            Instruction::MatchType { dst, value, .. } => {
                used.insert(*value);
                defined.insert(*dst);
            }
            Instruction::MatchValue { dst, value, .. } => {
                used.insert(*value);
                defined.insert(*dst);
            }
            Instruction::Await { result, future, .. } => {
                used.insert(*future);
                defined.insert(*result);
            }
            Instruction::Yield { value } => {
                used.insert(*value);
            }
            Instruction::YieldFrom { value } => {
                used.insert(*value);
            }
            Instruction::NewGenerator { dst, .. } => {
                defined.insert(*dst);
            }
            Instruction::TypeOf { dst, value } => {
                used.insert(*value);
                defined.insert(*dst);
            }
            Instruction::IsInstance { dst, value, .. } => {
                used.insert(*value);
                defined.insert(*dst);
            }
            Instruction::Cast { dst, value, .. } => {
                used.insert(*value);
                defined.insert(*dst);
            }
            Instruction::StrLen { dst, str_reg } => {
                used.insert(*str_reg);
                defined.insert(*dst);
            }
            Instruction::PrintDebug { value } => {
                used.insert(*value);
            }
            _ => {}
        }

        (used, defined)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_liveness_empty_instructions() {
        let instructions: Vec<Instruction> = vec![];
        let info = LivenessAnalyzer::analyze(&instructions);
        assert!(info.live_in.is_empty());
    }

    #[test]
    fn test_liveness_simple_move() {
        let instructions = vec![Instruction::Move { dst: 1, src: 0 }];
        let info = LivenessAnalyzer::analyze(&instructions);

        let live_in = info.get_live_in(0);
        assert!(live_in.contains(&0));
    }

    #[test]
    fn test_get_registers() {
        let (used, defined) = LivenessAnalyzer::get_registers_for_instruction(&Instruction::Move {
            dst: 1,
            src: 0,
        });

        assert!(used.contains(&0));
        assert!(defined.contains(&1));
    }

    #[test]
    fn test_liveness_arithmetic() {
        let instructions = vec![Instruction::AddInt {
            dst: 2,
            lhs: 0,
            rhs: 1,
        }];
        let info = LivenessAnalyzer::analyze(&instructions);

        let live_in = info.get_live_in(0);
        assert!(live_in.contains(&0));
        assert!(live_in.contains(&1));
    }
}
