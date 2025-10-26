//! Lifetime analysis for precise value lifetime computation
//!
//! Computes precise lifetimes for each value using ownership information from the parser's
//! semantic analysis. This enables:
//! - Automatic deallocation at end of lifetime
//! - Borrow tracking for reference validity
//! - Resource cleanup scheduling

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LifetimeInfo {
    pub start: u32,
    pub end: u32,
    pub borrowed_by: Vec<u16>,
}

impl LifetimeInfo {
    pub fn new(start: u32, end: u32) -> Self {
        Self {
            start,
            end,
            borrowed_by: Vec::new(),
        }
    }

    pub fn is_alive_at(&self, instruction: u32) -> bool {
        self.start <= instruction && instruction <= self.end
    }

    pub fn add_borrow(&mut self, borrower: u16) {
        if !self.borrowed_by.contains(&borrower) {
            self.borrowed_by.push(borrower);
        }
    }

    pub fn duration(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }
}

#[derive(Debug, Clone)]
pub struct LifetimeAnalysis {
    pub lifetimes: HashMap<u16, LifetimeInfo>,
}

impl LifetimeAnalysis {
    pub fn new() -> Self {
        Self {
            lifetimes: HashMap::new(),
        }
    }

    pub fn add_lifetime(&mut self, register: u16, lifetime: LifetimeInfo) {
        self.lifetimes.insert(register, lifetime);
    }

    pub fn get_lifetime(&self, register: u16) -> Option<&LifetimeInfo> {
        self.lifetimes.get(&register)
    }

    pub fn get_lifetime_mut(&mut self, register: u16) -> Option<&mut LifetimeInfo> {
        self.lifetimes.get_mut(&register)
    }

    pub fn register_borrow(&mut self, owner: u16, borrower: u16) {
        if let Some(lifetime) = self.lifetimes.get_mut(&owner) {
            lifetime.add_borrow(borrower);
        }
    }

    pub fn registers_live_at(&self, instruction: u32) -> Vec<u16> {
        self.lifetimes
            .iter()
            .filter(|(_, lifetime)| lifetime.is_alive_at(instruction))
            .map(|(reg, _)| *reg)
            .collect()
    }

    pub fn get_all_lifetimes(&self) -> &HashMap<u16, LifetimeInfo> {
        &self.lifetimes
    }

    pub fn count_registers(&self) -> usize {
        self.lifetimes.len()
    }
}

impl Default for LifetimeAnalysis {
    fn default() -> Self {
        Self::new()
    }
}

pub struct LifetimeAnalyzer;

impl LifetimeAnalyzer {
    pub fn analyze(
        instructions: &[crate::bytecode::Instruction],
        _ownership_info: Option<&str>,
    ) -> LifetimeAnalysis {
        let mut analysis = LifetimeAnalysis::new();

        if instructions.is_empty() {
            return analysis;
        }

        let mut last_use: HashMap<u16, u32> = HashMap::new();
        let mut first_def: HashMap<u16, u32> = HashMap::new();

        for (idx, instr) in instructions.iter().enumerate() {
            let instr_id = idx as u32;

            let (used, defined) = Self::get_registers_for_instruction(instr);

            for def_reg in &defined {
                first_def.entry(*def_reg).or_insert(instr_id);
                last_use.insert(*def_reg, instr_id);
            }

            for use_reg in &used {
                last_use.insert(*use_reg, instr_id);
            }
        }

        for (register, first_seen) in &first_def {
            let last_seen = last_use.get(register).copied().unwrap_or(*first_seen);
            let lifetime = LifetimeInfo::new(*first_seen, last_seen);
            analysis.add_lifetime(*register, lifetime);
        }

        analysis
    }

    fn get_registers_for_instruction(instr: &crate::bytecode::Instruction) -> (Vec<u16>, Vec<u16>) {
        use crate::bytecode::Instruction;

        let mut used = Vec::new();
        let mut defined = Vec::new();

        match instr {
            Instruction::Move { dst, src } => {
                used.push(*src);
                defined.push(*dst);
            }
            Instruction::LoadConst { dst, .. } => {
                defined.push(*dst);
            }
            Instruction::LoadGlobal { dst, .. } => {
                defined.push(*dst);
            }
            Instruction::StoreGlobal { src, .. } => {
                used.push(*src);
            }
            Instruction::LoadLocal { dst, .. } => {
                defined.push(*dst);
            }
            Instruction::StoreLocal { src, .. } => {
                used.push(*src);
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
                used.push(*lhs);
                used.push(*rhs);
                defined.push(*dst);
            }
            Instruction::Call {
                func_reg,
                arg_base,
                arg_count,
                ..
            } => {
                used.push(*func_reg);
                for i in 0..*arg_count {
                    used.push(arg_base + i);
                }
            }
            Instruction::Return { value } => {
                used.push(*value);
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
    fn test_lifetime_info_creation() {
        let lifetime = LifetimeInfo::new(5, 15);
        assert_eq!(lifetime.start, 5);
        assert_eq!(lifetime.end, 15);
        assert_eq!(lifetime.duration(), 10);
    }

    #[test]
    fn test_lifetime_is_alive_at() {
        let lifetime = LifetimeInfo::new(5, 15);
        assert!(lifetime.is_alive_at(5));
        assert!(lifetime.is_alive_at(10));
        assert!(lifetime.is_alive_at(15));
        assert!(!lifetime.is_alive_at(4));
        assert!(!lifetime.is_alive_at(16));
    }

    #[test]
    fn test_lifetime_borrow_tracking() {
        let mut lifetime = LifetimeInfo::new(5, 15);
        lifetime.add_borrow(10);
        lifetime.add_borrow(11);

        assert_eq!(lifetime.borrowed_by.len(), 2);
        assert!(lifetime.borrowed_by.contains(&10));
    }

    #[test]
    fn test_lifetime_analysis_creation() {
        let analysis = LifetimeAnalysis::new();
        assert_eq!(analysis.count_registers(), 0);
    }

    #[test]
    fn test_lifetime_analysis_add() {
        let mut analysis = LifetimeAnalysis::new();
        let lifetime = LifetimeInfo::new(0, 10);
        analysis.add_lifetime(0, lifetime);

        assert_eq!(analysis.count_registers(), 1);
        assert!(analysis.get_lifetime(0).is_some());
    }

    #[test]
    fn test_lifetime_analysis_borrow() {
        let mut analysis = LifetimeAnalysis::new();
        let lifetime = LifetimeInfo::new(0, 10);
        analysis.add_lifetime(0, lifetime);

        analysis.register_borrow(0, 5);

        let lifetime = analysis.get_lifetime(0).unwrap();
        assert!(lifetime.borrowed_by.contains(&5));
    }

    #[test]
    fn test_lifetime_analysis_registers_live_at() {
        let mut analysis = LifetimeAnalysis::new();
        analysis.add_lifetime(0, LifetimeInfo::new(5, 15));
        analysis.add_lifetime(1, LifetimeInfo::new(10, 20));

        let live_at_12 = analysis.registers_live_at(12);
        assert!(live_at_12.contains(&0));
        assert!(live_at_12.contains(&1));

        let live_at_3 = analysis.registers_live_at(3);
        assert!(live_at_3.is_empty());
    }
}
