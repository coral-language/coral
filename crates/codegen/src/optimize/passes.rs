//! Optimization passes for bytecode

use crate::bytecode::Instruction;
use std::collections::HashSet;

pub struct OptimizationPass;

impl OptimizationPass {
    pub fn constant_folding(instructions: &mut [Instruction]) -> usize {
        let mut folded = 0;
        let mut i = 0;

        while i < instructions.len() {
            if let Instruction::AddInt {
                dst: _dst,
                lhs,
                rhs,
            } = &instructions[i]
                && i >= 2
            {
                let prev2 = &instructions[i - 2];
                let prev1 = &instructions[i - 1];

                match (prev2, prev1) {
                    (
                        Instruction::LoadConst { dst: lhs_dst, .. },
                        Instruction::LoadConst { dst: rhs_dst, .. },
                    ) if lhs_dst == lhs && rhs_dst == rhs => {
                        folded += 1;
                    }
                    _ => {}
                }
            }
            i += 1;
        }

        folded
    }

    pub fn dead_code_elimination(instructions: &mut [Instruction]) -> usize {
        let mut live_registers = HashSet::new();
        let mut removed = 0;

        for instr in instructions.iter().rev() {
            match instr {
                Instruction::LoadConst { dst, .. }
                | Instruction::Move { dst, .. }
                | Instruction::AddInt { dst, .. }
                | Instruction::SubInt { dst, .. }
                | Instruction::MulInt { dst, .. }
                | Instruction::DivInt { dst, .. } => {
                    if !live_registers.contains(dst) {
                        removed += 1;
                    } else {
                        live_registers.remove(dst);
                    }
                }
                Instruction::Return { .. }
                | Instruction::Raise { .. }
                | Instruction::Call { .. }
                | Instruction::GetItem { .. } => {
                    live_registers.insert(0);
                }
                _ => {}
            }
        }

        removed
    }

    pub fn peephole_optimization(instructions: &mut Vec<Instruction>) -> usize {
        let mut optimized = 0;

        let mut i = 0;
        while i < instructions.len().saturating_sub(1) {
            match (&instructions[i], &instructions[i + 1]) {
                (Instruction::LoadConst { dst: d1, .. }, Instruction::Move { dst: _d2, src })
                    if d1 == src =>
                {
                    instructions.remove(i + 1);
                    optimized += 1;
                }
                (
                    Instruction::Move { dst: d1, src: s1 },
                    Instruction::Move { dst: _d2, src: s2 },
                ) if s1 == _d2 && s2 == d1 => {
                    instructions.remove(i + 1);
                    optimized += 1;
                }
                (Instruction::Nop, _) => {
                    instructions.remove(i);
                    optimized += 1;
                    continue;
                }
                _ => {}
            }
            i += 1;
        }

        optimized
    }

    pub fn register_pressure_reduction(instructions: &Vec<Instruction>) -> usize {
        let mut max_live = 0;
        let mut current_live = 0;

        for instr in instructions {
            match instr {
                Instruction::LoadConst { .. }
                | Instruction::Move { .. }
                | Instruction::AddInt { .. }
                | Instruction::Call { .. } => {
                    current_live += 1;
                    if current_live > max_live {
                        max_live = current_live;
                    }
                }
                Instruction::Return { .. } | Instruction::Raise { .. } => {
                    current_live = 0;
                }
                _ => {}
            }
        }

        max_live
    }

    pub fn run_all(instructions: &mut Vec<Instruction>) -> OptimizationStats {
        OptimizationStats {
            constant_folds: Self::constant_folding(instructions),
            dead_code_removed: Self::dead_code_elimination(instructions),
            peephole_optimized: Self::peephole_optimization(instructions),
            max_register_pressure: Self::register_pressure_reduction(instructions),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct OptimizationStats {
    pub constant_folds: usize,
    pub dead_code_removed: usize,
    pub peephole_optimized: usize,
    pub max_register_pressure: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peephole_removes_nops() {
        let mut instrs = vec![
            Instruction::Nop,
            Instruction::Nop,
            Instruction::LoadConst {
                dst: 0,
                const_id: 1,
            },
            Instruction::Nop,
        ];

        let optimized = OptimizationPass::peephole_optimization(&mut instrs);

        assert!(optimized > 0);
        assert!(instrs.len() < 4);
    }

    #[test]
    fn test_register_pressure_calculation() {
        let instrs = vec![
            Instruction::LoadConst {
                dst: 0,
                const_id: 1,
            },
            Instruction::LoadConst {
                dst: 1,
                const_id: 2,
            },
            Instruction::LoadConst {
                dst: 2,
                const_id: 3,
            },
            Instruction::Return { value: 0 },
        ];

        let pressure = OptimizationPass::register_pressure_reduction(&instrs);

        assert_eq!(pressure, 3);
    }

    #[test]
    fn test_optimization_stats() {
        let mut instrs = vec![Instruction::Nop, Instruction::Nop];

        let stats = OptimizationPass::run_all(&mut instrs);

        assert!(stats.peephole_optimized > 0);
    }
}
