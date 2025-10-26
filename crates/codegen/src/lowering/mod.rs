//! Lowering and intermediate representation generation
//!
//! Transforms bytecode into intermediate representations for analysis and optimization:
//! - Control flow graph construction (CFG)
//! - SSA form conversion
//! - Closure analysis

pub mod closure;
pub mod control_flow;
pub mod ssa;

pub use closure::{ClosureAnalysis, ClosureAnalyzer};
pub use control_flow::{BasicBlock, CFGBuilder, ControlFlowGraph};
pub use ssa::{SSAConverter, SSAForm, SSAValue};

use crate::bytecode::Instruction;

pub struct LoweringPipeline;

impl LoweringPipeline {
    pub fn lower_to_cfg(instructions: &[Instruction]) -> ControlFlowGraph {
        CFGBuilder::build(instructions)
    }

    pub fn lower_to_ssa(instructions: &[Instruction]) -> SSAForm {
        SSAConverter::convert(instructions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lowering_pipeline() {
        let instructions = vec![Instruction::Nop];
        let cfg = LoweringPipeline::lower_to_cfg(&instructions);
        assert_eq!(cfg.entry_block, 0);

        let ssa = LoweringPipeline::lower_to_ssa(&instructions);
        assert_eq!(ssa.entry_block, 0);
    }
}
