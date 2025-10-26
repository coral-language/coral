//! Static analysis for bytecode
//!
//! Provides analysis passes for:
//! - Liveness analysis: Determines which registers are live at each instruction
//! - Escape analysis: Determines which values can be stack-allocated
//! - Lifetime analysis: Computes precise lifetimes for each value

pub mod escape;
pub mod lifetime;
pub mod liveness;

pub use escape::{AllocationKind, EscapeAnalyzer, EscapeInfo};
pub use lifetime::{LifetimeAnalysis, LifetimeAnalyzer, LifetimeInfo};
pub use liveness::{LivenessAnalyzer, LivenessInfo};

use crate::bytecode::Instruction;

pub struct AnalysisResult {
    pub liveness: LivenessInfo,
    pub escape: EscapeInfo,
    pub lifetime: LifetimeAnalysis,
}

pub fn analyze(instructions: &[Instruction]) -> AnalysisResult {
    AnalysisResult {
        liveness: LivenessAnalyzer::analyze(instructions),
        escape: EscapeAnalyzer::analyze(instructions),
        lifetime: LifetimeAnalyzer::analyze(instructions, None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_full_analysis() {
        let instructions = vec![Instruction::Nop];
        let result = analyze(&instructions);

        assert_eq!(result.liveness.live_in.len(), 1);
        assert_eq!(result.escape.escaping_count(), 0);
        assert_eq!(result.lifetime.count_registers(), 0);
    }
}
