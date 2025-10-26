//! Optimization Pass Tests
//!
//! Comprehensive tests for bytecode optimization passes including:
//! - Peephole optimization
//! - Dead code elimination
//! - Constant folding
//! - Register pressure reduction

use coral_codegen::bytecode::Instruction;
use coral_codegen::optimize::passes::OptimizationPass;

#[test]
fn test_peephole_optimization_removes_nops() {
    let mut instructions = vec![
        Instruction::Nop,
        Instruction::Nop,
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::Nop,
    ];

    let optimized = OptimizationPass::peephole_optimization(&mut instructions);

    assert!(optimized > 0, "Should optimize at least one Nop");
    let nop_count = instructions
        .iter()
        .filter(|i| matches!(i, Instruction::Nop))
        .count();
    assert!(nop_count < 4, "Should remove at least one Nop");
}

#[test]
fn test_peephole_optimization_with_moves() {
    let mut instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::Move { dst: 1, src: 0 },
        Instruction::Return { value: 1 },
    ];

    let optimized = OptimizationPass::peephole_optimization(&mut instructions);
    assert!(optimized > 0, "Should detect redundant moves");
}

#[test]
fn test_dead_code_elimination_basic() {
    let instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::LoadConst {
            dst: 1,
            const_id: 2,
        },
        Instruction::Return { value: 0 },
    ];

    let removed = OptimizationPass::dead_code_elimination(&mut instructions.clone());
    assert!(removed > 0, "Should detect unused register 1");
}

#[test]
fn test_dead_code_elimination_preserves_return() {
    let instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::LoadConst {
            dst: 1,
            const_id: 2,
        },
        Instruction::Return { value: 0 },
    ];

    let mut test_instrs = instructions.clone();
    OptimizationPass::dead_code_elimination(&mut test_instrs);

    assert!(
        test_instrs
            .iter()
            .any(|i| matches!(i, Instruction::Return { .. })),
        "Should preserve Return instruction"
    );
}

#[test]
fn test_constant_folding_basic() {
    let mut instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::LoadConst {
            dst: 1,
            const_id: 2,
        },
        Instruction::AddInt {
            dst: 2,
            lhs: 0,
            rhs: 1,
        },
    ];

    let folded = OptimizationPass::constant_folding(&mut instructions);
    assert!(folded > 0, "Should detect opportunity for constant folding");
}

#[test]
fn test_register_pressure_calculation() {
    let instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::LoadConst {
            dst: 1,
            const_id: 2,
        },
        Instruction::AddInt {
            dst: 2,
            lhs: 0,
            rhs: 1,
        },
        Instruction::Return { value: 2 },
    ];

    let pressure = OptimizationPass::register_pressure_reduction(&instructions);
    assert!(pressure >= 2, "Should calculate register pressure");
}

#[test]
fn test_optimization_pipeline_run_all() {
    let mut instructions = vec![
        Instruction::Nop,
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::Nop,
    ];

    let stats = OptimizationPass::run_all(&mut instructions);
    assert!(
        !instructions.is_empty(),
        "Should not remove all instructions"
    );
    assert!(
        stats.peephole_optimized > 0,
        "Pipeline should apply peephole optimization"
    );
}

#[test]
fn test_optimization_preserves_semantics() {
    let mut instructions = vec![
        Instruction::LoadConst {
            dst: 0,
            const_id: 42,
        },
        Instruction::LoadConst {
            dst: 1,
            const_id: 8,
        },
        Instruction::AddInt {
            dst: 2,
            lhs: 0,
            rhs: 1,
        },
        Instruction::Return { value: 2 },
    ];

    let original_len = instructions.len();
    let _stats = OptimizationPass::run_all(&mut instructions);

    assert!(
        instructions.len() <= original_len,
        "Optimization should not add instructions"
    );
    assert!(
        instructions
            .iter()
            .any(|i| matches!(i, Instruction::Return { .. })),
        "Optimization must preserve return"
    );
}

#[test]
fn test_optimization_mixed_instructions() {
    let mut instructions = vec![
        Instruction::Nop,
        Instruction::LoadConst {
            dst: 0,
            const_id: 1,
        },
        Instruction::Move { dst: 1, src: 0 },
        Instruction::Nop,
        Instruction::Return { value: 1 },
    ];

    let stats = OptimizationPass::run_all(&mut instructions);

    assert!(
        stats.peephole_optimized > 0,
        "Should optimize Nops and moves"
    );
    assert!(
        instructions.len() < 5,
        "Should reduce instruction count from optimization"
    );
}
