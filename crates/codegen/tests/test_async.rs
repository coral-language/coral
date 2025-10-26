use coral_codegen::bytecode::Instruction;
use coral_codegen::compiler::async_lowering::AsyncLowering;
use coral_codegen::compiler::context::CompilationContext;

#[test]
fn test_async_state_allocation() {
    let mut lowering = AsyncLowering::new();

    let state0 = lowering.allocate_state();
    let state1 = lowering.allocate_state();
    let state2 = lowering.allocate_state();

    assert_eq!(state0.0, 0);
    assert_eq!(state1.0, 1);
    assert_eq!(state2.0, 2);
}

#[test]
fn test_await_point_registration() {
    let mut lowering = AsyncLowering::new();
    let state0 = lowering.allocate_state();
    let state1 = lowering.allocate_state();

    lowering.register_await_point(state0, 10, 5, 8);
    lowering.register_await_point(state1, 25, 6, 9);

    let machine = lowering.finalize();

    assert_eq!(machine.await_points.len(), 2);
    assert_eq!(machine.num_states, 2);
    assert_eq!(machine.await_points[0].result_reg, 5);
    assert_eq!(machine.await_points[1].result_reg, 6);
}

#[test]
fn test_state_machine_finalization() {
    let mut lowering = AsyncLowering::new();
    let _state0 = lowering.allocate_state();
    let _state1 = lowering.allocate_state();
    let _state2 = lowering.allocate_state();

    let machine = lowering.finalize();

    assert_eq!(machine.num_states, 3);
    assert_eq!(machine.state_transitions.len(), 0);
}

#[test]
fn test_await_instruction_generation() {
    let mut ctx = CompilationContext::new(256);

    let const_id = ctx.add_constant_string("test".to_string());
    ctx.emit(Instruction::LoadConst { dst: 0, const_id });

    let initial_offset = ctx.current_instruction_offset();
    ctx.emit(Instruction::Await {
        result: 1,
        future: 0,
        state_id: 0,
    });

    let final_offset = ctx.current_instruction_offset();

    assert_eq!(initial_offset, 1);
    assert_eq!(final_offset, 2);
    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_yield_instruction_generation() {
    let mut ctx = CompilationContext::new(256);

    let int_id = ctx.add_constant_integer(42);
    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: int_id,
    });

    ctx.emit(Instruction::Yield { value: 0 });

    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_multiple_await_points() {
    let mut lowering = AsyncLowering::new();

    for i in 0..5 {
        let state = lowering.allocate_state();
        lowering.register_await_point(state, i * 10, i as u16, i as u16 + 1);
    }

    let machine = lowering.finalize();

    assert_eq!(machine.num_states, 5);
    assert_eq!(machine.await_points.len(), 5);

    for (i, point) in machine.await_points.iter().enumerate() {
        assert_eq!(point.state_id.0, i as u32);
        assert_eq!(point.result_reg, i as u16);
    }
}

#[test]
fn test_async_context_preservation() {
    let mut ctx = CompilationContext::new(512);

    let s1 = ctx.add_constant_string("str1".to_string());
    let i1 = ctx.add_constant_integer(100);
    let b1 = ctx.add_constant_bool(true);

    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: s1,
    });
    ctx.emit(Instruction::LoadConst {
        dst: 1,
        const_id: i1,
    });
    ctx.emit(Instruction::LoadConst {
        dst: 2,
        const_id: b1,
    });

    assert_eq!(ctx.instructions.len(), 3);
    assert_eq!(ctx.current_instruction_offset(), 3);
}

#[test]
fn test_state_machine_with_await_sequence() {
    let mut lowering = AsyncLowering::new();
    let mut await_points = Vec::new();

    for i in 0..3 {
        let state = lowering.allocate_state();
        let offset = i as u32 * 5;
        lowering.register_await_point(state, offset, i as u16 + 10, i as u16 + 20);
        await_points.push((state, offset));
    }

    let machine = lowering.finalize();

    assert_eq!(machine.num_states, 3);
    for (idx, point) in machine.await_points.iter().enumerate() {
        assert_eq!(point.state_id.0, idx as u32);
        assert_eq!(point.instruction_offset, idx as u32 * 5);
    }
}

#[test]
fn test_generator_expression_creation() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::NewGenerator {
        dst: 0,
        function_id: 1,
    });

    assert_eq!(ctx.instructions.len(), 1);
}

#[test]
fn test_yield_with_value() {
    let mut ctx = CompilationContext::new(256);

    let const_id = ctx.add_constant_integer(42);
    ctx.emit(Instruction::LoadConst { dst: 0, const_id });

    ctx.emit(Instruction::Yield { value: 0 });

    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_yield_from_expression() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::NewList {
        dst: 0,
        size_hint: 3,
    });

    ctx.emit(Instruction::YieldFrom { value: 0 });

    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_await_expression() {
    let mut ctx = CompilationContext::new(256);

    let future_const = ctx.add_constant_string("future".to_string());
    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: future_const,
    });

    ctx.emit(Instruction::Await {
        result: 1,
        future: 0,
        state_id: 0,
    });

    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_async_function_state_count() {
    let mut lowering = AsyncLowering::new();

    for i in 0..10 {
        let state = lowering.allocate_state();
        lowering.register_await_point(state, i as u32 * 5, i as u16, i as u16 + 1);
    }

    let machine = lowering.finalize();

    assert_eq!(machine.num_states, 10);
    assert_eq!(machine.await_points.len(), 10);
}

#[test]
fn test_mixed_sync_async_context() {
    let mut ctx = CompilationContext::new(256);

    let const_id = ctx.add_constant_integer(100);
    ctx.emit(Instruction::LoadConst { dst: 0, const_id });

    ctx.emit(Instruction::Await {
        result: 1,
        future: 0,
        state_id: 0,
    });

    ctx.emit(Instruction::AddInt {
        dst: 2,
        lhs: 1,
        rhs: 0,
    });

    assert_eq!(ctx.instructions.len(), 3);
}

#[test]
fn test_nested_generator_expressions() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::NewGenerator {
        dst: 0,
        function_id: 1,
    });

    ctx.emit(Instruction::NewGenerator {
        dst: 1,
        function_id: 2,
    });

    assert_eq!(ctx.instructions.len(), 2);
}

#[test]
fn test_yield_in_loop() {
    let mut ctx = CompilationContext::new(256);

    for i in 0..5 {
        let const_id = ctx.add_constant_integer(i as i64);
        ctx.emit(Instruction::LoadConst { dst: 0, const_id });
        ctx.emit(Instruction::Yield { value: 0 });
    }

    assert_eq!(ctx.instructions.len(), 10);
}

#[test]
fn test_async_error_propagation() {
    let mut ctx = CompilationContext::new(256);

    let future_id = ctx.add_constant_string("error_future".to_string());
    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: future_id,
    });

    ctx.emit(Instruction::Await {
        result: 1,
        future: 0,
        state_id: 1,
    });

    ctx.emit(Instruction::PropagateError { result: 1 });

    assert_eq!(ctx.instructions.len(), 3);
}

#[test]
fn test_complex_async_pattern() {
    let mut lowering = AsyncLowering::new();
    let _ctx = CompilationContext::new(512);

    let state0 = lowering.allocate_state();
    let state1 = lowering.allocate_state();
    let state2 = lowering.allocate_state();

    lowering.register_await_point(state0, 0, 0, 1);
    lowering.register_await_point(state1, 10, 2, 3);
    lowering.register_await_point(state2, 20, 4, 5);

    let machine = lowering.finalize();

    assert_eq!(machine.num_states, 3);
    assert_eq!(machine.await_points.len(), 3);

    for (i, point) in machine.await_points.iter().enumerate() {
        assert_eq!(point.state_id.0, i as u32);
        assert_eq!(point.instruction_offset, (i as u32) * 10);
    }
}
