//! Async/await state machine generation

use crate::bytecode::Instruction;
use crate::compiler::context::CompilationContext;
use crate::error::CodegenResult;
use coral_parser::ast::{Expr, FuncDefStmt};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StateId(pub u32);

#[derive(Debug, Clone)]
pub struct AsyncStateMachine {
    pub num_states: u32,
    pub state_transitions: HashMap<StateId, StateTransition>,
    pub await_points: Vec<AwaitPoint>,
}

#[derive(Debug, Clone)]
pub struct StateTransition {
    pub from_state: StateId,
    pub to_state: StateId,
    pub await_expr_reg: u16,
    pub result_reg: u16,
}

#[derive(Debug, Clone)]
pub struct AwaitPoint {
    pub state_id: StateId,
    pub instruction_offset: u32,
    pub result_reg: u16,
    pub future_reg: u16,
}

pub struct AsyncLowering {
    next_state_id: u32,
    await_points: Vec<AwaitPoint>,
    state_transitions: HashMap<StateId, StateTransition>,
}

impl Default for AsyncLowering {
    fn default() -> Self {
        Self::new()
    }
}

impl AsyncLowering {
    pub fn new() -> Self {
        Self {
            next_state_id: 0,
            await_points: Vec::new(),
            state_transitions: HashMap::new(),
        }
    }

    pub fn allocate_state(&mut self) -> StateId {
        let id = StateId(self.next_state_id);
        self.next_state_id += 1;
        id
    }

    pub fn register_await_point(
        &mut self,
        state_id: StateId,
        instruction_offset: u32,
        result_reg: u16,
        future_reg: u16,
    ) {
        self.await_points.push(AwaitPoint {
            state_id,
            instruction_offset,
            result_reg,
            future_reg,
        });
    }

    pub fn finalize(self) -> AsyncStateMachine {
        AsyncStateMachine {
            num_states: self.next_state_id,
            state_transitions: self.state_transitions,
            await_points: self.await_points,
        }
    }
}

pub struct AsyncCompiler;

impl AsyncCompiler {
    pub fn compile_async_function(
        _ctx: &mut CompilationContext,
        _func_def: &FuncDefStmt,
    ) -> CodegenResult<AsyncStateMachine> {
        let mut lowering = AsyncLowering::new();

        let initial_state = lowering.allocate_state();
        let _ = initial_state;

        Ok(lowering.finalize())
    }

    pub fn lower_await(
        lowering: &mut AsyncLowering,
        ctx: &mut CompilationContext,
        await_expr: &Expr,
    ) -> CodegenResult<u16> {
        let future_reg = crate::compiler::expr::ExprCompiler::compile(ctx, await_expr)?;

        let result_reg = ctx.allocate_register(0)?;
        let state_id = lowering.allocate_state();

        let instruction_offset = ctx.current_instruction_offset();
        lowering.register_await_point(state_id, instruction_offset, result_reg, future_reg);

        ctx.emit(Instruction::Await {
            result: result_reg,
            future: future_reg,
            state_id: state_id.0,
        });

        Ok(result_reg)
    }

    pub fn lower_yield(
        _lowering: &mut AsyncLowering,
        ctx: &mut CompilationContext,
        value_expr: Option<&Expr>,
    ) -> CodegenResult<u16> {
        let value_reg = if let Some(expr) = value_expr {
            crate::compiler::expr::ExprCompiler::compile(ctx, expr)?
        } else {
            let none_id = ctx.add_constant_none();
            let reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: reg,
                const_id: none_id,
            });
            reg
        };

        ctx.emit(Instruction::Yield { value: value_reg });

        Ok(value_reg)
    }

    pub fn validate_async_safety(
        _func_def: &FuncDefStmt,
        _has_await_points: bool,
    ) -> CodegenResult<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocate_state() {
        let mut lowering = AsyncLowering::new();
        let state1 = lowering.allocate_state();
        let state2 = lowering.allocate_state();

        assert_eq!(state1.0, 0);
        assert_eq!(state2.0, 1);
    }

    #[test]
    fn test_register_await_point() {
        let mut lowering = AsyncLowering::new();
        let state = lowering.allocate_state();

        lowering.register_await_point(state, 100, 5, 10);

        let machine = lowering.finalize();
        assert_eq!(machine.await_points.len(), 1);
        assert_eq!(machine.await_points[0].state_id, state);
        assert_eq!(machine.await_points[0].result_reg, 5);
    }

    #[test]
    fn test_finalize_state_machine() {
        let mut lowering = AsyncLowering::new();
        let _state1 = lowering.allocate_state();
        let _state2 = lowering.allocate_state();

        let machine = lowering.finalize();
        assert_eq!(machine.num_states, 2);
    }
}
