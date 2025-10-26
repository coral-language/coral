//! Error handling compilation - try/except and exception table generation

use crate::bytecode::Instruction;
use crate::bytecode::function::ExceptionHandler;
use crate::compiler::context::CompilationContext;
use crate::error::CodegenResult;
use coral_parser::ast::{Expr, TryStmt};

#[derive(Debug, Clone)]
pub struct ExceptionFrame {
    pub try_start: u32,
    pub try_end: u32,
    pub handler_offset: u32,
    pub exception_type: u32,
}

pub struct ErrorHandlingCompiler;

impl ErrorHandlingCompiler {
    pub fn compile_try(ctx: &mut CompilationContext, stmt: &TryStmt) -> CodegenResult<()> {
        let try_start = ctx.current_instruction_offset();

        ctx.emit(Instruction::BeginTry { handler_offset: 0 });

        for body_stmt in stmt.body {
            crate::compiler::stmt::StmtCompiler::compile(ctx, body_stmt)?;
        }

        let try_end = ctx.current_instruction_offset();
        ctx.emit(Instruction::EndTry);

        let mut handlers = Vec::new();

        for except_handler in stmt.handlers {
            let handler_offset = ctx.current_instruction_offset();

            if let Some(exc_type) = &except_handler.typ {
                let _exc_reg = crate::compiler::expr::ExprCompiler::compile(ctx, exc_type)?;
            }

            if let Some(exc_name) = except_handler.name {
                let exc_reg = ctx.allocate_register(0)?;
                ctx.define_local(exc_name.to_string(), exc_reg)?;
            }

            for handler_stmt in except_handler.body {
                crate::compiler::stmt::StmtCompiler::compile(ctx, handler_stmt)?;
            }

            handlers.push(ExceptionHandler {
                start_offset: try_start,
                end_offset: try_end,
                handler_offset,
                exception_type: 0,
            });
        }

        for else_stmt in stmt.orelse {
            crate::compiler::stmt::StmtCompiler::compile(ctx, else_stmt)?;
        }

        for finally_stmt in stmt.finalbody {
            crate::compiler::stmt::StmtCompiler::compile(ctx, finally_stmt)?;
        }

        Ok(())
    }

    pub fn compile_raise(
        ctx: &mut CompilationContext,
        exception_expr: Option<&Expr>,
    ) -> CodegenResult<()> {
        if let Some(exc) = exception_expr {
            let exc_reg = crate::compiler::expr::ExprCompiler::compile(ctx, exc)?;
            ctx.emit(Instruction::Raise { exception: exc_reg });
        } else {
            let none_id = ctx.add_constant_none();
            let exc_reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: exc_reg,
                const_id: none_id,
            });
            ctx.emit(Instruction::Raise { exception: exc_reg });
        }

        Ok(())
    }

    pub fn compile_assert(
        ctx: &mut CompilationContext,
        test_expr: &Expr,
        msg_expr: Option<&Expr>,
    ) -> CodegenResult<()> {
        let test_reg = crate::compiler::expr::ExprCompiler::compile(ctx, test_expr)?;

        let skip_raise = ctx.current_instruction_offset();
        ctx.emit(Instruction::JumpIfTrue {
            condition: test_reg,
            offset: 0,
        });

        if let Some(msg) = msg_expr {
            let _msg_reg = crate::compiler::expr::ExprCompiler::compile(ctx, msg)?;
        } else {
            let assert_msg = ctx.add_constant_string("Assertion failed".to_string());
            let _msg_reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: _msg_reg,
                const_id: assert_msg,
            });
        }

        let exc_msg_id = ctx.add_constant_string("AssertionError".to_string());
        let exc_reg = ctx.allocate_register(0)?;
        ctx.emit(Instruction::LoadConst {
            dst: exc_reg,
            const_id: exc_msg_id,
        });
        ctx.emit(Instruction::Raise { exception: exc_reg });

        let end_offset = ctx.current_instruction_offset();
        if let Instruction::JumpIfTrue { offset, .. } = &mut ctx.instructions[skip_raise as usize] {
            *offset = end_offset;
        }

        Ok(())
    }

    pub fn add_exception_handler(
        handlers: &mut Vec<ExceptionHandler>,
        start_offset: u32,
        end_offset: u32,
        handler_offset: u32,
        exception_type: u32,
    ) {
        handlers.push(ExceptionHandler {
            start_offset,
            end_offset,
            handler_offset,
            exception_type,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exception_handler_creation() {
        let mut handlers = Vec::new();
        ErrorHandlingCompiler::add_exception_handler(&mut handlers, 0, 10, 20, 1);

        assert_eq!(handlers.len(), 1);
        assert_eq!(handlers[0].start_offset, 0);
        assert_eq!(handlers[0].end_offset, 10);
        assert_eq!(handlers[0].handler_offset, 20);
    }

    #[test]
    fn test_multiple_exception_handlers() {
        let mut handlers = Vec::new();
        ErrorHandlingCompiler::add_exception_handler(&mut handlers, 0, 10, 20, 1);
        ErrorHandlingCompiler::add_exception_handler(&mut handlers, 10, 20, 30, 2);
        ErrorHandlingCompiler::add_exception_handler(&mut handlers, 20, 30, 40, 3);

        assert_eq!(handlers.len(), 3);
        assert_eq!(handlers[1].start_offset, 10);
        assert_eq!(handlers[2].handler_offset, 40);
    }
}
