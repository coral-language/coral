//! Statement compilation to bytecode

use crate::bytecode::{Function, Instruction};
use crate::compiler::context::CompilationContext;
use crate::compiler::expr::ExprCompiler;
use crate::error::{CodegenError, CodegenResult};
use coral_parser::ast::Stmt;

pub struct StmtCompiler;

impl StmtCompiler {
    pub fn compile(ctx: &mut CompilationContext, stmt: &Stmt) -> CodegenResult<()> {
        match stmt {
            Stmt::Expr(s) => Self::compile_expr_stmt(ctx, &s.value),
            Stmt::Assign(s) => Self::compile_assign(ctx, s),
            Stmt::AnnAssign(s) => Self::compile_annassign(ctx, s),
            Stmt::AugAssign(s) => Self::compile_augassign(ctx, s),
            Stmt::Return(s) => Self::compile_return(ctx, s),
            Stmt::If(s) => Self::compile_if(ctx, s),
            Stmt::While(s) => Self::compile_while(ctx, s),
            Stmt::For(s) => Self::compile_for(ctx, s),
            Stmt::Pass(_s) => {
                ctx.emit(Instruction::Nop);
                Ok(())
            }
            Stmt::Break(_) => Err(CodegenError::CompilationError(
                "Break outside loop".to_string(),
            )),
            Stmt::Continue(_) => Err(CodegenError::CompilationError(
                "Continue outside loop".to_string(),
            )),
            Stmt::FuncDef(s) => Self::compile_funcdef(ctx, s),
            Stmt::ClassDef(_s) => {
                let compiled_class = crate::compiler::class::ClassCompiler::compile_class(ctx, _s)?;
                crate::compiler::class::ClassCompiler::emit_class_definition(ctx, &compiled_class);
                Ok(())
            }
            Stmt::Import(_s) => Err(CodegenError::CompilationError(
                "Import support coming in Phase 3".to_string(),
            )),
            Stmt::From(_s) => Err(CodegenError::CompilationError(
                "From import support coming in Phase 3".to_string(),
            )),
            Stmt::Export(_s) => Err(CodegenError::CompilationError(
                "Export support coming in Phase 3".to_string(),
            )),
            Stmt::Raise(s) => {
                crate::compiler::error_handling::ErrorHandlingCompiler::compile_raise(
                    ctx,
                    s.exc.as_ref(),
                )
            }
            Stmt::Try(s) => {
                crate::compiler::error_handling::ErrorHandlingCompiler::compile_try(ctx, s)
            }
            Stmt::Assert(s) => {
                crate::compiler::error_handling::ErrorHandlingCompiler::compile_assert(
                    ctx,
                    &s.test,
                    s.msg.as_ref(),
                )
            }
            Stmt::With(_s) => Err(CodegenError::CompilationError(
                "With statement support coming in Phase 3".to_string(),
            )),
            Stmt::Delete(_s) => Err(CodegenError::CompilationError(
                "Delete support coming in Phase 3".to_string(),
            )),
            Stmt::Global(_) => Ok(()),
            Stmt::Nonlocal(_) => Ok(()),
            Stmt::Match(_s) => crate::compiler::pattern::PatternCompiler::compile_match(ctx, _s),
            Stmt::Yield(_) => Err(CodegenError::AsyncError(
                "Yield statement coming in Phase 4".to_string(),
            )),
            Stmt::TypeAlias(_) => Ok(()),
        }
    }

    fn compile_expr_stmt(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::Expr,
    ) -> CodegenResult<()> {
        ExprCompiler::compile(ctx, expr)?;
        Ok(())
    }

    fn compile_assign(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::AssignStmt,
    ) -> CodegenResult<()> {
        let value_reg = ExprCompiler::compile(ctx, &stmt.value)?;

        for target_expr in stmt.targets {
            match target_expr {
                coral_parser::ast::Expr::Name(n) => {
                    ctx.define_local(n.id.to_string(), value_reg)?;
                }
                coral_parser::ast::Expr::Subscript(s) => {
                    let obj = ExprCompiler::compile(ctx, s.value)?;
                    let key = ExprCompiler::compile(ctx, s.slice)?;
                    ctx.emit(Instruction::SetItem {
                        obj,
                        key,
                        value: value_reg,
                    });
                }
                coral_parser::ast::Expr::Attribute(a) => {
                    let obj = ExprCompiler::compile(ctx, a.value)?;
                    let attr_id = ctx.add_constant_string(a.attr.to_string());
                    ctx.emit(Instruction::SetAttr {
                        obj,
                        attr_id,
                        value: value_reg,
                    });
                }
                _ => {
                    return Err(CodegenError::CompilationError(
                        "Invalid assignment target".to_string(),
                    ));
                }
            }
        }

        Ok(())
    }

    fn compile_annassign(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::AnnAssignStmt,
    ) -> CodegenResult<()> {
        if let Some(value_expr) = &stmt.value {
            let value_reg = ExprCompiler::compile(ctx, value_expr)?;

            if let coral_parser::ast::Expr::Name(n) = &stmt.target {
                ctx.define_local(n.id.to_string(), value_reg)?;
            }
        }

        Ok(())
    }

    fn compile_augassign(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::AugAssignStmt,
    ) -> CodegenResult<()> {
        let target_reg = ExprCompiler::compile(ctx, &stmt.target)?;
        let value_reg = ExprCompiler::compile(ctx, &stmt.value)?;
        let result = ctx.allocate_register(0)?;

        let instr = match stmt.op {
            "+=" => Instruction::AddInt {
                dst: result,
                lhs: target_reg,
                rhs: value_reg,
            },
            "-=" => Instruction::SubInt {
                dst: result,
                lhs: target_reg,
                rhs: value_reg,
            },
            "*=" => Instruction::MulInt {
                dst: result,
                lhs: target_reg,
                rhs: value_reg,
            },
            "/=" => Instruction::DivInt {
                dst: result,
                lhs: target_reg,
                rhs: value_reg,
            },
            "%=" => Instruction::ModInt {
                dst: result,
                lhs: target_reg,
                rhs: value_reg,
            },
            _ => {
                return Err(CodegenError::CompilationError(format!(
                    "Unknown augmented assignment operator: {}",
                    stmt.op
                )));
            }
        };

        ctx.emit(instr);

        if let coral_parser::ast::Expr::Name(n) = &stmt.target {
            ctx.define_local(n.id.to_string(), result)?;
        }

        Ok(())
    }

    fn compile_return(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::ReturnStmt,
    ) -> CodegenResult<()> {
        let value_reg = if let Some(value_expr) = &stmt.value {
            ExprCompiler::compile(ctx, value_expr)?
        } else {
            ctx.add_constant_none();
            ctx.allocate_register(0)?
        };

        ctx.emit(Instruction::Return { value: value_reg });
        Ok(())
    }

    fn compile_if(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::IfStmt,
    ) -> CodegenResult<()> {
        let test_reg = ExprCompiler::compile(ctx, &stmt.test)?;

        let if_false_offset = ctx.current_instruction_offset();
        ctx.emit(Instruction::JumpIfFalse {
            condition: test_reg,
            offset: 0,
        });

        for body_stmt in stmt.body {
            Self::compile(ctx, body_stmt)?;
        }

        let exit_offset = ctx.current_instruction_offset();
        ctx.emit(Instruction::Jump { offset: 0 });

        let else_start = ctx.current_instruction_offset();
        if let Instruction::JumpIfFalse { offset, .. } =
            &mut ctx.instructions[if_false_offset as usize]
        {
            *offset = else_start;
        }

        for orelse_stmt in stmt.orelse {
            Self::compile(ctx, orelse_stmt)?;
        }

        let end = ctx.current_instruction_offset();
        if let Instruction::Jump { offset } = &mut ctx.instructions[exit_offset as usize] {
            *offset = end;
        }

        Ok(())
    }

    fn compile_while(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::WhileStmt,
    ) -> CodegenResult<()> {
        let loop_start = ctx.current_instruction_offset();

        let test_reg = ExprCompiler::compile(ctx, &stmt.test)?;

        let exit_offset = ctx.current_instruction_offset();
        ctx.emit(Instruction::JumpIfFalse {
            condition: test_reg,
            offset: 0,
        });

        for body_stmt in stmt.body {
            Self::compile(ctx, body_stmt)?;
        }

        ctx.emit(Instruction::Jump { offset: loop_start });

        let exit_target = ctx.current_instruction_offset();
        if let Instruction::JumpIfFalse { offset, .. } = &mut ctx.instructions[exit_offset as usize]
        {
            *offset = exit_target;
        }

        Ok(())
    }

    fn compile_for(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::ForStmt,
    ) -> CodegenResult<()> {
        let _iter_reg = ExprCompiler::compile(ctx, &stmt.iter)?;

        let loop_start = ctx.current_instruction_offset();

        for body_stmt in stmt.body {
            Self::compile(ctx, body_stmt)?;
        }

        ctx.emit(Instruction::Jump { offset: loop_start });

        Ok(())
    }

    fn compile_funcdef(
        ctx: &mut CompilationContext,
        stmt: &coral_parser::ast::FuncDefStmt,
    ) -> CodegenResult<()> {
        let mut func = if stmt.is_async {
            Function::new_async(
                stmt.name.to_string(),
                256,
                stmt.args
                    .args
                    .iter()
                    .map(|arg| (arg.arg.to_string(), 0))
                    .collect(),
                0,
                1,
            )
        } else {
            Function::new(
                stmt.name.to_string(),
                256,
                stmt.args
                    .args
                    .iter()
                    .map(|arg| (arg.arg.to_string(), 0))
                    .collect(),
                0,
            )
        };

        let mut func_ctx = CompilationContext::new(256);

        for body_stmt in stmt.body {
            Self::compile(&mut func_ctx, body_stmt)?;
        }

        func.instructions = func_ctx.instructions;
        func.num_registers = func_ctx.register_allocator.count();

        ctx.emit(Instruction::Nop);

        Ok(())
    }
}
