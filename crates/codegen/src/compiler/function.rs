//! Function and closure compilation

use crate::bytecode::{Function, Instruction};
use crate::compiler::context::CompilationContext;
use crate::compiler::stmt::StmtCompiler;
use crate::error::CodegenResult;
use coral_parser::ast::FuncDefStmt;

pub struct FunctionCompiler;

impl FunctionCompiler {
    pub fn compile_function(
        _ctx: &mut CompilationContext,
        func_def: &FuncDefStmt,
    ) -> CodegenResult<Function> {
        let mut func_ctx = CompilationContext::new(256);

        func_ctx.push_scope();

        for arg in func_def.args.args {
            let reg = func_ctx.allocate_register(0)?;
            func_ctx.define_local(arg.arg.to_string(), reg)?;
        }

        if let Some(vararg) = &func_def.args.vararg {
            let reg = func_ctx.allocate_register(0)?;
            func_ctx.define_local(vararg.arg.to_string(), reg)?;
        }

        if let Some(kwarg) = &func_def.args.kwarg {
            let reg = func_ctx.allocate_register(0)?;
            func_ctx.define_local(kwarg.arg.to_string(), reg)?;
        }

        for stmt in func_def.body {
            StmtCompiler::compile(&mut func_ctx, stmt)?;
        }

        func_ctx.pop_scope();

        let mut func = if func_def.is_async {
            Function::new_async(
                func_def.name.to_string(),
                func_ctx.register_allocator.count(),
                func_def
                    .args
                    .args
                    .iter()
                    .map(|arg| (arg.arg.to_string(), 0))
                    .collect(),
                0,
                1,
            )
        } else {
            Function::new(
                func_def.name.to_string(),
                func_ctx.register_allocator.count(),
                func_def
                    .args
                    .args
                    .iter()
                    .map(|arg| (arg.arg.to_string(), 0))
                    .collect(),
                0,
            )
        };

        func.instructions = func_ctx.instructions;

        Ok(func)
    }

    pub fn compile_lambda(
        ctx: &mut CompilationContext,
        lambda_expr: &coral_parser::ast::LambdaExpr,
    ) -> CodegenResult<u16> {
        let mut func_ctx = CompilationContext::new(256);

        func_ctx.push_scope();

        for arg in lambda_expr.args.args {
            let reg = func_ctx.allocate_register(0)?;
            func_ctx.define_local(arg.arg.to_string(), reg)?;
        }

        let body_reg =
            crate::compiler::expr::ExprCompiler::compile(&mut func_ctx, lambda_expr.body)?;
        func_ctx.emit(Instruction::Return { value: body_reg });

        func_ctx.pop_scope();

        let mut func = Function::new(
            "<lambda>".to_string(),
            func_ctx.register_allocator.count(),
            lambda_expr
                .args
                .args
                .iter()
                .map(|arg| (arg.arg.to_string(), 0))
                .collect(),
            0,
        );

        func.instructions = func_ctx.instructions;

        let result_reg = ctx.allocate_register(0)?;
        ctx.emit(Instruction::Nop);

        Ok(result_reg)
    }
}
