//! Class compilation with method dispatch and vtables

use crate::bytecode::{Function, Instruction};
use crate::compiler::context::CompilationContext;
use crate::compiler::function::FunctionCompiler;
use crate::error::CodegenResult;
use coral_parser::ast::{ClassDefStmt, Stmt};

pub struct ClassCompiler;

#[derive(Debug, Clone)]
pub struct CompiledClass {
    pub name: String,
    pub methods: Vec<Function>,
    pub is_protocol: bool,
}

impl ClassCompiler {
    pub fn compile_class(
        ctx: &mut CompilationContext,
        class_def: &ClassDefStmt,
    ) -> CodegenResult<CompiledClass> {
        let mut methods = Vec::new();

        for stmt in class_def.body {
            if let Stmt::FuncDef(func_def) = stmt {
                let func = FunctionCompiler::compile_function(ctx, func_def)?;
                methods.push(func);
            }
        }

        Ok(CompiledClass {
            name: class_def.name.to_string(),
            methods,
            is_protocol: class_def.is_protocol,
        })
    }

    pub fn emit_class_definition(ctx: &mut CompilationContext, compiled_class: &CompiledClass) {
        let _class_id = ctx.add_constant_string(compiled_class.name.clone());

        ctx.emit(Instruction::Nop);
    }
}
