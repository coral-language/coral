//! Module compilation

use crate::bytecode::BytecodeModule;
use coral_parser::ast::Module;
use crate::error::CodegenResult;

pub struct ModuleCompiler;

impl ModuleCompiler {
    pub fn compile(_module: &Module) -> CodegenResult<BytecodeModule> {
        unimplemented!("Module compilation")
    }
}
