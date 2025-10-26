//! # Coral Code Generation
//!
//! This crate provides code generation for the Coral programming language.
//! It transforms the AST into bytecode suitable for interpretation or further compilation.
//!
//! ## Architecture
//!
//! The code generation pipeline:
//! 1. **Input**: AST from semantic analysis (PassManager output)
//! 2. **Lowering**: AST → intermediate representation with control flow analysis
//! 3. **Compilation**: IR → bytecode instructions with register allocation
//! 4. **Optimization**: Dead code elimination, constant folding, peephole optimization
//! 5. **Linking**: Module resolution and import validation
//! 6. **Output**: BytecodeModule ready for execution or LLVM backend
//!
//! ## Usage
//!
//! ```rust,ignore
//! use coral_codegen::compile;
//! use coral_parser::parse;
//!
//! let source = "let x = 42; print(x)";
//! let parse_result = parse(source);
//! let bytecode = compile(&parse_result.ast)?;
//! ```

pub mod analysis;
pub mod bytecode;
pub mod compiler;
pub mod error;
pub mod linker;
pub mod lowering;
pub mod optimize;
pub mod stdlib;

pub use bytecode::{BytecodeModule, Function, Instruction, Opcode};
pub use compiler::module::ModuleCompiler;
pub use error::{CodegenError, CodegenResult};
pub use stdlib::{IntrinsicRegistry, NativeModuleRegistry};

/// Compile an AST module to bytecode
///
/// # Arguments
/// * `module` - The AST module from parser
///
/// # Returns
/// A BytecodeModule ready for execution
pub fn compile(module: &coral_parser::ast::Module) -> CodegenResult<BytecodeModule> {
    ModuleCompiler::compile(module)
}
