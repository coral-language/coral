//! # Coral Code Generation
//!
//! This crate provides code generation for the Coral programming language.
//! It transforms the HIR (High-Level Intermediate Representation) from semantic analysis
//! into bytecode suitable for interpretation or further compilation.
//!
//! ## Architecture
//!
//! The code generation pipeline:
//! 1. **Input**: ParseResult from semantic analysis with HIR, type context, and symbol table
//! 2. **Lowering**: HIR → intermediate representation with control flow analysis
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
//! let source = "x = 42; print(x)";
//! match parse(source) {
//!     Ok(parse_result) => {
//!         let bytecode = compile(&parse_result)?;
//!     }
//!     Err(e) => eprintln!("Parse error: {}", e),
//! }
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
// pub use compiler::module::ModuleCompiler;
pub use error::{CodegenError, CodegenResult};
pub use linker::{ImportResolver, ModuleLoader};
pub use stdlib::{IntrinsicRegistry, NativeModuleRegistry};

/// Compile a Coral module to bytecode using the ParseResult with full semantic information
///
/// # Arguments
/// * `parse_result` - The complete parse result from parser containing AST, HIR, type context, and symbol table
///
/// # Returns
/// A BytecodeModule ready for execution
///
/// # Errors
/// Returns a CodegenError if bytecode generation fails
pub fn compile(
    _parse_result: &coral_parser::ParseResultWithMetadata,
) -> CodegenResult<BytecodeModule> {
    // ModuleCompiler::compile(parse_result)
    Err(CodegenError::CompilationError(
        "ModuleCompiler not yet implemented".to_string(),
    ))
}
