//! # Coral Code Generation
//!
//! This crate provides code generation backends for the Coral programming language.
//! It includes both bytecode generation for the interpreter and LLVM backend for
//! native code compilation.
//!
//! ## Backends
//!
//! - **Bytecode**: Generates bytecode for the Coral virtual machine
//! - **LLVM**: Generates optimized native code using LLVM
//!
//! ## Architecture
//!
//! The code generation process:
//! 1. Takes an AST from the semantic analysis phase
//! 2. Performs backend-specific optimizations
//! 3. Generates target-specific code (bytecode or native)
//! 4. Returns executable code or bytecode instructions
//!
//! ## Usage
//!
//! ```rust,ignore
//! use coral_codegen::{BytecodeGenerator, LLVMBackend};
//! use coral_ast::Ast;
//! use bumpalo::Bump;
//!
//! // Create a simple AST
//! let arena = Bump::new();
//! let ast = Ast {
//!   body: vec![],
//! };
//!
//! // Generate bytecode
//! let bytecode_gen = BytecodeGenerator::new();
//! let bytecode = bytecode_gen.generate(&ast);
//!
//! // Generate native code
//! let llvm_backend = LLVMBackend::new();
//! let native_code = llvm_backend.generate(&ast);
//! ```

pub mod bytecode;
pub mod llvm_backend;

pub use bytecode::BytecodeGenerator;
pub use llvm_backend::LLVMBackend;
