//! Bytecode module root
//! Re-exports all bytecode-related types and modules

pub mod constant_pool;
pub mod function;
pub mod instruction;
pub mod module;
pub mod opcode;
pub mod register;

pub use constant_pool::ConstantPool;
pub use function::Function;
pub use instruction::Instruction;
pub use module::{BytecodeModule, ExportEntry, ExportKind, ExportTable, ImportEntry, ImportTable};
pub use opcode::Opcode;
pub use register::{RegisterAllocator, TypedRegister};
