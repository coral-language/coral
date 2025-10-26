//! Module linker - resolves imports, validates exports, and links bytecode modules

pub mod loader;
pub mod resolver;

pub use loader::ModuleLoader;
pub use resolver::ImportResolver;
