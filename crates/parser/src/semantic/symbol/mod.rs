pub mod scope;
pub mod table;

pub use scope::{BindingKind, Scope, ScopeType, Symbol};
pub use table::{ScopeMetadata, SymbolTable, SymbolTableSnapshot, SyncSymbolTable};
