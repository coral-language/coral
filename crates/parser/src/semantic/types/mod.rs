// Type system implementation

pub mod builtins;
pub mod context;
pub mod display;
pub mod generics;
pub mod unification;

pub use context::{AttributeKind, Type, TypeId};
pub use display::pretty_print_type;
pub use generics::GenericContext;
pub use unification::{UnificationResult, unify};
