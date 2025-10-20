// Type system implementation

pub mod context;
pub mod display;
pub mod generics;
pub mod unification;

pub use context::{Type, TypeId};
pub use display::pretty_print_type;
pub use generics::GenericContext;
pub use unification::{UnificationResult, unify};
