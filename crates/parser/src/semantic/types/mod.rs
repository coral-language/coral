// Type system implementation

pub mod builtins;
pub mod context;
pub mod display;
pub mod generics;
pub mod guards;
pub mod narrowing;
pub mod unification;

pub use context::{AttributeKind, Type, TypeId};
pub use display::pretty_print_type;
pub use generics::GenericContext;
pub use guards::{TypeGuardRegistry, extract_type_guard};
pub use narrowing::{
    TypeNarrowingContext, merge_types, narrow_is_none, narrow_isinstance, narrow_truthiness,
};
pub use unification::{UnificationResult, unify};
