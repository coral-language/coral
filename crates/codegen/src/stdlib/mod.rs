//! Standard library integration

pub mod intrinsics;
pub mod native;
pub mod prelude;

pub use intrinsics::IntrinsicRegistry;
pub use native::NativeModuleRegistry;
pub use prelude::PRELUDE_SYMBOLS;
