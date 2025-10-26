//! Standard library integration - intrinsics and native modules

pub mod intrinsics;
pub mod native;

pub use intrinsics::IntrinsicRegistry;
pub use native::NativeModuleRegistry;
