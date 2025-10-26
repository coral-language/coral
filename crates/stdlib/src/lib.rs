pub mod builtins;
pub mod modules;
pub mod types;
pub mod utils;
pub mod value;

pub use builtins::BuiltinRegistry;
pub use utils::StdlibError;
pub use value::Value;

pub mod prelude {
    pub use crate::builtins::BuiltinRegistry;
    pub use crate::builtins::functions::*;
    pub use crate::builtins::globals::*;
    pub use crate::utils::StdlibError;
    pub use crate::value::Value;
}
