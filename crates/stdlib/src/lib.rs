pub mod value;
pub mod utils;
pub mod builtins;
pub mod types;
pub mod modules;

pub use value::Value;
pub use utils::StdlibError;
pub use builtins::BuiltinRegistry;

pub mod prelude {
    pub use crate::value::Value;
    pub use crate::utils::StdlibError;
    pub use crate::builtins::BuiltinRegistry;
    pub use crate::builtins::functions::*;
    pub use crate::builtins::globals::*;
}
