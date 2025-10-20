//! Protocol and special method definitions.

/// Common protocols and special methods.
pub struct Protocols;

impl Protocols {
    /// Iterator protocol methods.
    pub const ITER: &'static str = "__iter__";
    pub const NEXT: &'static str = "__next__";

    /// Context manager protocol methods.
    pub const ENTER: &'static str = "__enter__";
    pub const EXIT: &'static str = "__exit__";
    pub const AENTER: &'static str = "__aenter__";
    pub const AEXIT: &'static str = "__aexit__";

    /// Descriptor protocol methods.
    pub const GET: &'static str = "__get__";
    pub const SET: &'static str = "__set__";
    pub const DELETE: &'static str = "__delete__";

    /// Callable protocol.
    pub const CALL: &'static str = "__call__";

    /// Container protocol methods.
    pub const LEN: &'static str = "__len__";
    pub const GETITEM: &'static str = "__getitem__";
    pub const SETITEM: &'static str = "__setitem__";
    pub const DELITEM: &'static str = "__delitem__";
    pub const CONTAINS: &'static str = "__contains__";

    /// Comparison protocol methods.
    pub const EQ: &'static str = "__eq__";
    pub const NE: &'static str = "__ne__";
    pub const LT: &'static str = "__lt__";
    pub const LE: &'static str = "__le__";
    pub const GT: &'static str = "__gt__";
    pub const GE: &'static str = "__ge__";

    /// Arithmetic protocol methods.
    pub const ADD: &'static str = "__add__";
    pub const SUB: &'static str = "__sub__";
    pub const MUL: &'static str = "__mul__";
    pub const TRUEDIV: &'static str = "__truediv__";
    pub const FLOORDIV: &'static str = "__floordiv__";
    pub const MOD: &'static str = "__mod__";
    pub const POW: &'static str = "__pow__";

    /// Reflected arithmetic protocol methods.
    pub const RADD: &'static str = "__radd__";
    pub const RSUB: &'static str = "__rsub__";
    pub const RMUL: &'static str = "__rmul__";

    /// Unary protocol methods.
    pub const NEG: &'static str = "__neg__";
    pub const POS: &'static str = "__pos__";
    pub const INVERT: &'static str = "__invert__";

    /// String representation methods.
    pub const STR: &'static str = "__str__";
    pub const REPR: &'static str = "__repr__";
    pub const FORMAT: &'static str = "__format__";

    /// Object lifecycle methods.
    pub const INIT: &'static str = "__init__";
    pub const NEW: &'static str = "__new__";
    pub const DEL: &'static str = "__del__";

    /// Attribute access methods.
    pub const GETATTR: &'static str = "__getattr__";
    pub const SETATTR: &'static str = "__setattr__";
    pub const DELATTR: &'static str = "__delattr__";
    pub const GETATTRIBUTE: &'static str = "__getattribute__";

    /// Check if a name is a special method.
    pub fn is_special_method(name: &str) -> bool {
        name.starts_with("__") && name.ends_with("__") && name.len() > 4
    }

    /// Check if a name is a private method.
    pub fn is_private_method(name: &str) -> bool {
        name.starts_with('_') && !Self::is_special_method(name)
    }

    /// Check if a name is a public method.
    pub fn is_public_method(name: &str) -> bool {
        !name.starts_with('_')
    }
}

/// Protocol implementation checker.
pub struct ProtocolChecker;

impl ProtocolChecker {
    /// Check if a class implements the iterator protocol.
    pub fn implements_iterator(methods: &[&str]) -> bool {
        methods.contains(&Protocols::ITER) && methods.contains(&Protocols::NEXT)
    }

    /// Check if a class implements the context manager protocol.
    pub fn implements_context_manager(methods: &[&str]) -> bool {
        methods.contains(&Protocols::ENTER) && methods.contains(&Protocols::EXIT)
    }

    /// Check if a class implements the async context manager protocol.
    pub fn implements_async_context_manager(methods: &[&str]) -> bool {
        methods.contains(&Protocols::AENTER) && methods.contains(&Protocols::AEXIT)
    }

    /// Check if a class implements the descriptor protocol.
    pub fn implements_descriptor(methods: &[&str]) -> bool {
        methods.contains(&Protocols::GET)
    }

    /// Check if a class is callable.
    pub fn is_callable(methods: &[&str]) -> bool {
        methods.contains(&Protocols::CALL)
    }

    /// Check if a class implements the container protocol.
    pub fn implements_container(methods: &[&str]) -> bool {
        methods.contains(&Protocols::GETITEM) || methods.contains(&Protocols::LEN)
    }
}
