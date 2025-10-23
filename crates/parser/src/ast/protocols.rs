//! Protocol and special method definitions.

/// Common protocols and special methods.
pub struct Protocols;

impl Protocols {
    /// Constructor method (uses keyword, not @operator)
    pub const CONSTRUCTOR: &'static str = "constructor";

    /// Iterator protocol methods (use @operator decorator)
    pub const ITER: &'static str = "iter";
    pub const NEXT: &'static str = "next";

    /// Context manager protocol methods (use @operator decorator)
    pub const ENTER: &'static str = "enter";
    pub const EXIT: &'static str = "exit";
    pub const AENTER: &'static str = "aenter";
    pub const AEXIT: &'static str = "aexit";

    /// Descriptor protocol methods (use @operator decorator)
    pub const GET: &'static str = "get";
    pub const SET: &'static str = "set";
    pub const DELETE: &'static str = "delete";

    /// Callable protocol (use @operator decorator)
    pub const CALL: &'static str = "call";

    /// Container protocol methods (use @operator decorator)
    pub const LEN: &'static str = "len";
    pub const GETITEM: &'static str = "getitem";
    pub const SETITEM: &'static str = "setitem";
    pub const DELITEM: &'static str = "delitem";
    pub const CONTAINS: &'static str = "contains";

    /// Comparison protocol methods (use @operator decorator)
    pub const EQUALS: &'static str = "equals";
    pub const NOT_EQUALS: &'static str = "not_equals";
    pub const LESS_THAN: &'static str = "less_than";
    pub const LESS_EQUAL: &'static str = "less_equal";
    pub const GREATER_THAN: &'static str = "greater_than";
    pub const GREATER_EQUAL: &'static str = "greater_equal";

    /// Arithmetic protocol methods (use @operator decorator)
    pub const ADD: &'static str = "add";
    pub const SUBTRACT: &'static str = "subtract";
    pub const MULTIPLY: &'static str = "multiply";
    pub const TRUEDIV: &'static str = "truediv";
    pub const FLOORDIV: &'static str = "floordiv";
    pub const MOD: &'static str = "mod";
    pub const POW: &'static str = "pow";

    /// Reflected arithmetic protocol methods (use @operator decorator)
    pub const RADD: &'static str = "radd";
    pub const RSUB: &'static str = "rsub";
    pub const RMUL: &'static str = "rmul";

    /// Unary protocol methods (use @operator decorator)
    pub const NEG: &'static str = "neg";
    pub const POS: &'static str = "pos";
    pub const INVERT: &'static str = "invert";

    /// String representation methods (use @operator decorator)
    pub const STR: &'static str = "str";
    pub const REPR: &'static str = "repr";
    pub const FORMAT: &'static str = "format";

    /// Object lifecycle methods - del is rarely used, NEW not supported
    pub const DEL: &'static str = "del";

    /// Attribute access methods (use @operator decorator)
    pub const GETATTR: &'static str = "getattr";
    pub const SETATTR: &'static str = "setattr";
    pub const DELATTR: &'static str = "delattr";
    pub const GETATTRIBUTE: &'static str = "getattribute";

    /// Check if a name is a special method that requires @operator decorator.
    /// Constructor is special - it's a keyword, not a decorated method.
    pub fn is_special_method(name: &str) -> bool {
        matches!(
            name,
            "iter"
                | "next"
                | "enter"
                | "exit"
                | "aenter"
                | "aexit"
                | "get"
                | "set"
                | "delete"
                | "call"
                | "len"
                | "getitem"
                | "setitem"
                | "delitem"
                | "contains"
                | "equals"
                | "not_equals"
                | "less_than"
                | "less_equal"
                | "greater_than"
                | "greater_equal"
                | "add"
                | "subtract"
                | "multiply"
                | "truediv"
                | "floordiv"
                | "mod"
                | "pow"
                | "radd"
                | "rsub"
                | "rmul"
                | "neg"
                | "pos"
                | "invert"
                | "str"
                | "repr"
                | "format"
                | "getattr"
                | "setattr"
                | "delattr"
                | "getattribute"
        )
    }

    /// Check if a name is a private method.
    pub fn is_private_method(name: &str) -> bool {
        name.starts_with('_') && !Self::is_special_method(name) && name != "constructor"
    }

    /// Check if a name is a public method.
    pub fn is_public_method(name: &str) -> bool {
        !name.starts_with('_') || name == "constructor" || Self::is_special_method(name)
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
