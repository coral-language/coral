//! Static operator strings to avoid repeated arena allocations.

// Binary operators
pub const OP_ADD: &str = "+";
pub const OP_SUB: &str = "-";
pub const OP_MULT: &str = "*";
pub const OP_DIV: &str = "/";
pub const OP_MOD: &str = "%";
pub const OP_POW: &str = "**";
pub const OP_FLOORDIV: &str = "//";
pub const OP_MATMULT: &str = "@";

// Bitwise operators
pub const OP_BITOR: &str = "|";
pub const OP_BITXOR: &str = "^";
pub const OP_BITAND: &str = "&";
pub const OP_LSHIFT: &str = "<<";
pub const OP_RSHIFT: &str = ">>";

// Boolean operators
pub const OP_AND: &str = "and";
pub const OP_OR: &str = "or";

// Unary operators
pub const OP_NOT: &str = "not";
pub const OP_UADD: &str = "+";
pub const OP_USUB: &str = "-";
pub const OP_INVERT: &str = "~";

// Comparison operators
pub const OP_EQ: &str = "==";
pub const OP_NE: &str = "!=";
pub const OP_LT: &str = "<";
pub const OP_LE: &str = "<=";
pub const OP_GT: &str = ">";
pub const OP_GE: &str = ">=";
pub const OP_IS: &str = "is";
pub const OP_ISNOT: &str = "is not";
pub const OP_IN: &str = "in";
pub const OP_NOTIN: &str = "not in";

// Constants
pub const CONST_TRUE: &str = "True";
pub const CONST_FALSE: &str = "False";
pub const CONST_NONE: &str = "None";
