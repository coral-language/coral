//! Operators.

use std::str::FromStr;

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,      // +
    Sub,      // -
    Mult,     // *
    MatMult,  // @
    Div,      // /
    FloorDiv, // //
    Mod,      // %
    Pow,      // **
    LShift,   // <<
    RShift,   // >>
    BitOr,    // |
    BitXor,   // ^
    BitAnd,   // &
}

impl FromStr for BinaryOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(BinaryOp::Add),
            "-" => Ok(BinaryOp::Sub),
            "*" => Ok(BinaryOp::Mult),
            "@" => Ok(BinaryOp::MatMult),
            "/" => Ok(BinaryOp::Div),
            "//" => Ok(BinaryOp::FloorDiv),
            "%" => Ok(BinaryOp::Mod),
            "**" => Ok(BinaryOp::Pow),
            "<<" => Ok(BinaryOp::LShift),
            ">>" => Ok(BinaryOp::RShift),
            "|" => Ok(BinaryOp::BitOr),
            "^" => Ok(BinaryOp::BitXor),
            "&" => Ok(BinaryOp::BitAnd),
            _ => Err(()),
        }
    }
}

impl BinaryOp {
    /// Convert to string representation.
    pub fn as_str(self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mult => "*",
            BinaryOp::MatMult => "@",
            BinaryOp::Div => "/",
            BinaryOp::FloorDiv => "//",
            BinaryOp::Mod => "%",
            BinaryOp::Pow => "**",
            BinaryOp::LShift => "<<",
            BinaryOp::RShift => ">>",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::BitAnd => "&",
        }
    }
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Invert, // ~
    Not,    // not
    UAdd,   // +
    USub,   // -
}

impl UnaryOp {
    /// Parse from string representation.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "~" => Some(UnaryOp::Invert),
            "not" => Some(UnaryOp::Not),
            "+" => Some(UnaryOp::UAdd),
            "-" => Some(UnaryOp::USub),
            _ => None,
        }
    }

    /// Convert to string representation.
    pub fn as_str(self) -> &'static str {
        match self {
            UnaryOp::Invert => "~",
            UnaryOp::Not => "not",
            UnaryOp::UAdd => "+",
            UnaryOp::USub => "-",
        }
    }
}

/// Comparison operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComparisonOp {
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    LtE,   // <=
    Gt,    // >
    GtE,   // >=
    Is,    // is
    IsNot, // is not
    In,    // in
    NotIn, // not in
}

impl ComparisonOp {
    /// Parse from string representation.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "==" => Some(ComparisonOp::Eq),
            "!=" => Some(ComparisonOp::NotEq),
            "<" => Some(ComparisonOp::Lt),
            "<=" => Some(ComparisonOp::LtE),
            ">" => Some(ComparisonOp::Gt),
            ">=" => Some(ComparisonOp::GtE),
            "is" => Some(ComparisonOp::Is),
            "is not" => Some(ComparisonOp::IsNot),
            "in" => Some(ComparisonOp::In),
            "not in" => Some(ComparisonOp::NotIn),
            _ => None,
        }
    }

    /// Convert to string representation.
    pub fn as_str(self) -> &'static str {
        match self {
            ComparisonOp::Eq => "==",
            ComparisonOp::NotEq => "!=",
            ComparisonOp::Lt => "<",
            ComparisonOp::LtE => "<=",
            ComparisonOp::Gt => ">",
            ComparisonOp::GtE => ">=",
            ComparisonOp::Is => "is",
            ComparisonOp::IsNot => "is not",
            ComparisonOp::In => "in",
            ComparisonOp::NotIn => "not in",
        }
    }
}

/// Boolean operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    And, // and
    Or,  // or
}

impl BoolOp {
    /// Parse from string representation.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "and" => Some(BoolOp::And),
            "or" => Some(BoolOp::Or),
            _ => None,
        }
    }

    /// Convert to string representation.
    pub fn as_str(self) -> &'static str {
        match self {
            BoolOp::And => "and",
            BoolOp::Or => "or",
        }
    }
}

/// Augmented assignment operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AugOp {
    Add,      // +=
    Sub,      // -=
    Mult,     // *=
    MatMult,  // @=
    Div,      // /=
    FloorDiv, // //=
    Mod,      // %=
    Pow,      // **=
    LShift,   // <<=
    RShift,   // >>=
    BitOr,    // |=
    BitXor,   // ^=
    BitAnd,   // &=
}

impl AugOp {
    /// Parse from string representation.
    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "+=" => Some(AugOp::Add),
            "-=" => Some(AugOp::Sub),
            "*=" => Some(AugOp::Mult),
            "@=" => Some(AugOp::MatMult),
            "/=" => Some(AugOp::Div),
            "//=" => Some(AugOp::FloorDiv),
            "%=" => Some(AugOp::Mod),
            "**=" => Some(AugOp::Pow),
            "<<=" => Some(AugOp::LShift),
            ">>=" => Some(AugOp::RShift),
            "|=" => Some(AugOp::BitOr),
            "^=" => Some(AugOp::BitXor),
            "&=" => Some(AugOp::BitAnd),
            _ => None,
        }
    }

    /// Convert to string representation.
    pub fn as_str(self) -> &'static str {
        match self {
            AugOp::Add => "+=",
            AugOp::Sub => "-=",
            AugOp::Mult => "*=",
            AugOp::MatMult => "@=",
            AugOp::Div => "/=",
            AugOp::FloorDiv => "//=",
            AugOp::Mod => "%=",
            AugOp::Pow => "**=",
            AugOp::LShift => "<<=",
            AugOp::RShift => ">>=",
            AugOp::BitOr => "|=",
            AugOp::BitXor => "^=",
            AugOp::BitAnd => "&=",
        }
    }
}
