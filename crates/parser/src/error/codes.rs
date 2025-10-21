//! Error codes and categories for Coral parsing.

/// Error severity levels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// Informational message
    Info,
    /// Warning that doesn't prevent parsing
    Warning,
    /// Error that prevents successful parsing
    Error,
    /// Fatal error that stops parsing immediately
    Fatal,
}

/// Error code categories.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // ===== Lexical Errors (E1xxx) =====
    E1001, // Invalid character
    E1002, // Unterminated string literal
    E1003, // Invalid number literal
    E1004, // Invalid identifier
    E1005, // Mixed tabs and spaces
    E1006, // Invalid escape sequence

    // ===== Syntax Errors (E2xxx) =====
    E2001, // Unexpected token
    E2002, // Expected token not found
    E2003, // Unexpected EOF
    E2004, // Invalid syntax
    E2005, // Indentation error
    E2006, // Unclosed delimiter
    E2007, // Unmatched closing delimiter
    E2008, // Missing colon
    E2009, // Expected expression
    E2010, // Invalid assignment target
    E2011, // Break outside loop
    E2012, // Continue outside loop
    E2013, // Return outside function
    E2014, // Yield outside function
    E2015, // Await outside async
    E2016, // Async for outside async
    E2017, // Async with outside async
    E2018, // Duplicate parameter
    E2019, // Duplicate argument
    E2020, // Positional after keyword
    E2021, // Invalid parameter order
    E2022, // Mixed except syntax
    E2023, // Bare except star
    E2024, // Future import not first
    E2025, // Relative import beyond top level
    E2026, // Invalid relative import
    E2027, // Unexpected indent
    E2028, // Expected indent
    E2029, // Unindent mismatch

    // ===== Name Resolution Errors (E3xxx) =====
    E3001, // Undefined name
    E3002, // Duplicate definition
    E3003, // Nonlocal without enclosing scope
    E3004, // Used before definition

    // ===== Control Flow Errors (E3100-E3199) =====
    W3100, // Unreachable code (warning)
    W3101, // Infinite loop detected (warning)
    E3102, // Missing return statement
    E3103, // Inconsistent return types
    E3104, // Dead code after return

    // ===== Type System Errors (E4xxx) =====
    E4001, // Type mismatch
    E4002, // Invalid binary operation
    E4003, // Invalid unary operation
    E4004, // Argument count mismatch
    E4005, // Argument type mismatch
    E4006, // Return type mismatch
    E4007, // Invalid subscript
    E4008, // Invalid attribute access
    E4009, // Not callable
    E4010, // Not subscriptable
    E4011, // Not iterable
    E4012, // Cannot infer type
    E4013, // Ambiguous type
    E4014, // Type annotation required
    E4015, // Invalid type annotation

    // ===== Protocol Errors (E4100-E4199) =====
    E4100, // Missing protocol method
    E4101, // Method signature mismatch
    E4102, // Protocol with implementation
    E4103, // Invalid protocol definition
    E4104, // Protocol not satisfied
    E4105, // Runtime checkable protocol violation

    // ===== Import Errors (E5xxx) =====
    E5001, // Module not found
    E5002, // Circular import
    E5003, // Invalid import syntax
    E5004, // Import from non-module
    E5005, // Cannot import name
    E5006, // Relative import in non-package

    // ===== Module System Errors (E5100-E5199) =====
    E5100, // Export undefined name
    E5101, // Duplicate export
    E5102, // Invalid module introspection
    E5103, // Export from non-existent module
    E5104, // Invalid export syntax

    // ===== Concurrency Errors (E6xxx) =====
    E6001, // Data race detected
    E6002, // Potential deadlock
    E6003, // Non-Send type across threads
    E6004, // Non-Sync type shared between threads
    E6005, // Lock not released
    E6006, // Unsynchronized access
    E6007, // Double lock acquisition
    E6008, // Lock order violation

    // ===== Memory Safety Errors (E7xxx) =====
    E7001, // Use after free
    E7002, // Double free
    E7003, // Resource leak
    E7004, // Dangling reference
    E7005, // Circular reference without weak link
    E7006, // Invalid lifetime
    E7007, // Moved value used

    // ===== Pattern Matching Errors (E8xxx) =====
    E8001, // Non-exhaustive match
    E8002, // Unreachable pattern
    E8003, // Pattern type mismatch
    E8004, // Invalid pattern syntax
    E8005, // Duplicate pattern binding

    // ===== Decorator Errors (E9xxx) =====
    E9001, // Decorator not found
    E9002, // Invalid decorator target
    E9003, // Decorator signature mismatch
    E9004, // Decorator application failed
    E9005, // Invalid decorator composition

    // ===== Deprecation Warnings (W2xxx) =====
    W2001, // Deprecated feature

    // ===== Code Quality Warnings (W3xxx) =====
    W3001, // Unused variable
    W3002, // Unused function
    W3003, // Unused import (currently used)
    W3004, // Unused parameter
    W3006, // Shadows builtin (currently used)
    W3007, // Shadows import (currently used)
}

impl ErrorCode {
    /// Get the numeric code.
    pub fn code(&self) -> u32 {
        match self {
            // Lexical
            ErrorCode::E1001 => 1001,
            ErrorCode::E1002 => 1002,
            ErrorCode::E1003 => 1003,
            ErrorCode::E1004 => 1004,
            ErrorCode::E1005 => 1005,
            ErrorCode::E1006 => 1006,

            // Syntax
            ErrorCode::E2001 => 2001,
            ErrorCode::E2002 => 2002,
            ErrorCode::E2003 => 2003,
            ErrorCode::E2004 => 2004,
            ErrorCode::E2005 => 2005,
            ErrorCode::E2006 => 2006,
            ErrorCode::E2007 => 2007,
            ErrorCode::E2008 => 2008,
            ErrorCode::E2009 => 2009,
            ErrorCode::E2010 => 2010,
            ErrorCode::E2011 => 2011,
            ErrorCode::E2012 => 2012,
            ErrorCode::E2013 => 2013,
            ErrorCode::E2014 => 2014,
            ErrorCode::E2015 => 2015,
            ErrorCode::E2016 => 2016,
            ErrorCode::E2017 => 2017,
            ErrorCode::E2018 => 2018,
            ErrorCode::E2019 => 2019,
            ErrorCode::E2020 => 2020,
            ErrorCode::E2021 => 2021,
            ErrorCode::E2022 => 2022,
            ErrorCode::E2023 => 2023,
            ErrorCode::E2024 => 2024,
            ErrorCode::E2025 => 2025,
            ErrorCode::E2026 => 2026,
            ErrorCode::E2027 => 2027,
            ErrorCode::E2028 => 2028,
            ErrorCode::E2029 => 2029,

            // Name Resolution
            ErrorCode::E3001 => 3001,
            ErrorCode::E3002 => 3002,
            ErrorCode::E3003 => 3003,
            ErrorCode::E3004 => 3004,

            // Control Flow
            ErrorCode::W3100 => 3100,
            ErrorCode::W3101 => 3101,
            ErrorCode::E3102 => 3102,
            ErrorCode::E3103 => 3103,
            ErrorCode::E3104 => 3104,

            // Type System
            ErrorCode::E4001 => 4001,
            ErrorCode::E4002 => 4002,
            ErrorCode::E4003 => 4003,
            ErrorCode::E4004 => 4004,
            ErrorCode::E4005 => 4005,
            ErrorCode::E4006 => 4006,
            ErrorCode::E4007 => 4007,
            ErrorCode::E4008 => 4008,
            ErrorCode::E4009 => 4009,
            ErrorCode::E4010 => 4010,
            ErrorCode::E4011 => 4011,
            ErrorCode::E4012 => 4012,
            ErrorCode::E4013 => 4013,
            ErrorCode::E4014 => 4014,
            ErrorCode::E4015 => 4015,

            // Protocols
            ErrorCode::E4100 => 4100,
            ErrorCode::E4101 => 4101,
            ErrorCode::E4102 => 4102,
            ErrorCode::E4103 => 4103,
            ErrorCode::E4104 => 4104,
            ErrorCode::E4105 => 4105,

            // Import
            ErrorCode::E5001 => 5001,
            ErrorCode::E5002 => 5002,
            ErrorCode::E5003 => 5003,
            ErrorCode::E5004 => 5004,
            ErrorCode::E5005 => 5005,
            ErrorCode::E5006 => 5006,

            // Module System
            ErrorCode::E5100 => 5100,
            ErrorCode::E5101 => 5101,
            ErrorCode::E5102 => 5102,
            ErrorCode::E5103 => 5103,
            ErrorCode::E5104 => 5104,

            // Concurrency
            ErrorCode::E6001 => 6001,
            ErrorCode::E6002 => 6002,
            ErrorCode::E6003 => 6003,
            ErrorCode::E6004 => 6004,
            ErrorCode::E6005 => 6005,
            ErrorCode::E6006 => 6006,
            ErrorCode::E6007 => 6007,
            ErrorCode::E6008 => 6008,

            // Memory Safety
            ErrorCode::E7001 => 7001,
            ErrorCode::E7002 => 7002,
            ErrorCode::E7003 => 7003,
            ErrorCode::E7004 => 7004,
            ErrorCode::E7005 => 7005,
            ErrorCode::E7006 => 7006,
            ErrorCode::E7007 => 7007,

            // Pattern Matching
            ErrorCode::E8001 => 8001,
            ErrorCode::E8002 => 8002,
            ErrorCode::E8003 => 8003,
            ErrorCode::E8004 => 8004,
            ErrorCode::E8005 => 8005,

            // Decorators
            ErrorCode::E9001 => 9001,
            ErrorCode::E9002 => 9002,
            ErrorCode::E9003 => 9003,
            ErrorCode::E9004 => 9004,
            ErrorCode::E9005 => 9005,

            // Deprecation Warnings
            ErrorCode::W2001 => 2001,

            // Code Quality Warnings
            ErrorCode::W3001 => 3001,
            ErrorCode::W3002 => 3002,
            ErrorCode::W3003 => 3003,
            ErrorCode::W3004 => 3004,
            ErrorCode::W3006 => 3006,
            ErrorCode::W3007 => 3007,
        }
    }

    /// Get the error category.
    pub fn category(&self) -> &'static str {
        match self.code() / 1000 {
            1 => "Lexical",
            2 => "Syntax",
            3 => "Semantic",
            4 => "Type",
            5 => "Import",
            6 => "Concurrency",
            7 => "Memory Safety",
            8 => "Pattern Matching",
            9 => "Decorator",
            _ => "Unknown",
        }
    }

    /// Get the default severity for this error code.
    pub fn default_severity(&self) -> Severity {
        // Determine severity based on the variant name prefix
        // W-prefixed codes are warnings, E-prefixed are errors, I-prefixed are info
        let variant_name = format!("{:?}", self);

        if variant_name.starts_with('W') {
            Severity::Warning
        } else if variant_name.starts_with('I') {
            Severity::Info
        } else {
            // E-prefixed or any other prefix defaults to Error
            Severity::Error
        }
    }
    /// Get the appropriate prefix for this error code (E, W, or I)
    pub fn prefix(&self) -> char {
        match self.default_severity() {
            Severity::Info => 'I',
            Severity::Warning => 'W',
            Severity::Error | Severity::Fatal => 'E',
        }
    }
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{:04}", self.prefix(), self.code())
    }
}
