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
    E3100, // Unreachable code
    E3101, // Infinite loop detected
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
            ErrorCode::E3100 => 3100,
            ErrorCode::E3101 => 3101,
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
        match self.code() / 1000 {
            1..=9 => Severity::Error,
            _ => Severity::Error,
        }
    }

    /// Get a human-readable description for this error code.
    pub fn description(&self) -> &'static str {
        match self {
            // Lexical errors
            ErrorCode::E1001 => "Invalid character in source code",
            ErrorCode::E1002 => "String literal is not properly terminated",
            ErrorCode::E1003 => "Number literal has invalid format",
            ErrorCode::E1004 => "Identifier name is not valid",
            ErrorCode::E1005 => "Inconsistent use of tabs and spaces in indentation",
            ErrorCode::E1006 => "Invalid escape sequence in string",

            // Syntax errors
            ErrorCode::E2001 => "Unexpected token encountered during parsing",
            ErrorCode::E2002 => "Expected a specific token but found something else",
            ErrorCode::E2003 => "File ended unexpectedly while parsing",
            ErrorCode::E2004 => "Invalid syntax structure",
            ErrorCode::E2005 => "Indentation is inconsistent or incorrect",
            ErrorCode::E2006 => "Opening delimiter has no matching closing delimiter",
            ErrorCode::E2007 => "Closing delimiter has no matching opening delimiter",
            ErrorCode::E2008 => "Expected ':' after context",
            ErrorCode::E2009 => "Expected expression",
            ErrorCode::E2010 => "Invalid assignment target",
            ErrorCode::E2011 => "'break' can only be used inside loops",
            ErrorCode::E2012 => "'continue' can only be used inside loops",
            ErrorCode::E2013 => "'return' can only be used inside functions",
            ErrorCode::E2014 => "'yield' can only be used inside functions",
            ErrorCode::E2015 => "'await' can only be used inside async functions",
            ErrorCode::E2016 => "'async for' can only be used inside async functions",
            ErrorCode::E2017 => "'async with' can only be used inside async functions",
            ErrorCode::E2018 => "Duplicate parameter name",
            ErrorCode::E2019 => "Duplicate keyword argument",
            ErrorCode::E2020 => "Positional argument cannot follow keyword argument",
            ErrorCode::E2021 => "Parameters are in invalid order",
            ErrorCode::E2022 => "Cannot mix except and except* in the same try statement",
            ErrorCode::E2023 => "except* requires a specific exception type, not a bare except",
            ErrorCode::E2024 => "'from __future__ import' must occur at the beginning of the file",
            ErrorCode::E2025 => "Relative import cannot go beyond top-level package",
            ErrorCode::E2026 => "Invalid relative import syntax",
            ErrorCode::E2027 => "Unexpected indent",
            ErrorCode::E2028 => "Expected an indented block",
            ErrorCode::E2029 => "Unindent does not match any outer indentation level",

            // Name Resolution errors
            ErrorCode::E3001 => "Name is used before being defined",
            ErrorCode::E3002 => "Name is already defined in this scope",
            ErrorCode::E3003 => "nonlocal declaration without enclosing scope",
            ErrorCode::E3004 => "Name is used before definition",

            // Control Flow errors
            ErrorCode::E3100 => "Code is unreachable",
            ErrorCode::E3101 => "Infinite loop detected",
            ErrorCode::E3102 => "Missing return statement",
            ErrorCode::E3103 => "Inconsistent return types",
            ErrorCode::E3104 => "Dead code after return statement",

            // Type errors
            ErrorCode::E4001 => "Type mismatch between expected and actual types",
            ErrorCode::E4002 => "Incompatible types in binary operation",
            ErrorCode::E4003 => "Incompatible type in unary operation",
            ErrorCode::E4004 => "Wrong number of arguments",
            ErrorCode::E4005 => "Invalid argument type",
            ErrorCode::E4006 => "Return type doesn't match function signature",
            ErrorCode::E4007 => "Invalid subscript operation",
            ErrorCode::E4008 => "Invalid attribute access",
            ErrorCode::E4009 => "Cannot call non-callable",
            ErrorCode::E4010 => "Cannot subscript non-subscriptable",
            ErrorCode::E4011 => "Cannot iterate non-iterable",
            ErrorCode::E4012 => "Cannot infer type",
            ErrorCode::E4013 => "Ambiguous type",
            ErrorCode::E4014 => "Type annotation required",
            ErrorCode::E4015 => "Invalid type annotation",

            // Protocol errors
            ErrorCode::E4100 => "Class doesn't implement required protocol method",
            ErrorCode::E4101 => "Method signature doesn't match protocol",
            ErrorCode::E4102 => "Protocol cannot have implementations",
            ErrorCode::E4103 => "Invalid protocol definition",
            ErrorCode::E4104 => "Protocol not satisfied",
            ErrorCode::E4105 => "Runtime checkable protocol violation",

            // Import errors
            ErrorCode::E5001 => "Module could not be found or loaded",
            ErrorCode::E5002 => "Circular dependency detected between modules",
            ErrorCode::E5003 => "Import statement has invalid syntax",
            ErrorCode::E5004 => "Import from non-module",
            ErrorCode::E5005 => "Cannot import name",
            ErrorCode::E5006 => "Relative import in non-package",

            // Module System errors
            ErrorCode::E5100 => "Exporting undefined name",
            ErrorCode::E5101 => "Duplicate export",
            ErrorCode::E5102 => "Invalid module introspection function",
            ErrorCode::E5103 => "Export from non-existent module",
            ErrorCode::E5104 => "Invalid export syntax",

            // Concurrency errors
            ErrorCode::E6001 => "Data race: shared mutable data accessed without synchronization",
            ErrorCode::E6002 => "Potential deadlock: circular lock dependency detected",
            ErrorCode::E6003 => "Attempting to send non-Send type across threads",
            ErrorCode::E6004 => "Attempting to share non-Sync type between threads",
            ErrorCode::E6005 => "Lock acquired but never released",
            ErrorCode::E6006 => "Accessing shared data without holding required lock",
            ErrorCode::E6007 => "Double lock acquisition",
            ErrorCode::E6008 => "Lock order violation",

            // Memory Safety errors
            ErrorCode::E7001 => "Variable used after being freed/deallocated",
            ErrorCode::E7002 => "Attempting to free already freed memory",
            ErrorCode::E7003 => "Resource not properly cleaned up",
            ErrorCode::E7004 => "Reference to variable that went out of scope",
            ErrorCode::E7005 => "Circular reference that cannot be automatically freed",
            ErrorCode::E7006 => "Invalid lifetime",
            ErrorCode::E7007 => "Moved value used",

            // Pattern Matching errors
            ErrorCode::E8001 => "Match statement is not exhaustive",
            ErrorCode::E8002 => "Pattern is unreachable",
            ErrorCode::E8003 => "Pattern type doesn't match subject type",
            ErrorCode::E8004 => "Invalid pattern syntax",
            ErrorCode::E8005 => "Duplicate pattern binding",

            // Decorator errors
            ErrorCode::E9001 => "Decorator not found",
            ErrorCode::E9002 => "Invalid decorator target",
            ErrorCode::E9003 => "Decorator signature mismatch",
            ErrorCode::E9004 => "Decorator application failed",
            ErrorCode::E9005 => "Invalid decorator composition",

            // Deprecation warnings
            ErrorCode::W2001 => "Using deprecated feature",

            // Code quality warnings
            ErrorCode::W3001 => "Variable assigned but never used",
            ErrorCode::W3002 => "Function defined but never called",
            ErrorCode::W3003 => "Import never used",
            ErrorCode::W3004 => "Parameter never used in function body",
            ErrorCode::W3006 => "Name shadows a builtin",
            ErrorCode::W3007 => "Import shadows a previous import",
        }
    }

    /// Get a helpful suggestion for fixing this error.
    pub fn suggestion(&self) -> Option<&'static str> {
        match self {
            // Lexical errors
            ErrorCode::E1001 => {
                Some("Remove the invalid character or escape it if it's part of a string.")
            }
            ErrorCode::E1002 => {
                Some("Add a closing quote (', \", ''', or \"\"\") to complete the string.")
            }
            ErrorCode::E1003 => {
                Some("Check the number format. Valid examples: 42, 3.14, 1e-5, 0xFF, 0o77, 0b1010.")
            }
            ErrorCode::E1004 => Some(
                "Identifiers must start with a letter or underscore, followed by letters, digits, or underscores.",
            ),
            ErrorCode::E1005 => Some(
                "Use consistent indentation throughout (either spaces or tabs, not both). Most code uses 4 spaces.",
            ),
            ErrorCode::E1006 => {
                Some("Use valid escape sequences like \\n, \\t, \\r, \\\\, \\', \\\".")
            }

            // Syntax errors
            ErrorCode::E2001 => Some(
                "Look for missing operators, parentheses, commas, or quotation marks around this location.",
            ),
            ErrorCode::E2002 => Some(
                "Check that you've included all required parts of the statement (colons, keywords, etc.).",
            ),
            ErrorCode::E2003 => Some(
                "You may have unclosed brackets (), [], {}, quotes, or an incomplete statement at the end.",
            ),
            ErrorCode::E2004 => {
                Some("Review the syntax structure and check for typos or missing elements.")
            }
            ErrorCode::E2005 => Some(
                "Use consistent indentation throughout (either spaces or tabs, not both). Most code uses 4 spaces.",
            ),
            ErrorCode::E2006 => Some("Add the matching closing delimiter: ), ], or }."),
            ErrorCode::E2007 => {
                Some("Remove this closing delimiter or add a matching opening delimiter earlier.")
            }
            ErrorCode::E2008 => Some("Add a colon ':' after the statement header."),
            ErrorCode::E2009 => Some("Provide an expression or statement here."),
            ErrorCode::E2010 => Some(
                "You can only assign to variables, attributes, subscripts, or tuple/list unpacking targets.",
            ),
            ErrorCode::E2011 => Some("Place 'break' inside a 'for' or 'while' loop body."),
            ErrorCode::E2012 => Some("Place 'continue' inside a 'for' or 'while' loop body."),
            ErrorCode::E2013 => {
                Some("Place 'return' inside a function definition (def or async def).")
            }
            ErrorCode::E2014 => {
                Some("Place 'yield' inside a function definition (def or async def).")
            }
            ErrorCode::E2015 => Some("Place 'await' inside an async function definition."),
            ErrorCode::E2016 => Some("Place 'async for' inside an async function definition."),
            ErrorCode::E2017 => Some("Place 'async with' inside an async function definition."),
            ErrorCode::E2018 => {
                Some("Parameter names must be unique within the function signature.")
            }
            ErrorCode::E2019 => Some("Each keyword argument can only be specified once."),
            ErrorCode::E2020 => Some("Put all positional arguments before keyword arguments."),
            ErrorCode::E2021 => Some("Correct order: positional, *args, keyword-only, **kwargs."),
            ErrorCode::E2022 => {
                Some("Use either 'except' or 'except*', not both in the same try statement.")
            }
            ErrorCode::E2023 => Some("Use 'except* ExceptionType:' instead of 'except*:'."),
            ErrorCode::E2024 => {
                Some("Move 'from __future__ import' to the top of the file, after any docstring.")
            }
            ErrorCode::E2025 => Some("Reduce the number of leading dots in the relative import."),
            ErrorCode::E2026 => {
                Some("Use valid relative import syntax: 'from .module import name'.")
            }
            ErrorCode::E2027 => {
                Some("Check your indentation level and ensure it matches the expected level.")
            }
            ErrorCode::E2028 => Some("Add an indented block after the colon."),
            ErrorCode::E2029 => {
                Some("Match the indentation level to a previous level or add proper indentation.")
            }

            // Name Resolution errors
            ErrorCode::E3001 => {
                Some("Define the name before using it, or check for typos in the name.")
            }
            ErrorCode::E3002 => {
                Some("Choose a different name or remove one of the duplicate definitions.")
            }
            ErrorCode::E3003 => Some(
                "Ensure the name exists in an enclosing scope before declaring it as nonlocal.",
            ),
            ErrorCode::E3004 => {
                Some("Define the name before using it, or check for typos in the name.")
            }

            // Control Flow errors
            ErrorCode::E3100 => Some("Remove the unreachable code or fix the control flow logic."),
            ErrorCode::E3101 => {
                Some("Add a condition that will eventually become false to break the loop.")
            }
            ErrorCode::E3102 => {
                Some("Add a return statement or ensure all code paths return a value.")
            }
            ErrorCode::E3103 => Some("Ensure all return statements return the same type."),
            ErrorCode::E3104 => Some("Remove the dead code or restructure the control flow."),

            // Type errors
            ErrorCode::E4001 => {
                Some("Make sure both operands are compatible types for this operation.")
            }
            ErrorCode::E4002 => Some("This operation may not be supported for the given type."),
            ErrorCode::E4003 => {
                Some("This unary operation may not be supported for the given type.")
            }
            ErrorCode::E4004 => {
                Some("Provide the expected number of arguments for this function call.")
            }
            ErrorCode::E4005 => {
                Some("Convert the argument to the expected type or provide a different value.")
            }
            ErrorCode::E4006 => Some("Ensure the return type matches the function signature."),
            ErrorCode::E4007 => Some("Make sure the subscript operation is valid for this type."),
            ErrorCode::E4008 => Some("Check that the attribute exists and is accessible."),
            ErrorCode::E4009 => {
                Some("Make sure the object is callable (function, method, or callable object).")
            }
            ErrorCode::E4010 => {
                Some("Make sure the object supports subscripting (list, dict, etc.).")
            }
            ErrorCode::E4011 => {
                Some("Make sure the object is iterable (list, tuple, string, etc.).")
            }
            ErrorCode::E4012 => Some("Add explicit type annotations to help the type checker."),
            ErrorCode::E4013 => {
                Some("Provide more specific type information to resolve ambiguity.")
            }
            ErrorCode::E4014 => Some("Add a type annotation to clarify the expected type."),
            ErrorCode::E4015 => Some("Use valid type annotation syntax."),

            // Protocol errors
            ErrorCode::E4100 => Some("Implement the missing method in the class."),
            ErrorCode::E4101 => Some("Update the method signature to match the protocol."),
            ErrorCode::E4102 => Some("Remove the method implementation from the protocol."),
            ErrorCode::E4103 => Some("Fix the protocol definition syntax."),
            ErrorCode::E4104 => Some("Implement all required methods from the protocol."),
            ErrorCode::E4105 => {
                Some("Ensure the protocol is properly marked as runtime checkable.")
            }

            // Import errors
            ErrorCode::E5001 => {
                Some("Check that the module name is correct and the module is installed.")
            }
            ErrorCode::E5002 => Some("Reorganize your modules to break the circular dependency."),
            ErrorCode::E5003 => {
                Some("Use valid import syntax: 'import module' or 'from module import name'.")
            }
            ErrorCode::E5004 => {
                Some("Make sure you're importing from a module, not a class or function.")
            }
            ErrorCode::E5005 => Some("Check that the name exists in the module."),
            ErrorCode::E5006 => Some("Use relative imports only within packages."),

            // Module System errors
            ErrorCode::E5100 => Some("Define the name before exporting it."),
            ErrorCode::E5101 => Some("Remove the duplicate export or use different names."),
            ErrorCode::E5102 => {
                Some("Use a valid module introspection function like 'is_main', 'name', or 'path'.")
            }
            ErrorCode::E5103 => Some("Make sure the module exists before exporting from it."),
            ErrorCode::E5104 => Some("Use valid export syntax."),

            // Concurrency errors
            ErrorCode::E6001 => {
                Some("Use locks (Lock, RLock) or atomic types to protect shared mutable state.")
            }
            ErrorCode::E6002 => {
                Some("Always acquire locks in a consistent order across your program.")
            }
            ErrorCode::E6003 => {
                Some("Consider using thread-safe alternatives or Arc/Mutex wrappers.")
            }
            ErrorCode::E6004 => Some("Use Arc/Mutex or other synchronization primitives."),
            ErrorCode::E6005 => Some(
                "Use 'with' statement for automatic lock release or ensure manual release in all branches.",
            ),
            ErrorCode::E6006 => Some("Acquire the lock before accessing this shared variable."),
            ErrorCode::E6007 => Some("Use RLock for reentrant locking or restructure your code."),
            ErrorCode::E6008 => Some("Establish a consistent lock ordering convention."),

            // Memory Safety errors
            ErrorCode::E7001 => {
                Some("Coral automatically tracks lifetimes to prevent use-after-free bugs.")
            }
            ErrorCode::E7002 => {
                Some("Coral prevents double-free through automatic lifetime analysis.")
            }
            ErrorCode::E7003 => {
                Some("Consider using 'with' statement for automatic resource cleanup.")
            }
            ErrorCode::E7004 => {
                Some("Coral ensures references never outlive the data they point to.")
            }
            ErrorCode::E7005 => {
                Some("Consider breaking the cycle by using weak references or explicit cleanup.")
            }
            ErrorCode::E7006 => Some("Check the lifetime annotations and scope boundaries."),
            ErrorCode::E7007 => Some("Avoid using moved values after they've been moved."),

            // Pattern Matching errors
            ErrorCode::E8001 => {
                Some("Add patterns to cover all possible values or add a wildcard pattern.")
            }
            ErrorCode::E8002 => Some("Remove the unreachable pattern or reorder patterns."),
            ErrorCode::E8003 => Some("Ensure the pattern type matches the subject type."),
            ErrorCode::E8004 => Some("Use valid pattern syntax."),
            ErrorCode::E8005 => Some("Use different variable names for pattern bindings."),

            // Decorator errors
            ErrorCode::E9001 => Some("Check that the decorator is imported and available."),
            ErrorCode::E9002 => Some("Make sure the decorator is compatible with the target."),
            ErrorCode::E9003 => {
                Some("Update the decorator signature to match the expected signature.")
            }
            ErrorCode::E9004 => Some("Check the decorator implementation and fix the error."),
            ErrorCode::E9005 => Some("Ensure decorators are composed correctly."),

            // Deprecation warnings
            ErrorCode::W2001 => Some("Check the documentation for the recommended alternative."),

            // Code quality warnings
            ErrorCode::W3001 => Some(
                "Remove the unused variable or use it in your code. Prefix with '_' to suppress this warning.",
            ),
            ErrorCode::W3002 => Some("Remove the unused function or call it from your code."),
            ErrorCode::W3003 => Some("Remove the unused import statement."),
            ErrorCode::W3004 => Some(
                "Remove the unused parameter or prefix it with '_' to indicate it's intentionally unused.",
            ),
            ErrorCode::W3006 => Some("Choose a different name that doesn't shadow the builtin."),
            ErrorCode::W3007 => Some("Remove one of the imports or use an alias with 'as'."),
        }
    }
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code_num = self.code();
        // Warning codes start with W, error codes start with E
        if matches!(
            self,
            ErrorCode::W2001
                | ErrorCode::W3001
                | ErrorCode::W3002
                | ErrorCode::W3003
                | ErrorCode::W3004
                | ErrorCode::W3006
                | ErrorCode::W3007
        ) {
            write!(f, "W{:04}", code_num)
        } else {
            write!(f, "E{:04}", code_num)
        }
    }
}
