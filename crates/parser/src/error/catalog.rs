//! Error metadata catalog providing comprehensive information about all error types.
//!
//! This module contains the ErrorMetadata struct and implementations that map
//! ErrorKind variants to their corresponding error codes, severity levels,
//! descriptions, and suggestions.

use super::codes::{ErrorCode, Severity};
use super::kinds::ErrorKind;

/// Error category for organizing different types of errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCategory {
    Lexical,
    Syntax,
    Semantic,
    Type,
    Protocol,
    Import,
    ModuleSystem,
    Concurrency,
    MemorySafety,
    PatternMatching,
    Decorator,
}

impl ErrorCategory {
    /// Get the string representation of this category.
    pub fn as_str(&self) -> &'static str {
        match self {
            ErrorCategory::Lexical => "Lexical",
            ErrorCategory::Syntax => "Syntax",
            ErrorCategory::Semantic => "Semantic",
            ErrorCategory::Type => "Type",
            ErrorCategory::Protocol => "Protocol",
            ErrorCategory::Import => "Import",
            ErrorCategory::ModuleSystem => "Module System",
            ErrorCategory::Concurrency => "Concurrency",
            ErrorCategory::MemorySafety => "Memory Safety",
            ErrorCategory::PatternMatching => "Pattern Matching",
            ErrorCategory::Decorator => "Decorator",
        }
    }
}

/// Comprehensive error metadata for each error kind.
#[derive(Debug, Clone)]
pub struct ErrorMetadata {
    pub code: ErrorCode,
    pub severity: Severity,
    pub category: ErrorCategory,
    pub error_type: &'static str,
    pub title: &'static str,
    pub description: &'static str,
    pub suggestion: Option<&'static str>,
    pub help_url: Option<&'static str>,
}

impl ErrorKind {
    /// Get comprehensive metadata for this error kind.
    pub fn metadata(&self) -> ErrorMetadata {
        match self {
            // Lexical errors
            ErrorKind::InvalidCharacter => ErrorMetadata {
                code: ErrorCode::E1001,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "SyntaxError",
                title: "Invalid character in source code",
                description: "The source code contains a character that is not valid in Coral syntax",
                suggestion: Some(
                    "Remove the invalid character or escape it if it's part of a string",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E1001"),
            },
            ErrorKind::UnterminatedString => ErrorMetadata {
                code: ErrorCode::E1002,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "SyntaxError",
                title: "String literal is not properly terminated",
                description: "A string literal is missing its closing quote",
                suggestion: Some(
                    "Add a closing quote (', \", ''', or \"\"\") to complete the string",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E1002"),
            },
            ErrorKind::InvalidNumber => ErrorMetadata {
                code: ErrorCode::E1003,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "ValueError",
                title: "Number literal has invalid format",
                description: "The number literal does not follow valid Coral number syntax",
                suggestion: Some(
                    "Check the number format. Valid examples: 42, 3.14, 1e-5, 0xFF, 0o77, 0b1010",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E1003"),
            },
            ErrorKind::InvalidIdentifier => ErrorMetadata {
                code: ErrorCode::E1004,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "SyntaxError",
                title: "Identifier name is not valid",
                description: "The identifier does not follow valid Coral identifier rules",
                suggestion: Some(
                    "Identifiers must start with a letter or underscore, followed by letters, digits, or underscores",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E1004"),
            },
            ErrorKind::MixedTabsAndSpaces => ErrorMetadata {
                code: ErrorCode::E1005,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "IndentationError",
                title: "Inconsistent use of tabs and spaces in indentation",
                description: "The code mixes tabs and spaces for indentation, which is not allowed",
                suggestion: Some(
                    "Use consistent indentation throughout (either spaces or tabs, not both). Most code uses 4 spaces",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E1005"),
            },
            ErrorKind::InvalidEscapeSequence => ErrorMetadata {
                code: ErrorCode::E1006,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "SyntaxError",
                title: "Invalid escape sequence in string",
                description: "The string contains an invalid escape sequence",
                suggestion: Some("Use valid escape sequences like \\n, \\t, \\r, \\\\, \\', \\\""),
                help_url: Some("https://docs.coral-lang.org/errors/E1006"),
            },

            // Syntax errors
            ErrorKind::UnexpectedToken { .. } => ErrorMetadata {
                code: ErrorCode::E2001,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Unexpected token encountered during parsing",
                description: "The parser encountered a token that was not expected at this location",
                suggestion: Some(
                    "Look for missing operators, parentheses, commas, or quotation marks around this location",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2001"),
            },
            ErrorKind::ExpectedToken { .. } => ErrorMetadata {
                code: ErrorCode::E2002,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Expected a specific token but found something else",
                description: "The parser expected a specific token but found a different one",
                suggestion: Some(
                    "Check that you've included all required parts of the statement (colons, keywords, etc.)",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2002"),
            },
            ErrorKind::UnexpectedEof => ErrorMetadata {
                code: ErrorCode::E2003,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "File ended unexpectedly while parsing",
                description: "The file ended while the parser was expecting more content",
                suggestion: Some(
                    "You may have unclosed brackets (), [], {}, quotes, or an incomplete statement at the end",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2003"),
            },
            ErrorKind::InvalidSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E2004,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Invalid syntax structure",
                description: "The code structure does not follow valid Coral syntax",
                suggestion: Some(
                    "Review the syntax structure and check for typos or missing elements",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2004"),
            },
            ErrorKind::IndentationError => ErrorMetadata {
                code: ErrorCode::E2005,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Indentation is inconsistent or incorrect",
                description: "The indentation does not follow indentation-based syntax rules",
                suggestion: Some(
                    "Use consistent indentation throughout (either spaces or tabs, not both). Most code uses 4 spaces",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2005"),
            },
            ErrorKind::UnclosedDelimiter { .. } => ErrorMetadata {
                code: ErrorCode::E2006,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Opening delimiter has no matching closing delimiter",
                description: "A bracket, parenthesis, or brace is not properly closed",
                suggestion: Some("Add the matching closing delimiter"),
                help_url: Some("https://docs.coral-lang.org/errors/E2006"),
            },
            ErrorKind::UnmatchedClosing { .. } => ErrorMetadata {
                code: ErrorCode::E2007,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Closing delimiter has no matching opening delimiter",
                description: "Found a closing bracket, parenthesis, or brace without a corresponding opening one",
                suggestion: Some(
                    "Remove the extra closing delimiter or add the matching opening one",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2007"),
            },
            ErrorKind::MissingColon { .. } => ErrorMetadata {
                code: ErrorCode::E2008,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Expected ':' after statement",
                description: "Compound statements like if, while, for, def, and class require a colon",
                suggestion: Some("Add a ':' at the end of the statement"),
                help_url: Some("https://docs.coral-lang.org/errors/E2008"),
            },
            ErrorKind::ExpectedExpression => ErrorMetadata {
                code: ErrorCode::E2009,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Expected an expression",
                description: "An expression was expected but not found",
                suggestion: Some("Provide a valid expression"),
                help_url: Some("https://docs.coral-lang.org/errors/E2009"),
            },
            ErrorKind::InvalidAssignmentTarget => ErrorMetadata {
                code: ErrorCode::E2010,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Invalid assignment target",
                description: "The left side of an assignment must be a variable, attribute, or subscript",
                suggestion: Some(
                    "Assignment target must be a variable name, object.attribute, or container[index]",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2010"),
            },
            ErrorKind::BreakOutsideLoop => ErrorMetadata {
                code: ErrorCode::E2011,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'break' statement outside loop",
                description: "'break' can only be used inside for or while loops",
                suggestion: Some("Move the 'break' statement inside a loop or remove it"),
                help_url: Some("https://docs.coral-lang.org/errors/E2011"),
            },
            ErrorKind::ContinueOutsideLoop => ErrorMetadata {
                code: ErrorCode::E2012,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'continue' statement outside loop",
                description: "'continue' can only be used inside for or while loops",
                suggestion: Some("Move the 'continue' statement inside a loop or remove it"),
                help_url: Some("https://docs.coral-lang.org/errors/E2012"),
            },
            ErrorKind::ReturnOutsideFunction => ErrorMetadata {
                code: ErrorCode::E2013,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'return' statement outside function",
                description: "'return' can only be used inside functions",
                suggestion: Some("Move the 'return' statement inside a function or remove it"),
                help_url: Some("https://docs.coral-lang.org/errors/E2013"),
            },
            ErrorKind::YieldOutsideFunction => ErrorMetadata {
                code: ErrorCode::E2014,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'yield' statement outside function",
                description: "'yield' can only be used inside functions",
                suggestion: Some("Move the 'yield' statement inside a function or remove it"),
                help_url: Some("https://docs.coral-lang.org/errors/E2014"),
            },
            ErrorKind::AwaitOutsideAsync => ErrorMetadata {
                code: ErrorCode::E2015,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'await' expression outside async function",
                description: "'await' can only be used inside async functions",
                suggestion: Some(
                    "Use 'await' inside an async function or make the enclosing function async",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2015"),
            },
            ErrorKind::AsyncForOutsideAsync => ErrorMetadata {
                code: ErrorCode::E2016,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'async for' statement outside async function",
                description: "'async for' can only be used inside async functions",
                suggestion: Some(
                    "Use 'async for' inside an async function or use a regular 'for' loop",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2016"),
            },
            ErrorKind::AsyncWithOutsideAsync => ErrorMetadata {
                code: ErrorCode::E2017,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'async with' statement outside async function",
                description: "'async with' can only be used inside async functions",
                suggestion: Some(
                    "Use 'async with' inside an async function or use a regular 'with' statement",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2017"),
            },
            ErrorKind::DuplicateParameter { .. } => ErrorMetadata {
                code: ErrorCode::E2018,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Duplicate parameter name",
                description: "A parameter name appears more than once in the function signature",
                suggestion: Some("Use unique names for all parameters"),
                help_url: Some("https://docs.coral-lang.org/errors/E2018"),
            },
            ErrorKind::DuplicateArgument { .. } => ErrorMetadata {
                code: ErrorCode::E2019,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Duplicate keyword argument",
                description: "A keyword argument is specified more than once",
                suggestion: Some("Remove duplicate keyword arguments"),
                help_url: Some("https://docs.coral-lang.org/errors/E2019"),
            },
            ErrorKind::PositionalAfterKeyword => ErrorMetadata {
                code: ErrorCode::E2020,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Positional argument after keyword argument",
                description: "Positional arguments must come before keyword arguments",
                suggestion: Some(
                    "Reorder arguments to place positional arguments before keyword arguments",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2020"),
            },
            ErrorKind::InvalidParameterOrder => ErrorMetadata {
                code: ErrorCode::E2021,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Invalid parameter order",
                description: "Parameters must be ordered: positional, *args, keyword-only, **kwargs",
                suggestion: Some("Reorder parameters according to parameter ordering rules"),
                help_url: Some("https://docs.coral-lang.org/errors/E2021"),
            },
            ErrorKind::MixedExceptSyntax => ErrorMetadata {
                code: ErrorCode::E2022,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Cannot mix 'except' and 'except*'",
                description: "A try statement cannot have both regular except and except* clauses",
                suggestion: Some(
                    "Use either 'except' or 'except*', but not both in the same try statement",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2022"),
            },
            ErrorKind::BareExceptStar => ErrorMetadata {
                code: ErrorCode::E2023,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'except*' requires exception type",
                description: "'except*' cannot be used without specifying an exception type",
                suggestion: Some("Specify an exception type: except* ExceptionType:"),
                help_url: Some("https://docs.coral-lang.org/errors/E2023"),
            },
            ErrorKind::FutureImportNotFirst => ErrorMetadata {
                code: ErrorCode::E2024,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'from __future__ import' not at beginning",
                description: "'from __future__ import' must occur at the beginning of the file",
                suggestion: Some(
                    "Move the __future__ import to the top of the file, after any module docstring",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2024"),
            },
            ErrorKind::RelativeImportBeyondTopLevel => ErrorMetadata {
                code: ErrorCode::E2025,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "ImportError",
                title: "Relative import goes beyond top-level package",
                description: "Too many leading dots in relative import",
                suggestion: Some("Reduce the number of leading dots in the relative import"),
                help_url: Some("https://docs.coral-lang.org/errors/E2025"),
            },
            ErrorKind::InvalidRelativeImport => ErrorMetadata {
                code: ErrorCode::E2026,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "ImportError",
                title: "Invalid relative import syntax",
                description: "The relative import syntax is malformed",
                suggestion: Some(
                    "Check the relative import syntax: from . import name, from .. import name",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E2026"),
            },
            ErrorKind::UnexpectedIndent => ErrorMetadata {
                code: ErrorCode::E2027,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Unexpected indent",
                description: "Found indentation where it wasn't expected",
                suggestion: Some("Remove the extra indentation"),
                help_url: Some("https://docs.coral-lang.org/errors/E2027"),
            },
            ErrorKind::ExpectedIndent => ErrorMetadata {
                code: ErrorCode::E2028,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Expected an indented block",
                description: "A compound statement requires an indented block",
                suggestion: Some("Add an indented block after the colon"),
                help_url: Some("https://docs.coral-lang.org/errors/E2028"),
            },
            ErrorKind::UnindentMismatch => ErrorMetadata {
                code: ErrorCode::E2029,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Unindent does not match any outer indentation level",
                description: "The dedent doesn't align with any previous indentation level",
                suggestion: Some("Check that your indentation is consistent"),
                help_url: Some("https://docs.coral-lang.org/errors/E2029"),
            },

            // Name Resolution errors
            ErrorKind::UndefinedName { .. } => ErrorMetadata {
                code: ErrorCode::E3001,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "NameError",
                title: "Name is not defined",
                description: "The name is used before being defined in the current scope",
                suggestion: Some("Define the name before using it, or check for typos in the name"),
                help_url: Some("https://docs.coral-lang.org/errors/E3001"),
            },
            ErrorKind::DuplicateDefinition { .. } => ErrorMetadata {
                code: ErrorCode::E3002,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "NameError",
                title: "Name is already defined",
                description: "The name is already defined in this scope",
                suggestion: Some(
                    "Choose a different name or remove one of the duplicate definitions",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E3002"),
            },
            ErrorKind::NonlocalWithoutEnclosing { .. } => ErrorMetadata {
                code: ErrorCode::E3003,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "SyntaxError",
                title: "nonlocal declaration without enclosing scope",
                description: "'nonlocal' requires a name to be defined in an enclosing scope",
                suggestion: Some(
                    "Remove the nonlocal declaration or define the name in an enclosing function",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E3003"),
            },
            ErrorKind::UsedBeforeDefinition { .. } => ErrorMetadata {
                code: ErrorCode::E3004,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "NameError",
                title: "Name used before definition",
                description: "The name is used before it's assigned a value",
                suggestion: Some("Define or assign the name before using it"),
                help_url: Some("https://docs.coral-lang.org/errors/E3004"),
            },
            ErrorKind::UnreachableCode { .. } => ErrorMetadata {
                code: ErrorCode::E3100,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Code is unreachable",
                description: "This code will never be executed",
                suggestion: Some("Remove the unreachable code or fix the control flow"),
                help_url: Some("https://docs.coral-lang.org/errors/E3100"),
            },
            ErrorKind::InfiniteLoop { .. } => ErrorMetadata {
                code: ErrorCode::E3101,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Infinite loop detected",
                description: "This loop has no exit condition",
                suggestion: Some("Add a break statement or modify the loop condition"),
                help_url: Some("https://docs.coral-lang.org/errors/E3101"),
            },
            ErrorKind::MissingReturn { .. } => ErrorMetadata {
                code: ErrorCode::E3102,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "TypeError",
                title: "Missing return statement",
                description: "Function with return type annotation doesn't always return a value",
                suggestion: Some("Add a return statement in all code paths"),
                help_url: Some("https://docs.coral-lang.org/errors/E3102"),
            },
            ErrorKind::InconsistentReturnTypes { .. } => ErrorMetadata {
                code: ErrorCode::E3103,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "TypeError",
                title: "Inconsistent return types",
                description: "Function returns different types in different code paths",
                suggestion: Some("Ensure all return statements return the same type"),
                help_url: Some("https://docs.coral-lang.org/errors/E3103"),
            },
            ErrorKind::DeadCodeAfterReturn => ErrorMetadata {
                code: ErrorCode::E3104,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Dead code after return",
                description: "Code after return statement will never be executed",
                suggestion: Some("Remove the dead code or restructure the function"),
                help_url: Some("https://docs.coral-lang.org/errors/E3104"),
            },
            ErrorKind::UnreachableExceptionHandler { .. } => ErrorMetadata {
                code: ErrorCode::E3100,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Unreachable exception handler",
                description: "This exception handler will never be reached",
                suggestion: Some("Reorder exception handlers from most specific to most general"),
                help_url: Some("https://docs.coral-lang.org/errors/E3100"),
            },

            // Type System errors
            ErrorKind::TypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4001,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Type mismatch",
                description: "Incompatible types for this operation",
                suggestion: Some("Make sure both operands are compatible types for this operation"),
                help_url: Some("https://docs.coral-lang.org/errors/E4001"),
            },
            ErrorKind::IncompatibleBinOp { .. } => ErrorMetadata {
                code: ErrorCode::E4002,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Incompatible types in binary operation",
                description: "The binary operation is not supported for these types",
                suggestion: Some("This operation may not be supported for the given type"),
                help_url: Some("https://docs.coral-lang.org/errors/E4002"),
            },
            ErrorKind::IncompatibleUnaryOp { .. } => ErrorMetadata {
                code: ErrorCode::E4003,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Incompatible type in unary operation",
                description: "The unary operation is not supported for this type",
                suggestion: Some("This unary operator may not be supported for the given type"),
                help_url: Some("https://docs.coral-lang.org/errors/E4003"),
            },
            ErrorKind::ArgumentCountMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4004,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Wrong number of arguments",
                description: "Function called with wrong number of arguments",
                suggestion: Some(
                    "Check the function signature and provide the correct number of arguments",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4004"),
            },
            ErrorKind::InvalidArgumentType { .. } => ErrorMetadata {
                code: ErrorCode::E4005,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Invalid argument type",
                description: "Argument has wrong type",
                suggestion: Some(
                    "Check the function signature and provide arguments of the correct type",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4005"),
            },
            ErrorKind::ReturnTypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4006,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Return type doesn't match function signature",
                description: "The returned value doesn't match the function's return type",
                suggestion: Some(
                    "Return a value that matches the function's return type annotation",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4006"),
            },
            ErrorKind::InvalidSubscript { .. } => ErrorMetadata {
                code: ErrorCode::E4007,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Invalid subscript operation",
                description: "Invalid index type for this container",
                suggestion: Some(
                    "Use an appropriate index type (e.g., integers for lists, hashable types for dicts)",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4007"),
            },
            ErrorKind::InvalidAttribute { .. } => ErrorMetadata {
                code: ErrorCode::E4008,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "AttributeError",
                title: "Invalid attribute access",
                description: "Type has no such attribute",
                suggestion: Some("Check the type's available attributes"),
                help_url: Some("https://docs.coral-lang.org/errors/E4008"),
            },
            ErrorKind::NotCallable { .. } => ErrorMetadata {
                code: ErrorCode::E4009,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot call non-callable",
                description: "Attempting to call a value that is not a function",
                suggestion: Some("Only functions and callable objects can be called"),
                help_url: Some("https://docs.coral-lang.org/errors/E4009"),
            },
            ErrorKind::NotSubscriptable { .. } => ErrorMetadata {
                code: ErrorCode::E4010,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot subscript non-subscriptable",
                description: "Attempting to index a value that doesn't support indexing",
                suggestion: Some(
                    "Only sequences, mappings, and types with __getitem__ can be indexed",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4010"),
            },
            ErrorKind::NotIterable { .. } => ErrorMetadata {
                code: ErrorCode::E4011,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot iterate non-iterable",
                description: "Attempting to iterate over a value that is not iterable",
                suggestion: Some(
                    "Only sequences, iterators, and types with __iter__ can be iterated",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E4011"),
            },
            ErrorKind::CannotInferType { .. } => ErrorMetadata {
                code: ErrorCode::E4012,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot infer type",
                description: "Type cannot be inferred from context",
                suggestion: Some("Add a type annotation to help the type checker"),
                help_url: Some("https://docs.coral-lang.org/errors/E4012"),
            },
            ErrorKind::AmbiguousType { .. } => ErrorMetadata {
                code: ErrorCode::E4013,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Ambiguous type",
                description: "Type is ambiguous and could be multiple things",
                suggestion: Some("Add a type annotation to clarify the intended type"),
                help_url: Some("https://docs.coral-lang.org/errors/E4013"),
            },
            ErrorKind::TypeAnnotationRequired { .. } => ErrorMetadata {
                code: ErrorCode::E4014,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Type annotation required",
                description: "A type annotation is required in this context",
                suggestion: Some("Add a type annotation"),
                help_url: Some("https://docs.coral-lang.org/errors/E4014"),
            },
            ErrorKind::InvalidTypeAnnotation { .. } => ErrorMetadata {
                code: ErrorCode::E4015,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Invalid type annotation",
                description: "The type annotation is not valid",
                suggestion: Some("Use a valid type annotation"),
                help_url: Some("https://docs.coral-lang.org/errors/E4015"),
            },

            // Protocol errors
            // For brevity, I'll include a few more key ones and add a macro for the rest

            // Protocol errors
            ErrorKind::MissingProtocolMethod { .. } => ErrorMetadata {
                code: ErrorCode::E4100,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Class doesn't implement required protocol method",
                description: "The class is missing a method required by the protocol",
                suggestion: Some("Implement all methods required by the protocol"),
                help_url: Some("https://docs.coral-lang.org/errors/E4100"),
            },
            ErrorKind::MethodSignatureMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4101,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Method signature doesn't match protocol",
                description: "The method signature doesn't match the protocol's requirements",
                suggestion: Some("Update the method signature to match the protocol"),
                help_url: Some("https://docs.coral-lang.org/errors/E4101"),
            },
            ErrorKind::ProtocolWithImplementation { .. } => ErrorMetadata {
                code: ErrorCode::E4102,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Protocol cannot have implementations",
                description: "Protocols should only declare method signatures, not implement them",
                suggestion: Some("Remove method implementations from the protocol"),
                help_url: Some("https://docs.coral-lang.org/errors/E4102"),
            },
            ErrorKind::InvalidProtocol { .. } => ErrorMetadata {
                code: ErrorCode::E4103,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Invalid protocol definition",
                description: "The protocol definition is not valid",
                suggestion: Some("Fix the protocol definition"),
                help_url: Some("https://docs.coral-lang.org/errors/E4103"),
            },
            ErrorKind::ProtocolNotSatisfied { .. } => ErrorMetadata {
                code: ErrorCode::E4104,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Protocol not satisfied",
                description: "The class does not satisfy the protocol requirements",
                suggestion: Some("Implement all required protocol methods with correct signatures"),
                help_url: Some("https://docs.coral-lang.org/errors/E4104"),
            },
            ErrorKind::RuntimeCheckableProtocolViolation { .. } => ErrorMetadata {
                code: ErrorCode::E4105,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Runtime checkable protocol violation",
                description: "Value doesn't satisfy runtime checkable protocol",
                suggestion: Some("Ensure the value implements all required protocol methods"),
                help_url: Some("https://docs.coral-lang.org/errors/E4105"),
            },

            // Import errors
            ErrorKind::ModuleNotFound { .. } => ErrorMetadata {
                code: ErrorCode::E5001,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ModuleNotFoundError",
                title: "Module not found",
                description: "The module could not be found or loaded",
                suggestion: Some("Check that the module exists and is in the import path"),
                help_url: Some("https://docs.coral-lang.org/errors/E5001"),
            },
            ErrorKind::CircularImport { .. } => ErrorMetadata {
                code: ErrorCode::E5002,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Circular import detected",
                description: "Modules have a circular dependency",
                suggestion: Some("Refactor the code to remove circular imports"),
                help_url: Some("https://docs.coral-lang.org/errors/E5002"),
            },
            ErrorKind::InvalidImportSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E5003,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "SyntaxError",
                title: "Invalid import syntax",
                description: "The import statement has invalid syntax",
                suggestion: Some("Check the import statement syntax"),
                help_url: Some("https://docs.coral-lang.org/errors/E5003"),
            },
            ErrorKind::ImportFromNonModule { .. } => ErrorMetadata {
                code: ErrorCode::E5004,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Import from non-module",
                description: "Attempting to import from something that is not a module",
                suggestion: Some("Only modules can be imported from"),
                help_url: Some("https://docs.coral-lang.org/errors/E5004"),
            },
            ErrorKind::CannotImportName { .. } => ErrorMetadata {
                code: ErrorCode::E5005,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Cannot import name",
                description: "The name cannot be imported from the module",
                suggestion: Some("Check that the name exists in the module"),
                help_url: Some("https://docs.coral-lang.org/errors/E5005"),
            },
            ErrorKind::RelativeImportInNonPackage => ErrorMetadata {
                code: ErrorCode::E5006,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Relative import in non-package",
                description: "Relative imports can only be used in packages",
                suggestion: Some("Use absolute imports or convert to a package"),
                help_url: Some("https://docs.coral-lang.org/errors/E5006"),
            },

            // Module System errors
            ErrorKind::ExportUndefined { .. } => ErrorMetadata {
                code: ErrorCode::E5100,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "NameError",
                title: "Exporting undefined name",
                description: "Attempting to export a name that is not defined",
                suggestion: Some("Define the name before exporting it"),
                help_url: Some("https://docs.coral-lang.org/errors/E5100"),
            },
            ErrorKind::DuplicateExport { .. } => ErrorMetadata {
                code: ErrorCode::E5101,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "ExportError",
                title: "Duplicate export",
                description: "The name is exported more than once",
                suggestion: Some("Remove duplicate exports"),
                help_url: Some("https://docs.coral-lang.org/errors/E5101"),
            },
            ErrorKind::InvalidIntrospection { .. } => ErrorMetadata {
                code: ErrorCode::E5102,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "TypeError",
                title: "Invalid module introspection function",
                description: "The introspection function is not valid",
                suggestion: Some("Use valid introspection functions like __all__, __dir__, etc."),
                help_url: Some("https://docs.coral-lang.org/errors/E5102"),
            },
            ErrorKind::ExportFromNonExistentModule { .. } => ErrorMetadata {
                code: ErrorCode::E5103,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "ModuleNotFoundError",
                title: "Export from non-existent module",
                description: "Attempting to export from a module that doesn't exist",
                suggestion: Some("Check that the module exists"),
                help_url: Some("https://docs.coral-lang.org/errors/E5103"),
            },
            ErrorKind::InvalidExportSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E5104,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "SyntaxError",
                title: "Invalid export syntax",
                description: "The export syntax is not valid",
                suggestion: Some("Check the export statement syntax"),
                help_url: Some("https://docs.coral-lang.org/errors/E5104"),
            },

            // Concurrency errors
            ErrorKind::DataRace { .. } => ErrorMetadata {
                code: ErrorCode::E6001,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Data race detected",
                description: "Shared mutable data accessed without synchronization",
                suggestion: Some(
                    "Use locks (Lock, RLock) or atomic types to protect shared mutable state",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E6001"),
            },
            ErrorKind::PotentialDeadlock { .. } => ErrorMetadata {
                code: ErrorCode::E6002,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Potential deadlock detected",
                description: "Circular lock dependency could lead to deadlock",
                suggestion: Some("Acquire locks in consistent order to avoid deadlocks"),
                help_url: Some("https://docs.coral-lang.org/errors/E6002"),
            },
            ErrorKind::NonSendType { .. } => ErrorMetadata {
                code: ErrorCode::E6003,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "TypeError",
                title: "Non-Send type cannot cross thread boundaries",
                description: "This type cannot be safely sent to another thread",
                suggestion: Some("Only Send types can be transferred between threads"),
                help_url: Some("https://docs.coral-lang.org/errors/E6003"),
            },
            ErrorKind::NonSyncType { .. } => ErrorMetadata {
                code: ErrorCode::E6004,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "TypeError",
                title: "Non-Sync type cannot be shared between threads",
                description: "This type cannot be safely shared between threads",
                suggestion: Some("Only Sync types can be shared between threads"),
                help_url: Some("https://docs.coral-lang.org/errors/E6004"),
            },
            ErrorKind::LockNotReleased { .. } => ErrorMetadata {
                code: ErrorCode::E6005,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Lock acquired but never released",
                description: "A lock was acquired but no corresponding release was found",
                suggestion: Some("Use 'with' statement to automatically release locks"),
                help_url: Some("https://docs.coral-lang.org/errors/E6005"),
            },
            ErrorKind::UnsynchronizedAccess { .. } => ErrorMetadata {
                code: ErrorCode::E6006,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Unsynchronized access to shared data",
                description: "Shared data accessed without holding the required lock",
                suggestion: Some("Acquire the lock before accessing shared data"),
                help_url: Some("https://docs.coral-lang.org/errors/E6006"),
            },
            ErrorKind::DoubleLock { .. } => ErrorMetadata {
                code: ErrorCode::E6007,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Double lock acquisition",
                description: "Attempting to acquire the same lock twice",
                suggestion: Some(
                    "Use RLock for reentrant locking or restructure code to avoid double acquisition",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E6007"),
            },
            ErrorKind::LockOrderViolation { .. } => ErrorMetadata {
                code: ErrorCode::E6008,
                severity: Severity::Error,
                category: ErrorCategory::Concurrency,
                error_type: "ConcurrencyError",
                title: "Lock order violation",
                description: "Locks acquired in inconsistent order",
                suggestion: Some("Always acquire locks in the same order to prevent deadlocks"),
                help_url: Some("https://docs.coral-lang.org/errors/E6008"),
            },

            // Memory Safety errors
            ErrorKind::UseAfterFree { .. } => ErrorMetadata {
                code: ErrorCode::E7001,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Use after free",
                description: "Variable used after being freed/deallocated",
                suggestion: Some(
                    "Coral automatically tracks lifetimes to prevent use-after-free bugs",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E7001"),
            },
            ErrorKind::DoubleFree { .. } => ErrorMetadata {
                code: ErrorCode::E7002,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Double free",
                description: "Attempting to free already freed memory",
                suggestion: Some("Memory is automatically managed; avoid manual memory management"),
                help_url: Some("https://docs.coral-lang.org/errors/E7002"),
            },
            ErrorKind::ResourceLeak { .. } => ErrorMetadata {
                code: ErrorCode::E7003,
                severity: Severity::Warning,
                category: ErrorCategory::MemorySafety,
                error_type: "ResourceWarning",
                title: "Resource leak detected",
                description: "Resource not properly cleaned up",
                suggestion: Some(
                    "Use 'with' statement to ensure resources are properly cleaned up",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E7003"),
            },
            ErrorKind::DanglingReference { .. } => ErrorMetadata {
                code: ErrorCode::E7004,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Dangling reference",
                description: "Reference to variable that went out of scope",
                suggestion: Some("Ensure references don't outlive the data they point to"),
                help_url: Some("https://docs.coral-lang.org/errors/E7004"),
            },
            ErrorKind::CircularReference { .. } => ErrorMetadata {
                code: ErrorCode::E7005,
                severity: Severity::Warning,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryWarning",
                title: "Circular reference detected",
                description: "Circular reference that cannot be automatically freed",
                suggestion: Some("Break circular references or use weak references"),
                help_url: Some("https://docs.coral-lang.org/errors/E7005"),
            },
            ErrorKind::InvalidLifetime { .. } => ErrorMetadata {
                code: ErrorCode::E7006,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "LifetimeError",
                title: "Invalid lifetime",
                description: "Variable has invalid lifetime",
                suggestion: Some("Ensure variable lifetimes are correctly specified"),
                help_url: Some("https://docs.coral-lang.org/errors/E7006"),
            },
            ErrorKind::MovedValueUsed { .. } => ErrorMetadata {
                code: ErrorCode::E7007,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Value used after move",
                description: "Variable used after its value was moved",
                suggestion: Some("Don't use a variable after moving its value"),
                help_url: Some("https://docs.coral-lang.org/errors/E7007"),
            },

            // Pattern Matching errors
            ErrorKind::NonExhaustiveMatch { .. } => ErrorMetadata {
                code: ErrorCode::E8001,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "MatchError",
                title: "Match statement is not exhaustive",
                description: "Not all possible patterns are covered",
                suggestion: Some("Add patterns to cover all cases or add a wildcard pattern"),
                help_url: Some("https://docs.coral-lang.org/errors/E8001"),
            },
            ErrorKind::UnreachablePattern { .. } => ErrorMetadata {
                code: ErrorCode::E8002,
                severity: Severity::Warning,
                category: ErrorCategory::PatternMatching,
                error_type: "MatchWarning",
                title: "Unreachable pattern",
                description: "This pattern will never match because a previous pattern already covers it",
                suggestion: Some("Remove or reorder patterns to make this pattern reachable"),
                help_url: Some("https://docs.coral-lang.org/errors/E8002"),
            },
            ErrorKind::PatternTypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E8003,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "TypeError",
                title: "Pattern type doesn't match subject type",
                description: "The pattern's type is incompatible with the value being matched",
                suggestion: Some("Use a pattern that matches the subject's type"),
                help_url: Some("https://docs.coral-lang.org/errors/E8003"),
            },
            ErrorKind::InvalidPatternSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E8004,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "SyntaxError",
                title: "Invalid pattern syntax",
                description: "The pattern syntax is not valid",
                suggestion: Some("Check the pattern syntax"),
                help_url: Some("https://docs.coral-lang.org/errors/E8004"),
            },
            ErrorKind::DuplicatePatternBinding { .. } => ErrorMetadata {
                code: ErrorCode::E8005,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "SyntaxError",
                title: "Duplicate pattern binding",
                description: "The same name is bound multiple times in a pattern",
                suggestion: Some("Use unique names for pattern bindings"),
                help_url: Some("https://docs.coral-lang.org/errors/E8005"),
            },

            // Decorator errors
            ErrorKind::DecoratorNotFound { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "NameError",
                title: "Decorator not found",
                description: "The decorator is not defined",
                suggestion: Some("Define the decorator or import it"),
                help_url: Some("https://docs.coral-lang.org/errors/E9001"),
            },
            ErrorKind::UndefinedDecorator { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "NameError",
                title: "Undefined decorator",
                description: "The decorator is not defined",
                suggestion: Some("Define the decorator or import it"),
                help_url: Some("https://docs.coral-lang.org/errors/E9001"),
            },
            ErrorKind::DuplicateDecorator { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Warning,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorWarning",
                title: "Duplicate decorator",
                description: "The same decorator is applied more than once",
                suggestion: Some("Remove duplicate decorators"),
                help_url: Some("https://docs.coral-lang.org/errors/E9001"),
            },
            ErrorKind::InvalidDecoratorExpression { .. } => ErrorMetadata {
                code: ErrorCode::E9002,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "SyntaxError",
                title: "Invalid decorator expression",
                description: "The decorator expression is not valid",
                suggestion: Some("Use a valid decorator expression (function name or call)"),
                help_url: Some("https://docs.coral-lang.org/errors/E9002"),
            },
            ErrorKind::InvalidDecoratorOrder { .. } => ErrorMetadata {
                code: ErrorCode::E9005,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator order",
                description: "Decorators are in invalid order",
                suggestion: Some("Reorder decorators according to requirements"),
                help_url: Some("https://docs.coral-lang.org/errors/E9005"),
            },
            ErrorKind::InvalidDecoratorTarget { .. } => ErrorMetadata {
                code: ErrorCode::E9002,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator target",
                description: "This decorator cannot be applied to this target",
                suggestion: Some("Use the decorator on a valid target"),
                help_url: Some("https://docs.coral-lang.org/errors/E9002"),
            },
            ErrorKind::DecoratorSignatureMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E9003,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "TypeError",
                title: "Decorator signature mismatch",
                description: "The decorated function's signature doesn't match decorator requirements",
                suggestion: Some(
                    "Adjust the function signature to match the decorator's requirements",
                ),
                help_url: Some("https://docs.coral-lang.org/errors/E9003"),
            },
            ErrorKind::DecoratorApplicationFailed { .. } => ErrorMetadata {
                code: ErrorCode::E9004,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Decorator application failed",
                description: "Failed to apply the decorator",
                suggestion: Some("Check the decorator implementation"),
                help_url: Some("https://docs.coral-lang.org/errors/E9004"),
            },
            ErrorKind::InvalidDecoratorComposition { .. } => ErrorMetadata {
                code: ErrorCode::E9005,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator composition",
                description: "These decorators cannot be composed together",
                suggestion: Some("Remove incompatible decorators or change their order"),
                help_url: Some("https://docs.coral-lang.org/errors/E9005"),
            },
        }
    }

    /// Format a human-readable message for this error kind.
    pub fn format_message(&self) -> String {
        match self {
            // Lexical errors
            ErrorKind::InvalidCharacter => "Invalid character in source code".to_string(),
            ErrorKind::UnterminatedString => {
                "String literal is not properly terminated".to_string()
            }
            ErrorKind::InvalidNumber => "Number literal has invalid format".to_string(),
            ErrorKind::InvalidIdentifier => "Identifier name is not valid".to_string(),
            ErrorKind::MixedTabsAndSpaces => {
                "Inconsistent use of tabs and spaces in indentation".to_string()
            }
            ErrorKind::InvalidEscapeSequence => "Invalid escape sequence in string".to_string(),

            // Syntax errors
            ErrorKind::UnexpectedToken { expected, found } => {
                if let Some(expected) = expected {
                    format!("Unexpected token '{}', expected '{}'", found, expected)
                } else {
                    format!("Unexpected token '{}'", found)
                }
            }
            ErrorKind::ExpectedToken { expected, found } => {
                format!("Expected '{}', found '{}'", expected, found)
            }
            ErrorKind::UnexpectedEof => "Unexpected end of file".to_string(),
            ErrorKind::InvalidSyntax { message } => format!("Invalid syntax: {}", message),
            ErrorKind::IndentationError => "Indentation error".to_string(),
            ErrorKind::UnclosedDelimiter { expected, .. } => {
                format!("Unclosed delimiter, expected '{}'", expected)
            }
            ErrorKind::UnmatchedClosing { delimiter } => {
                format!("Unmatched closing delimiter '{}'", delimiter)
            }
            ErrorKind::MissingColon { context } => {
                format!("Missing ':' after {}", context)
            }
            ErrorKind::ExpectedExpression => "Expected an expression".to_string(),
            ErrorKind::InvalidAssignmentTarget => "Invalid assignment target".to_string(),
            ErrorKind::BreakOutsideLoop => "'break' outside loop".to_string(),
            ErrorKind::ContinueOutsideLoop => "'continue' outside loop".to_string(),
            ErrorKind::ReturnOutsideFunction => "'return' outside function".to_string(),
            ErrorKind::YieldOutsideFunction => "'yield' outside function".to_string(),
            ErrorKind::AwaitOutsideAsync => "'await' outside async function".to_string(),
            ErrorKind::AsyncForOutsideAsync => "'async for' outside async function".to_string(),
            ErrorKind::AsyncWithOutsideAsync => "'async with' outside async function".to_string(),
            ErrorKind::DuplicateParameter { name } => {
                format!("Duplicate parameter name '{}'", name)
            }
            ErrorKind::DuplicateArgument { name } => {
                format!("Duplicate keyword argument '{}'", name)
            }
            ErrorKind::PositionalAfterKeyword => {
                "Positional argument after keyword argument".to_string()
            }
            ErrorKind::InvalidParameterOrder => "Invalid parameter order".to_string(),
            ErrorKind::MixedExceptSyntax => "Cannot mix 'except' and 'except*'".to_string(),
            ErrorKind::BareExceptStar => "'except*' requires exception type".to_string(),
            ErrorKind::FutureImportNotFirst => {
                "'from __future__ import' must be at beginning of file".to_string()
            }
            ErrorKind::RelativeImportBeyondTopLevel => {
                "Relative import beyond top-level package".to_string()
            }
            ErrorKind::InvalidRelativeImport => "Invalid relative import syntax".to_string(),
            ErrorKind::UnexpectedIndent => "Unexpected indent".to_string(),
            ErrorKind::ExpectedIndent => "Expected an indented block".to_string(),
            ErrorKind::UnindentMismatch => {
                "Unindent does not match any outer indentation level".to_string()
            }

            // Name resolution errors
            ErrorKind::UndefinedName { name } => format!("Name '{}' is not defined", name),
            ErrorKind::DuplicateDefinition { name, .. } => {
                format!("Name '{}' is already defined", name)
            }
            ErrorKind::NonlocalWithoutEnclosing { name } => {
                format!("No binding for nonlocal '{}' found", name)
            }
            ErrorKind::UsedBeforeDefinition { name } => {
                format!("Name '{}' used before definition", name)
            }

            // Control flow errors
            ErrorKind::UnreachableCode { reason } => {
                format!("Unreachable code: {}", reason)
            }
            ErrorKind::InfiniteLoop { reason } => {
                format!("Infinite loop detected: {}", reason)
            }
            ErrorKind::MissingReturn {
                function,
                expected_type,
            } => {
                format!("Function '{}' must return '{}'", function, expected_type)
            }
            ErrorKind::InconsistentReturnTypes { function } => {
                format!("Function '{}' has inconsistent return types", function)
            }
            ErrorKind::DeadCodeAfterReturn => "Dead code after return statement".to_string(),
            ErrorKind::UnreachableExceptionHandler { exception_type } => {
                format!("Exception handler for '{}' is unreachable", exception_type)
            }

            // Type system errors
            ErrorKind::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected '{}', found '{}'", expected, found)
            }
            ErrorKind::IncompatibleBinOp { op, left, right } => {
                format!(
                    "Invalid operation '{}' for types '{}' and '{}'",
                    op, left, right
                )
            }
            ErrorKind::IncompatibleUnaryOp { op, operand } => {
                format!("Invalid operation '{}' for type '{}'", op, operand)
            }
            ErrorKind::ArgumentCountMismatch { expected, found } => {
                format!("Expected {} arguments, found {}", expected, found)
            }
            ErrorKind::InvalidArgumentType {
                param_index,
                expected,
                found,
            } => {
                format!(
                    "Argument {} has wrong type: expected '{}', found '{}'",
                    param_index, expected, found
                )
            }
            ErrorKind::ReturnTypeMismatch { expected, found } => {
                format!(
                    "Return type mismatch: expected '{}', found '{}'",
                    expected, found
                )
            }
            ErrorKind::InvalidSubscript { container, index } => {
                format!("Cannot subscript '{}' with '{}'", container, index)
            }
            ErrorKind::InvalidAttribute {
                obj_type,
                attribute,
            } => {
                format!("Type '{}' has no attribute '{}'", obj_type, attribute)
            }
            ErrorKind::NotCallable { type_name } => {
                format!("'{}' object is not callable", type_name)
            }
            ErrorKind::NotSubscriptable { type_name } => {
                format!("'{}' object is not subscriptable", type_name)
            }
            ErrorKind::NotIterable { type_name } => {
                format!("'{}' object is not iterable", type_name)
            }
            ErrorKind::CannotInferType { context } => {
                format!("Cannot infer type for {}", context)
            }
            ErrorKind::AmbiguousType { context } => {
                format!("Ambiguous type for {}", context)
            }
            ErrorKind::TypeAnnotationRequired { context } => {
                format!("Type annotation required for {}", context)
            }
            ErrorKind::InvalidTypeAnnotation { annotation } => {
                format!("Invalid type annotation: '{}'", annotation)
            }

            // Protocol errors
            ErrorKind::MissingProtocolMethod {
                class_name,
                protocol_name,
                method_name,
            } => {
                format!(
                    "Class '{}' doesn't implement protocol '{}' method '{}'",
                    class_name, protocol_name, method_name
                )
            }
            ErrorKind::MethodSignatureMismatch {
                class_name,
                protocol_name,
                method_name,
                expected,
                found,
            } => {
                format!(
                    "Method '{}' in class '{}' doesn't match protocol '{}': expected '{}', found '{}'",
                    method_name, class_name, protocol_name, expected, found
                )
            }
            ErrorKind::ProtocolWithImplementation {
                protocol_name,
                method_name,
            } => {
                format!(
                    "Protocol '{}' cannot have implementation for method '{}'",
                    protocol_name, method_name
                )
            }
            ErrorKind::InvalidProtocol {
                protocol_name,
                reason,
            } => {
                format!("Invalid protocol '{}': {}", protocol_name, reason)
            }
            ErrorKind::ProtocolNotSatisfied {
                protocol_name,
                class_name,
            } => {
                format!(
                    "Class '{}' does not satisfy protocol '{}'",
                    class_name, protocol_name
                )
            }
            ErrorKind::RuntimeCheckableProtocolViolation { protocol_name } => {
                format!("Runtime checkable protocol '{}' violation", protocol_name)
            }

            // Import errors
            ErrorKind::ModuleNotFound { module_name } => {
                format!("No module named '{}'", module_name)
            }
            ErrorKind::CircularImport { cycle } => {
                format!("Circular import detected: {}", cycle.join(" -> "))
            }
            ErrorKind::InvalidImportSyntax { syntax } => {
                format!("Invalid import syntax: '{}'", syntax)
            }
            ErrorKind::ImportFromNonModule { name } => {
                format!("Cannot import from non-module '{}'", name)
            }
            ErrorKind::CannotImportName { name, module } => {
                format!("Cannot import name '{}' from '{}'", name, module)
            }
            ErrorKind::RelativeImportInNonPackage => {
                "Attempted relative import in non-package".to_string()
            }

            // Module system errors
            ErrorKind::ExportUndefined { name } => {
                format!("Exporting undefined name '{}'", name)
            }
            ErrorKind::DuplicateExport { name, .. } => {
                format!("Duplicate export of '{}'", name)
            }
            ErrorKind::InvalidIntrospection {
                function,
                suggestion,
            } => {
                if let Some(suggestion) = suggestion {
                    format!(
                        "Invalid introspection function '{}', try '{}'",
                        function, suggestion
                    )
                } else {
                    format!("Invalid introspection function '{}'", function)
                }
            }
            ErrorKind::ExportFromNonExistentModule { module } => {
                format!("Cannot export from non-existent module '{}'", module)
            }
            ErrorKind::InvalidExportSyntax { syntax } => {
                format!("Invalid export syntax: '{}'", syntax)
            }

            // Concurrency errors
            ErrorKind::DataRace {
                var_name,
                access_type,
                ..
            } => {
                format!(
                    "Data race detected: '{}' accessed {} without proper synchronization",
                    var_name, access_type
                )
            }
            ErrorKind::PotentialDeadlock { lock_chain, .. } => {
                format!("Potential deadlock: {}", lock_chain.join(" -> "))
            }
            ErrorKind::NonSendType {
                var_name,
                type_name,
            } => {
                format!(
                    "Cannot send '{}' of type '{}' across threads",
                    var_name, type_name
                )
            }
            ErrorKind::NonSyncType {
                var_name,
                type_name,
            } => {
                format!(
                    "Cannot share '{}' of type '{}' between threads",
                    var_name, type_name
                )
            }
            ErrorKind::LockNotReleased { lock_name } => {
                format!("Lock '{}' acquired but never released", lock_name)
            }
            ErrorKind::UnsynchronizedAccess {
                var_name,
                required_lock,
            } => {
                format!(
                    "Unsynchronized access to '{}' (requires lock '{}')",
                    var_name, required_lock
                )
            }
            ErrorKind::DoubleLock { lock_name, .. } => {
                format!("Attempting to acquire lock '{}' twice", lock_name)
            }
            ErrorKind::LockOrderViolation { lock1, lock2 } => {
                format!("Lock order violation: '{}' and '{}'", lock1, lock2)
            }

            // Memory safety errors
            ErrorKind::UseAfterFree { var_name, .. } => {
                format!("Variable '{}' used after being freed", var_name)
            }
            ErrorKind::DoubleFree { var_name, .. } => {
                format!("Attempting to free '{}' twice", var_name)
            }
            ErrorKind::ResourceLeak {
                resource_type,
                var_name,
            } => {
                format!(
                    "Resource leak: '{}' of type '{}' not properly cleaned up",
                    var_name, resource_type
                )
            }
            ErrorKind::DanglingReference { var_name, .. } => {
                format!("Dangling reference to '{}'", var_name)
            }
            ErrorKind::CircularReference { var_names } => {
                format!("Circular reference: {}", var_names.join(" -> "))
            }
            ErrorKind::InvalidLifetime { var_name } => {
                format!("Invalid lifetime for '{}'", var_name)
            }
            ErrorKind::MovedValueUsed { var_name } => {
                format!("Value '{}' used after move", var_name)
            }

            // Pattern matching errors
            ErrorKind::NonExhaustiveMatch { missing_patterns } => {
                format!(
                    "Non-exhaustive match, missing: {}",
                    missing_patterns.join(", ")
                )
            }
            ErrorKind::UnreachablePattern { reason } => {
                format!("Unreachable pattern: {}", reason)
            }
            ErrorKind::PatternTypeMismatch { expected, found } => {
                format!(
                    "Pattern type mismatch: expected '{}', found '{}'",
                    expected, found
                )
            }
            ErrorKind::InvalidPatternSyntax { pattern } => {
                format!("Invalid pattern syntax: '{}'", pattern)
            }
            ErrorKind::DuplicatePatternBinding { name } => {
                format!("Duplicate pattern binding '{}'", name)
            }

            // Decorator errors
            ErrorKind::DecoratorNotFound { name } => {
                format!("Decorator '{}' not found", name)
            }
            ErrorKind::UndefinedDecorator { name } => {
                format!("Undefined decorator '{}'", name)
            }
            ErrorKind::DuplicateDecorator { name, .. } => {
                format!("Duplicate decorator '{}'", name)
            }
            ErrorKind::InvalidDecoratorExpression { decorator } => {
                format!("Invalid decorator expression: '{}'", decorator)
            }
            ErrorKind::InvalidDecoratorOrder { reason } => {
                format!("Invalid decorator order: {}", reason)
            }
            ErrorKind::InvalidDecoratorTarget { decorator, target } => {
                format!("Cannot apply decorator '{}' to '{}'", decorator, target)
            }
            ErrorKind::DecoratorSignatureMismatch {
                decorator,
                expected,
                found,
            } => {
                format!(
                    "Decorator '{}' signature mismatch: expected '{}', found '{}'",
                    decorator, expected, found
                )
            }
            ErrorKind::DecoratorApplicationFailed { decorator, reason } => {
                format!("Failed to apply decorator '{}': {}", decorator, reason)
            }
            ErrorKind::InvalidDecoratorComposition { decorators } => {
                format!("Invalid decorator composition: {}", decorators.join(", "))
            }
        }
    }
}
