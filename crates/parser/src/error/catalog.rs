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
}

impl ErrorKind {
    /// Get comprehensive metadata for this error kind.
    pub fn metadata(&self) -> ErrorMetadata {
        match self {
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
            },
            ErrorKind::InvalidEscapeSequence => ErrorMetadata {
                code: ErrorCode::E1006,
                severity: Severity::Error,
                category: ErrorCategory::Lexical,
                error_type: "SyntaxError",
                title: "Invalid escape sequence in string",
                description: "The string contains an invalid escape sequence",
                suggestion: Some("Use valid escape sequences like \\n, \\t, \\r, \\\\, \\', \\\""),
            },

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
            },
            ErrorKind::IndentationError => ErrorMetadata {
                code: ErrorCode::E2005,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Indentation is inconsistent or incorrect",
                description: "The indentation does not follow indentation-based syntax rules",
                suggestion: Some(
                    "Ensure all lines in a block have the same indentation level. Use either spaces or tabs consistently throughout your code.",
                ),
            },
            ErrorKind::UnclosedDelimiter { .. } => ErrorMetadata {
                code: ErrorCode::E2006,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Opening delimiter has no matching closing delimiter",
                description: "A bracket, parenthesis, or brace is not properly closed",
                suggestion: Some("Add the matching closing delimiter"),
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
            },
            ErrorKind::MissingColon { .. } => ErrorMetadata {
                code: ErrorCode::E2008,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Expected ':' after statement",
                description: "Compound statements like if, while, for, def, and class require a colon",
                suggestion: Some("Add a ':' at the end of the statement"),
            },
            ErrorKind::ExpectedExpression => ErrorMetadata {
                code: ErrorCode::E2009,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Expected an expression",
                description: "An expression was expected but not found",
                suggestion: Some("Provide a valid expression"),
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
            },
            ErrorKind::BreakOutsideLoop => ErrorMetadata {
                code: ErrorCode::E2011,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'break' statement outside loop",
                description: "'break' can only be used inside for or while loops",
                suggestion: Some("Move the 'break' statement inside a loop or remove it"),
            },
            ErrorKind::ContinueOutsideLoop => ErrorMetadata {
                code: ErrorCode::E2012,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'continue' statement outside loop",
                description: "'continue' can only be used inside for or while loops",
                suggestion: Some("Move the 'continue' statement inside a loop or remove it"),
            },
            ErrorKind::ReturnOutsideFunction => ErrorMetadata {
                code: ErrorCode::E2013,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'return' statement outside function",
                description: "'return' can only be used inside functions",
                suggestion: Some("Move the 'return' statement inside a function or remove it"),
            },
            ErrorKind::YieldOutsideFunction => ErrorMetadata {
                code: ErrorCode::E2014,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'yield' statement outside function",
                description: "'yield' can only be used inside functions",
                suggestion: Some("Move the 'yield' statement inside a function or remove it"),
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
            },
            ErrorKind::DuplicateParameter { .. } => ErrorMetadata {
                code: ErrorCode::E2018,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Duplicate parameter name",
                description: "A parameter name appears more than once in the function signature",
                suggestion: Some("Use unique names for all parameters"),
            },
            ErrorKind::DuplicateArgument { .. } => ErrorMetadata {
                code: ErrorCode::E2019,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Duplicate keyword argument",
                description: "A keyword argument is specified more than once",
                suggestion: Some("Remove duplicate keyword arguments"),
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
            },
            ErrorKind::InvalidParameterOrder => ErrorMetadata {
                code: ErrorCode::E2021,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "Invalid parameter order",
                description: "Parameters must be ordered: positional, *args, keyword-only, **kwargs",
                suggestion: Some("Reorder parameters according to parameter ordering rules"),
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
            },
            ErrorKind::BareExceptStar => ErrorMetadata {
                code: ErrorCode::E2023,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "SyntaxError",
                title: "'except*' requires exception type",
                description: "'except*' cannot be used without specifying an exception type",
                suggestion: Some("Specify an exception type: except* ExceptionType:"),
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
            },
            ErrorKind::RelativeImportBeyondTopLevel => ErrorMetadata {
                code: ErrorCode::E2025,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "ImportError",
                title: "Relative import goes beyond top-level package",
                description: "Too many leading dots in relative import",
                suggestion: Some("Reduce the number of leading dots in the relative import"),
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
            },
            ErrorKind::UnexpectedIndent => ErrorMetadata {
                code: ErrorCode::E2027,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Unexpected indent",
                description: "Found indentation where it wasn't expected",
                suggestion: Some("Remove the extra indentation"),
            },
            ErrorKind::ExpectedIndent => ErrorMetadata {
                code: ErrorCode::E2028,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Expected an indented block",
                description: "A compound statement requires an indented block",
                suggestion: Some("Add an indented block after the colon"),
            },
            ErrorKind::UnindentMismatch => ErrorMetadata {
                code: ErrorCode::E2029,
                severity: Severity::Error,
                category: ErrorCategory::Syntax,
                error_type: "IndentationError",
                title: "Unindent does not match any outer indentation level",
                description: "The dedent amount doesn't align with any previous indentation level in the nesting hierarchy",
                suggestion: Some(
                    "Indentation levels must decrease to match a previous level. Check your block structure and ensure consistent indentation.",
                ),
            },

            ErrorKind::UndefinedName { .. } => ErrorMetadata {
                code: ErrorCode::E3001,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "NameError",
                title: "Name is not defined",
                description: "The name is used before being defined in the current scope",
                suggestion: Some("Define the name before using it, or check for typos in the name"),
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
            },
            ErrorKind::UsedBeforeDefinition { .. } => ErrorMetadata {
                code: ErrorCode::E3004,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "NameError",
                title: "Name used before definition",
                description: "The name is used before it's assigned a value",
                suggestion: Some("Define or assign the name before using it"),
            },
            ErrorKind::UnreachableCode { .. } => ErrorMetadata {
                code: ErrorCode::W3100,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Code is unreachable",
                description: "This code will never be executed",
                suggestion: Some("Remove the unreachable code or fix the control flow"),
            },
            ErrorKind::InfiniteLoop { .. } => ErrorMetadata {
                code: ErrorCode::W3101,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Infinite loop detected",
                description: "This loop has no exit condition",
                suggestion: Some("Add a break statement or modify the loop condition"),
            },
            ErrorKind::MissingReturn { .. } => ErrorMetadata {
                code: ErrorCode::E3102,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "TypeError",
                title: "Missing return statement",
                description: "Function with return type annotation doesn't always return a value",
                suggestion: Some("Add a return statement in all code paths"),
            },
            ErrorKind::InconsistentReturnTypes { .. } => ErrorMetadata {
                code: ErrorCode::E3103,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "TypeError",
                title: "Inconsistent return types",
                description: "Function returns different types in different code paths",
                suggestion: Some("Ensure all return statements return the same type"),
            },
            ErrorKind::DeadCodeAfterReturn => ErrorMetadata {
                code: ErrorCode::E3104,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Dead code after return",
                description: "Code after return statement will never be executed",
                suggestion: Some("Remove the dead code or restructure the function"),
            },
            ErrorKind::UnreachableExceptionHandler { .. } => ErrorMetadata {
                code: ErrorCode::W3100,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "Warning",
                title: "Unreachable exception handler",
                description: "This exception handler will never be reached",
                suggestion: Some("Reorder exception handlers from most specific to most general"),
            },

            ErrorKind::UninitializedVariable { .. } => ErrorMetadata {
                code: ErrorCode::E3150,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "UninitializedVariable",
                title: "Use of uninitialized variable",
                description: "Variable is used before being assigned a value",
                suggestion: Some("Initialize the variable before using it"),
            },
            ErrorKind::ConditionallyUninitializedVariable { .. } => ErrorMetadata {
                code: ErrorCode::E3151,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "UninitializedVariable",
                title: "Variable may not be initialized",
                description: "Variable may not be initialized on all code paths",
                suggestion: Some("Ensure the variable is initialized in all branches"),
            },
            ErrorKind::SelfReferentialInitialization { .. } => ErrorMetadata {
                code: ErrorCode::E3152,
                severity: Severity::Error,
                category: ErrorCategory::Semantic,
                error_type: "SelfReferentialInitialization",
                title: "Self-referential initialization",
                description: "Variable is used in its own initialization",
                suggestion: Some("Use a different variable or initialize with a literal value"),
            },

            ErrorKind::ConstantCondition { .. } => ErrorMetadata {
                code: ErrorCode::W3170,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "ConstantCondition",
                title: "Constant condition",
                description: "Condition always evaluates to the same value",
                suggestion: Some("Remove the condition or simplify the code"),
            },
            ErrorKind::DeadBranch { .. } => ErrorMetadata {
                code: ErrorCode::W3171,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "DeadCode",
                title: "Dead branch",
                description: "Branch will never be executed due to constant condition",
                suggestion: Some("Remove the dead branch"),
            },
            ErrorKind::SimplifiableExpression { .. } => ErrorMetadata {
                code: ErrorCode::W3172,
                severity: Severity::Warning,
                category: ErrorCategory::Semantic,
                error_type: "SimplifiableExpression",
                title: "Expression can be simplified",
                description: "Expression contains constant values that can be folded",
                suggestion: Some("Simplify the expression to improve readability"),
            },

            ErrorKind::TypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4001,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Type mismatch",
                description: "Incompatible types for this operation",
                suggestion: Some("Make sure both operands are compatible types for this operation"),
            },
            ErrorKind::IncompatibleBinOp { .. } => ErrorMetadata {
                code: ErrorCode::E4002,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Incompatible types in binary operation",
                description: "The binary operation is not supported for these types",
                suggestion: Some("This operation may not be supported for the given type"),
            },
            ErrorKind::IncompatibleUnaryOp { .. } => ErrorMetadata {
                code: ErrorCode::E4003,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Incompatible type in unary operation",
                description: "The unary operation is not supported for this type",
                suggestion: Some("This unary operator may not be supported for the given type"),
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
            },
            ErrorKind::UnexpectedKeywordArgument { .. } => ErrorMetadata {
                code: ErrorCode::E4006,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Unexpected keyword argument",
                description: "Function does not accept this keyword argument",
                suggestion: Some("Check the function signature for available parameter names"),
            },
            ErrorKind::ReturnTypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4007,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Return type doesn't match function signature",
                description: "The returned value doesn't match the function's return type",
                suggestion: Some(
                    "Return a value that matches the function's return type annotation",
                ),
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
            },
            ErrorKind::InvalidAttribute { .. } => ErrorMetadata {
                code: ErrorCode::E4008,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "AttributeError",
                title: "Invalid attribute access",
                description: "Type has no such attribute",
                suggestion: Some("Check the type's available attributes"),
            },
            ErrorKind::NotCallable { .. } => ErrorMetadata {
                code: ErrorCode::E4009,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot call non-callable",
                description: "Attempting to call a value that is not a function",
                suggestion: Some("Only functions and callable objects can be called"),
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
            },
            ErrorKind::CannotInferType { .. } => ErrorMetadata {
                code: ErrorCode::E4012,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot infer type",
                description: "Type cannot be inferred from context",
                suggestion: Some("Add a type annotation to help the type checker"),
            },
            ErrorKind::AmbiguousType { .. } => ErrorMetadata {
                code: ErrorCode::E4013,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Ambiguous type",
                description: "Type is ambiguous and could be multiple things",
                suggestion: Some("Add a type annotation to clarify the intended type"),
            },
            ErrorKind::TypeAnnotationRequired { .. } => ErrorMetadata {
                code: ErrorCode::E4014,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Type annotation required",
                description: "A type annotation is required in this context",
                suggestion: Some("Add a type annotation"),
            },
            ErrorKind::InvalidTypeAnnotation { .. } => ErrorMetadata {
                code: ErrorCode::E4015,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Invalid type annotation",
                description: "The type annotation is not valid",
                suggestion: Some("Use a valid type annotation"),
            },
            ErrorKind::ReadOnlyProperty { .. } => ErrorMetadata {
                code: ErrorCode::E4016,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Cannot assign to read-only property",
                description: "Property does not have a setter and cannot be modified",
                suggestion: Some("Add a @property.setter method or use a different attribute"),
            },
            ErrorKind::PropertySetterTypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4017,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Property setter type mismatch",
                description: "The value type does not match the property setter parameter type",
                suggestion: Some("Ensure the assigned value matches the setter's expected type"),
            },

            ErrorKind::MissingProtocolMethod { .. } => ErrorMetadata {
                code: ErrorCode::E4100,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Class doesn't implement required protocol method",
                description: "The class is missing a method required by the protocol",
                suggestion: Some("Implement all methods required by the protocol"),
            },
            ErrorKind::MissingProtocolAttribute { .. } => ErrorMetadata {
                code: ErrorCode::E4101,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Class doesn't have required protocol attribute",
                description: "The class is missing an attribute required by the protocol",
                suggestion: Some("Add all attributes required by the protocol"),
            },
            ErrorKind::IncompatibleProtocolAttribute { .. } => ErrorMetadata {
                code: ErrorCode::E4102,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Incompatible protocol attribute type",
                description: "The attribute type doesn't match the protocol requirement",
                suggestion: Some("Change the attribute type to match the protocol"),
            },
            ErrorKind::MethodSignatureMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E4103,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Method signature doesn't match protocol",
                description: "The method signature doesn't match the protocol's requirements",
                suggestion: Some("Update the method signature to match the protocol"),
            },
            ErrorKind::ProtocolWithImplementation { .. } => ErrorMetadata {
                code: ErrorCode::E4103,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Protocol cannot have implementations",
                description: "Protocols should only declare method signatures, not implement them",
                suggestion: Some("Remove method implementations from the protocol"),
            },
            ErrorKind::InvalidProtocol { .. } => ErrorMetadata {
                code: ErrorCode::E4103,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Invalid protocol definition",
                description: "The protocol definition is not valid",
                suggestion: Some("Fix the protocol definition"),
            },
            ErrorKind::ProtocolNotSatisfied { .. } => ErrorMetadata {
                code: ErrorCode::E4104,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Protocol not satisfied",
                description: "The class does not satisfy the protocol requirements",
                suggestion: Some("Implement all required protocol methods with correct signatures"),
            },
            ErrorKind::RuntimeCheckableProtocolViolation { .. } => ErrorMetadata {
                code: ErrorCode::E4105,
                severity: Severity::Error,
                category: ErrorCategory::Protocol,
                error_type: "ProtocolError",
                title: "Runtime checkable protocol violation",
                description: "Value doesn't satisfy runtime checkable protocol",
                suggestion: Some("Ensure the value implements all required protocol methods"),
            },

            ErrorKind::ModuleNotFound { .. } => ErrorMetadata {
                code: ErrorCode::E5001,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ModuleNotFoundError",
                title: "Module not found",
                description: "The module could not be found or loaded",
                suggestion: Some("Check that the module exists and is in the import path"),
            },
            ErrorKind::CircularImport { .. } => ErrorMetadata {
                code: ErrorCode::E5002,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Circular import detected",
                description: "Modules have a circular dependency",
                suggestion: Some("Refactor the code to remove circular imports"),
            },
            ErrorKind::InvalidImportSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E5003,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "SyntaxError",
                title: "Invalid import syntax",
                description: "The import statement has invalid syntax",
                suggestion: Some("Check the import statement syntax"),
            },
            ErrorKind::ImportFromNonModule { .. } => ErrorMetadata {
                code: ErrorCode::E5004,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Import from non-module",
                description: "Attempting to import from something that is not a module",
                suggestion: Some("Only modules can be imported from"),
            },
            ErrorKind::CannotImportName { .. } => ErrorMetadata {
                code: ErrorCode::E5005,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Cannot import name",
                description: "The name cannot be imported from the module",
                suggestion: Some("Check that the name exists in the module"),
            },
            ErrorKind::RelativeImportInNonPackage => ErrorMetadata {
                code: ErrorCode::E5006,
                severity: Severity::Error,
                category: ErrorCategory::Import,
                error_type: "ImportError",
                title: "Relative import in non-package",
                description: "Relative imports can only be used in packages",
                suggestion: Some("Use absolute imports or convert to a package"),
            },

            ErrorKind::ExportUndefined { .. } => ErrorMetadata {
                code: ErrorCode::E5100,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "NameError",
                title: "Exporting undefined name",
                description: "Attempting to export a name that is not defined",
                suggestion: Some("Define the name before exporting it"),
            },
            ErrorKind::DuplicateExport { .. } => ErrorMetadata {
                code: ErrorCode::E5101,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "ExportError",
                title: "Duplicate export",
                description: "The name is exported more than once",
                suggestion: Some("Remove duplicate exports"),
            },
            ErrorKind::InvalidIntrospection { .. } => ErrorMetadata {
                code: ErrorCode::E5102,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "TypeError",
                title: "Invalid module introspection function",
                description: "The introspection function is not valid",
                suggestion: Some("Use valid introspection functions like __all__, __dir__, etc."),
            },
            ErrorKind::ExportFromNonExistentModule { .. } => ErrorMetadata {
                code: ErrorCode::E5103,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "ModuleNotFoundError",
                title: "Export from non-existent module",
                description: "Attempting to export from a module that doesn't exist",
                suggestion: Some("Check that the module exists"),
            },
            ErrorKind::InvalidExportSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E5104,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "SyntaxError",
                title: "Invalid export syntax",
                description: "The export syntax is not valid",
                suggestion: Some("Check the export statement syntax"),
            },
            ErrorKind::ExportedNameNotInSourceModule { .. } => ErrorMetadata {
                code: ErrorCode::E5105,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "NameError",
                title: "Exported name not found in source module",
                description: "The name being re-exported does not exist in the source module",
                suggestion: Some("Check that the name is exported from the source module"),
            },
            ErrorKind::CircularReExport { .. } => ErrorMetadata {
                code: ErrorCode::E5106,
                severity: Severity::Error,
                category: ErrorCategory::ModuleSystem,
                error_type: "CircularDependencyError",
                title: "Circular re-export detected",
                description: "A circular dependency was detected in the re-export chain",
                suggestion: Some("Break the circular dependency by removing one of the re-exports"),
            },

            ErrorKind::BlockingCallInAsync { .. } => ErrorMetadata {
                code: ErrorCode::E6001,
                severity: Severity::Warning,
                category: ErrorCategory::Concurrency,
                error_type: "BlockingCallWarning",
                title: "Blocking call in async context",
                description: "A blocking operation is called in an async function, which may block the async runtime",
                suggestion: Some(
                    "Use async-compatible alternatives (e.g., asyncio.sleep instead of time.sleep, aiofiles instead of open)",
                ),
            },
            ErrorKind::InvalidFutureType { .. } => ErrorMetadata {
                code: ErrorCode::E6002,
                severity: Severity::Error,
                category: ErrorCategory::Type,
                error_type: "TypeError",
                title: "Invalid Future type for await",
                description: "Cannot await a value that is not awaitable (does not return a Future or coroutine)",
                suggestion: Some(
                    "Ensure the awaited expression returns a Future, coroutine, or other awaitable type",
                ),
            },
            ErrorKind::AsyncLifetimeViolation { .. } => ErrorMetadata {
                code: ErrorCode::E6003,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "LifetimeError",
                title: "Variable lifetime does not span await point",
                description: "A variable's lifetime ends before an await point in an async function",
                suggestion: Some(
                    "Ensure variables are properly scoped or use cloning if necessary",
                ),
            },

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
            },
            ErrorKind::DoubleFree { .. } => ErrorMetadata {
                code: ErrorCode::E7002,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Double free",
                description: "Attempting to free already freed memory",
                suggestion: Some("Memory is automatically managed; avoid manual memory management"),
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
            },
            ErrorKind::DanglingReference { .. } => ErrorMetadata {
                code: ErrorCode::E7004,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Dangling reference",
                description: "Reference to variable that went out of scope",
                suggestion: Some("Ensure references don't outlive the data they point to"),
            },
            ErrorKind::CircularReference { .. } => ErrorMetadata {
                code: ErrorCode::E7005,
                severity: Severity::Warning,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryWarning",
                title: "Circular reference detected",
                description: "Circular reference that cannot be automatically freed",
                suggestion: Some("Break circular references or use weak references"),
            },
            ErrorKind::InvalidLifetime { .. } => ErrorMetadata {
                code: ErrorCode::E7006,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "LifetimeError",
                title: "Invalid lifetime",
                description: "Variable has invalid lifetime",
                suggestion: Some("Ensure variable lifetimes are correctly specified"),
            },
            ErrorKind::MovedValueUsed { .. } => ErrorMetadata {
                code: ErrorCode::E7007,
                severity: Severity::Error,
                category: ErrorCategory::MemorySafety,
                error_type: "MemoryError",
                title: "Value used after move",
                description: "Variable used after its value was moved",
                suggestion: Some("Don't use a variable after moving its value"),
            },

            ErrorKind::NonExhaustiveMatch { .. } => ErrorMetadata {
                code: ErrorCode::E8001,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "MatchError",
                title: "Match statement is not exhaustive",
                description: "Not all possible patterns are covered",
                suggestion: Some("Add patterns to cover all cases or add a wildcard pattern"),
            },
            ErrorKind::UnreachablePattern { .. } => ErrorMetadata {
                code: ErrorCode::E8002,
                severity: Severity::Warning,
                category: ErrorCategory::PatternMatching,
                error_type: "MatchWarning",
                title: "Unreachable pattern",
                description: "This pattern will never match because a previous pattern already covers it",
                suggestion: Some("Remove or reorder patterns to make this pattern reachable"),
            },
            ErrorKind::PatternTypeMismatch { .. } => ErrorMetadata {
                code: ErrorCode::E8003,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "TypeError",
                title: "Pattern type doesn't match subject type",
                description: "The pattern's type is incompatible with the value being matched",
                suggestion: Some("Use a pattern that matches the subject's type"),
            },
            ErrorKind::InvalidPatternSyntax { .. } => ErrorMetadata {
                code: ErrorCode::E8004,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "SyntaxError",
                title: "Invalid pattern syntax",
                description: "The pattern syntax is not valid",
                suggestion: Some("Check the pattern syntax"),
            },
            ErrorKind::DuplicatePatternBinding { .. } => ErrorMetadata {
                code: ErrorCode::E8005,
                severity: Severity::Error,
                category: ErrorCategory::PatternMatching,
                error_type: "SyntaxError",
                title: "Duplicate pattern binding",
                description: "The same name is bound multiple times in a pattern",
                suggestion: Some("Use unique names for pattern bindings"),
            },

            ErrorKind::DecoratorNotFound { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "NameError",
                title: "Decorator not found",
                description: "The decorator is not defined",
                suggestion: Some("Define the decorator or import it"),
            },
            ErrorKind::UndefinedDecorator { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "NameError",
                title: "Undefined decorator",
                description: "The decorator is not defined",
                suggestion: Some("Define the decorator or import it"),
            },
            ErrorKind::DuplicateDecorator { .. } => ErrorMetadata {
                code: ErrorCode::E9001,
                severity: Severity::Warning,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorWarning",
                title: "Duplicate decorator",
                description: "The same decorator is applied more than once",
                suggestion: Some("Remove duplicate decorators"),
            },
            ErrorKind::InvalidDecoratorExpression { .. } => ErrorMetadata {
                code: ErrorCode::E9002,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "SyntaxError",
                title: "Invalid decorator expression",
                description: "The decorator expression is not valid",
                suggestion: Some("Use a valid decorator expression (function name or call)"),
            },
            ErrorKind::InvalidDecoratorOrder { .. } => ErrorMetadata {
                code: ErrorCode::E9005,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator order",
                description: "Decorators are in invalid order",
                suggestion: Some("Reorder decorators according to requirements"),
            },
            ErrorKind::InvalidDecoratorTarget { .. } => ErrorMetadata {
                code: ErrorCode::E9002,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator target",
                description: "This decorator cannot be applied to this target",
                suggestion: Some("Use the decorator on a valid target"),
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
            },
            ErrorKind::DecoratorApplicationFailed { .. } => ErrorMetadata {
                code: ErrorCode::E9004,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Decorator application failed",
                description: "Failed to apply the decorator",
                suggestion: Some("Check the decorator implementation"),
            },
            ErrorKind::InvalidDecoratorComposition { .. } => ErrorMetadata {
                code: ErrorCode::E9005,
                severity: Severity::Error,
                category: ErrorCategory::Decorator,
                error_type: "DecoratorError",
                title: "Invalid decorator composition",
                description: "These decorators cannot be composed together",
                suggestion: Some("Remove incompatible decorators or change their order"),
            },
        }
    }

    /// Format a human-readable message for this error kind.
    pub fn format_message(&self) -> String {
        match self {
            ErrorKind::InvalidCharacter => "Invalid character in source code".to_string(),
            ErrorKind::UnterminatedString => {
                "String literal is not properly terminated".to_string()
            }
            ErrorKind::InvalidNumber => "Number literal has invalid format".to_string(),
            ErrorKind::InvalidIdentifier => "Identifier name is not valid".to_string(),
            ErrorKind::InvalidEscapeSequence => "Invalid escape sequence in string".to_string(),

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

            ErrorKind::UninitializedVariable { var_name } => {
                format!("Variable '{}' is used before being initialized", var_name)
            }
            ErrorKind::ConditionallyUninitializedVariable {
                var_name,
                missing_paths,
            } => {
                if missing_paths.is_empty() {
                    format!(
                        "Variable '{}' may not be initialized on all code paths",
                        var_name
                    )
                } else {
                    format!(
                        "Variable '{}' may not be initialized in: {}",
                        var_name,
                        missing_paths.join(", ")
                    )
                }
            }
            ErrorKind::SelfReferentialInitialization { var_name } => {
                format!("Variable '{}' is used in its own initialization", var_name)
            }

            ErrorKind::ConstantCondition { value, suggestion } => {
                format!("Condition always evaluates to '{}': {}", value, suggestion)
            }
            ErrorKind::DeadBranch {
                branch_type,
                reason,
            } => {
                format!("The '{}' branch is never executed: {}", branch_type, reason)
            }
            ErrorKind::SimplifiableExpression {
                original,
                simplified,
            } => {
                format!(
                    "Expression '{}' can be simplified to '{}'",
                    original, simplified
                )
            }

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
            ErrorKind::UnexpectedKeywordArgument { name } => {
                format!("Unexpected keyword argument '{}'", name)
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
            ErrorKind::ReadOnlyProperty { name, class_name } => {
                format!(
                    "Cannot assign to read-only property '{}' of class '{}'",
                    name, class_name
                )
            }
            ErrorKind::PropertySetterTypeMismatch {
                property_name,
                expected,
                found,
            } => {
                format!(
                    "Property '{}' setter expects type '{}', but got '{}'",
                    property_name, expected, found
                )
            }

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
            ErrorKind::MissingProtocolAttribute {
                class_name,
                protocol_name,
                attribute_name,
            } => {
                format!(
                    "Class '{}' doesn't have protocol '{}' attribute '{}'",
                    class_name, protocol_name, attribute_name
                )
            }
            ErrorKind::IncompatibleProtocolAttribute {
                class_name,
                protocol_name,
                attribute_name,
                expected,
                found,
            } => {
                format!(
                    "Class '{}' protocol '{}' attribute '{}' has incompatible type: expected '{}', found '{}'",
                    class_name, protocol_name, attribute_name, expected, found
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
            ErrorKind::ExportedNameNotInSourceModule {
                name,
                source_module,
            } => {
                format!(
                    "Name '{}' is not exported from module '{}'",
                    name, source_module
                )
            }
            ErrorKind::CircularReExport { cycle } => {
                format!("Circular re-export detected: {}", cycle.join(" -> "))
            }

            ErrorKind::BlockingCallInAsync { call } => {
                format!("Blocking call '{}' in async context", call)
            }
            ErrorKind::InvalidFutureType { expr, actual_type } => {
                format!(
                    "Cannot await '{}': expected awaitable type, found '{}'",
                    expr, actual_type
                )
            }
            ErrorKind::AsyncLifetimeViolation { var } => {
                format!("Variable '{}' may not be valid across await point", var)
            }

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
