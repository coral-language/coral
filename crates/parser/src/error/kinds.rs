//! Comprehensive error kind definitions for all Coral compiler errors.
//!
//! This module contains the unified ErrorKind enum that covers all possible
//! errors from lexical analysis through semantic analysis.

use text_size::TextRange;

/// Comprehensive error kind enum covering all compiler errors.
#[derive(Debug, Clone)]
pub enum ErrorKind {
    /// Invalid character in source code
    InvalidCharacter,

    /// String literal is not properly terminated
    UnterminatedString,

    /// Number literal has invalid format
    InvalidNumber,

    /// Identifier name is not valid
    InvalidIdentifier,

    /// Invalid escape sequence in string
    InvalidEscapeSequence,

    /// Unexpected token encountered during parsing
    UnexpectedToken {
        expected: Option<String>,
        found: String,
    },

    /// Expected a specific token but found something else
    ExpectedToken { expected: String, found: String },

    /// File ended unexpectedly while parsing
    UnexpectedEof,

    /// Invalid syntax structure
    InvalidSyntax { message: String },

    /// Indentation is inconsistent or incorrect
    IndentationError,

    /// Opening delimiter has no matching closing delimiter
    UnclosedDelimiter {
        expected: char,
        opening_span: TextRange,
    },

    /// Closing delimiter has no matching opening delimiter
    UnmatchedClosing { delimiter: char },

    /// Expected ':' after context
    MissingColon { context: String },

    /// Expected expression
    ExpectedExpression,

    /// Invalid assignment target
    InvalidAssignmentTarget,

    /// 'break' can only be used inside loops
    BreakOutsideLoop,

    /// 'continue' can only be used inside loops
    ContinueOutsideLoop,

    /// 'return' can only be used inside functions
    ReturnOutsideFunction,

    /// 'yield' can only be used inside functions
    YieldOutsideFunction,

    /// 'await' can only be used inside async functions
    AwaitOutsideAsync,

    /// 'async for' can only be used inside async functions
    AsyncForOutsideAsync,

    /// 'async with' can only be used inside async functions
    AsyncWithOutsideAsync,

    /// Duplicate parameter name
    DuplicateParameter { name: String },

    /// Duplicate keyword argument
    DuplicateArgument { name: String },

    /// Positional argument cannot follow keyword argument
    PositionalAfterKeyword,

    /// Parameters are in invalid order
    InvalidParameterOrder,

    /// Cannot mix except and except* in the same try statement
    MixedExceptSyntax,

    /// except* requires a specific exception type, not a bare except
    BareExceptStar,

    /// 'from __future__ import' must occur at the beginning of the file
    FutureImportNotFirst,

    /// Relative import cannot go beyond top-level package
    RelativeImportBeyondTopLevel,

    /// Invalid relative import syntax
    InvalidRelativeImport,

    /// Unexpected indent
    UnexpectedIndent,

    /// Expected an indented block
    ExpectedIndent,

    /// Unindent does not match any outer indentation level
    UnindentMismatch,

    /// Name is used before being defined
    UndefinedName { name: String },

    /// Name is already defined in this scope
    DuplicateDefinition {
        name: String,
        previous_span: TextRange,
    },

    /// nonlocal declaration without enclosing scope
    NonlocalWithoutEnclosing { name: String },

    /// Name is used before definition
    UsedBeforeDefinition { name: String },

    /// Code is unreachable
    UnreachableCode { reason: String },

    /// Infinite loop detected
    InfiniteLoop { reason: String },

    /// Missing return statement
    MissingReturn {
        function: String,
        expected_type: String,
    },

    /// Inconsistent return types
    InconsistentReturnTypes { function: String },

    /// Dead code after return statement
    DeadCodeAfterReturn,

    /// Unreachable exception handler
    UnreachableExceptionHandler { exception_type: String },

    /// Variable used before being initialized
    UninitializedVariable { var_name: String },

    /// Variable may not be initialized on all paths
    ConditionallyUninitializedVariable {
        var_name: String,
        missing_paths: Vec<String>,
    },

    /// Variable used in its own initialization
    SelfReferentialInitialization { var_name: String },

    /// Constant condition always evaluates to the same value
    ConstantCondition { value: bool, suggestion: String },

    /// Dead branch due to constant condition
    DeadBranch { branch_type: String, reason: String },

    /// Expression can be simplified
    SimplifiableExpression {
        original: String,
        simplified: String,
    },

    /// Type mismatch between expected and actual types
    TypeMismatch { expected: String, found: String },

    /// Incompatible types in binary operation
    IncompatibleBinOp {
        op: String,
        left: String,
        right: String,
    },

    /// Incompatible type in unary operation
    IncompatibleUnaryOp { op: String, operand: String },

    /// Wrong number of arguments
    ArgumentCountMismatch { expected: usize, found: usize },

    /// Invalid argument type
    InvalidArgumentType {
        param_index: usize,
        expected: String,
        found: String,
    },

    /// Unexpected keyword argument in function call
    UnexpectedKeywordArgument { name: String },

    /// Return type doesn't match function signature
    ReturnTypeMismatch { expected: String, found: String },

    /// Invalid subscript operation
    InvalidSubscript { container: String, index: String },

    /// Invalid attribute access
    InvalidAttribute { obj_type: String, attribute: String },

    /// Cannot call non-callable
    NotCallable { type_name: String },

    /// Cannot subscript non-subscriptable
    NotSubscriptable { type_name: String },

    /// Cannot iterate non-iterable
    NotIterable { type_name: String },

    /// Cannot infer type
    CannotInferType { context: String },

    /// Ambiguous type
    AmbiguousType { context: String },

    /// Type annotation required
    TypeAnnotationRequired { context: String },

    /// Invalid type annotation
    InvalidTypeAnnotation { annotation: String },

    /// Attempt to assign to read-only property
    ReadOnlyProperty { name: String, class_name: String },

    /// Property setter type mismatch
    PropertySetterTypeMismatch {
        property_name: String,
        expected: String,
        found: String,
    },

    /// Class doesn't implement required protocol method
    MissingProtocolMethod {
        class_name: String,
        protocol_name: String,
        method_name: String,
    },

    /// Class doesn't have required protocol attribute
    MissingProtocolAttribute {
        class_name: String,
        protocol_name: String,
        attribute_name: String,
    },
    IncompatibleProtocolAttribute {
        class_name: String,
        protocol_name: String,
        attribute_name: String,
        expected: String,
        found: String,
    },

    /// Method signature doesn't match protocol
    MethodSignatureMismatch {
        class_name: String,
        protocol_name: String,
        method_name: String,
        expected: String,
        found: String,
    },

    /// Protocol cannot have implementations
    ProtocolWithImplementation {
        protocol_name: String,
        method_name: String,
    },

    /// Invalid protocol definition
    InvalidProtocol {
        protocol_name: String,
        reason: String,
    },

    /// Protocol not satisfied
    ProtocolNotSatisfied {
        protocol_name: String,
        class_name: String,
    },

    /// Runtime checkable protocol violation
    RuntimeCheckableProtocolViolation { protocol_name: String },

    /// Module could not be found or loaded
    ModuleNotFound { module_name: String },

    /// Circular dependency detected between modules
    CircularImport { cycle: Vec<String> },

    /// Import statement has invalid syntax
    InvalidImportSyntax { syntax: String },

    /// Import from non-module
    ImportFromNonModule { name: String },

    /// Cannot import name
    CannotImportName { name: String, module: String },

    /// Relative import in non-package
    RelativeImportInNonPackage,

    /// Exporting undefined name
    ExportUndefined { name: String },

    /// Duplicate export
    DuplicateExport { name: String, first_span: TextRange },

    /// Invalid module introspection function
    InvalidIntrospection {
        function: String,
        suggestion: Option<String>,
    },

    /// Export from non-existent module
    ExportFromNonExistentModule { module: String },

    /// Invalid export syntax
    InvalidExportSyntax { syntax: String },

    /// Exported name not found in source module (for re-exports)
    ExportedNameNotInSourceModule { name: String, source_module: String },

    /// Circular re-export detected
    CircularReExport { cycle: Vec<String> },

    /// Blocking call detected in async context
    BlockingCallInAsync { call: String },
    /// Invalid Future type for await
    InvalidFutureType { expr: String, actual_type: String },
    /// Variable lifetime doesn't span await point
    AsyncLifetimeViolation { var: String },

    /// Variable used after being freed/deallocated
    UseAfterFree {
        var_name: String,
        free_span: TextRange,
    },

    /// Attempting to free already freed memory
    DoubleFree {
        var_name: String,
        first_free_span: TextRange,
    },

    /// Resource not properly cleaned up
    ResourceLeak {
        resource_type: String,
        var_name: String,
    },

    /// Reference to variable that went out of scope
    DanglingReference {
        var_name: String,
        scope_end_span: TextRange,
    },

    /// Circular reference that cannot be automatically freed
    CircularReference { var_names: Vec<String> },

    /// Invalid lifetime
    InvalidLifetime { var_name: String },

    /// Moved value used
    MovedValueUsed { var_name: String },

    /// Match statement is not exhaustive
    NonExhaustiveMatch { missing_patterns: Vec<String> },

    /// Pattern is unreachable
    UnreachablePattern { reason: String },

    /// Pattern type doesn't match subject type
    PatternTypeMismatch { expected: String, found: String },

    /// Invalid pattern syntax
    InvalidPatternSyntax { pattern: String },

    /// Duplicate pattern binding
    DuplicatePatternBinding { name: String },

    /// Decorator not found
    DecoratorNotFound { name: String },

    /// Undefined decorator
    UndefinedDecorator { name: String },

    /// Duplicate decorator
    DuplicateDecorator {
        name: String,
        first_span: TextRange,
        second_span: TextRange,
    },

    /// Invalid decorator expression
    InvalidDecoratorExpression { decorator: String },

    /// Invalid decorator order
    InvalidDecoratorOrder { reason: String },

    /// Invalid decorator target
    InvalidDecoratorTarget { decorator: String, target: String },

    /// Decorator signature mismatch
    DecoratorSignatureMismatch {
        decorator: String,
        expected: String,
        found: String,
    },

    /// Decorator application failed
    DecoratorApplicationFailed { decorator: String, reason: String },

    /// Invalid decorator composition
    InvalidDecoratorComposition { decorators: Vec<String> },
}
