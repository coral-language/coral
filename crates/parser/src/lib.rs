//! Coral: A production-ready parser with compile-time safety analysis
//!
//! Coral implements a complete lexer, tokenizer, and parser with:
//! - Arena allocation for efficient AST storage
//! - Incremental parsing support
//! - Error recovery with diagnostics
//! - Concurrent AST visitor pattern
//! - Immutable AST structure
//! - Comprehensive semantic analysis with 11 analysis passes
//! - Compile-time memory safety (ownership checking)
//! - Compile-time concurrency safety (data race & deadlock detection)
//!
//! # Semantic Analysis
//!
//! Coral provides a sophisticated semantic analysis system with:
//! - **Type inference and checking** - Automatic type inference with explicit type validation
//! - **Control flow analysis** - Detects unreachable code and validates return paths
//! - **Pattern match exhaustiveness** - Ensures all cases are handled
//! - **Ownership and lifetime analysis** - Prevents use-after-free and memory leaks
//! - **Async/await validation** - Validates async function usage and detects blocking calls in async contexts
//! - **Module system validation** - Validates imports and exports with transitive re-export resolution
//! - **Import resolution** - Resolves and validates module dependencies
//! - **Decorator validation** - Ensures correct decorator usage
//! - **Protocol checking** - Structural subtyping validation (PEP 544)
//!
//! All passes are enabled by default for production safety. Use [`PassManager`]
//! directly for custom configuration.
//!
//! # Unified Parse API
//!
//! The parser provides a unified API that returns comprehensive metadata including
//! the AST, diagnostics, symbol table, and module exports:
//!
//! ```no_run
//! use coral_parser::parse;
//!
//! let source = r#"
//! def greet(name: str) -> str:
//!     return f"Hello, {name}!"
//! "#;
//!
//! match parse(source) {
//!     Ok(result) => {
//!         println!("Parsed module with {} statements", result.module.body.len());
//!
//!         if result.has_errors() {
//!             for error in result.errors() {
//!                 eprintln!("{:?}", error);
//!             }
//!         }
//!
//!         // Access symbol table
//!         if let Some(symbol) = result.symbol_table.lookup("greet") {
//!             println!("Found function: {:?}", symbol);
//!         }
//!     }
//!     Err(error) => eprintln!("Parse error: {}", error),
//! }
//! ```

pub mod arena;
pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod visitor;

pub mod helpers;

pub use arena::{Arena, Interner, Symbol};
pub use ast::{Expr, Module, Stmt};
pub use error::{Diagnostic, DiagnosticCollector, ParseError, ParseResult};
pub use lexer::{Lexer, Token};
pub use parser::{Mode, Parser};
pub use visitor::Visitor;

pub use semantic::module::ModuleExportRegistry;
pub use semantic::passes::manager::{PassManager, PassManagerConfig, PassPriority, PassStatistics};
pub use semantic::passes::ownership_check::{
    FunctionOwnershipAnalysis, MoveCandidate, MoveReason, OwnershipRecommendations,
};
pub use semantic::symbol::table::SymbolTable;
pub use semantic::types::Type;

/// Result type for Coral operations that includes semantic analysis
pub type CoralResult<T> = Result<T, CoralError>;

/// Error type that can represent both parse and semantic errors
#[derive(Debug, Clone)]
pub enum CoralError {
    /// Parse error (syntax error)
    ParseError(Box<ParseError>),
    /// Semantic analysis errors (type errors, safety violations, etc.)
    SemanticErrors(Vec<Diagnostic>),
}

impl std::fmt::Display for CoralError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoralError::ParseError(e) => write!(f, "{}", e),
            CoralError::SemanticErrors(diagnostics) => {
                writeln!(
                    f,
                    "Semantic analysis failed with {} error(s):",
                    diagnostics.len()
                )?;
                for (i, diag) in diagnostics.iter().enumerate() {
                    if i > 0 {
                        writeln!(f)?;
                    }
                    write!(f, "{:?}", diag)?;
                }
                Ok(())
            }
        }
    }
}

impl std::error::Error for CoralError {}

impl From<ParseError> for CoralError {
    fn from(error: ParseError) -> Self {
        CoralError::ParseError(Box::new(error))
    }
}

impl From<Box<ParseError>> for CoralError {
    fn from(error: Box<ParseError>) -> Self {
        CoralError::ParseError(error)
    }
}

impl From<Vec<Diagnostic>> for CoralError {
    fn from(diagnostics: Vec<Diagnostic>) -> Self {
        CoralError::SemanticErrors(diagnostics)
    }
}

/// Configuration for parsing and semantic analysis (internal)
#[derive(Debug, Clone)]
struct ParseConfig {
    /// Root directory for module resolution
    project_root: std::path::PathBuf,

    /// Current file path being analyzed (for import resolution)
    file_path: Option<std::path::PathBuf>,

    /// Enable all optional safety passes (ownership, concurrency)
    enable_safety_passes: bool,

    /// Maximum number of errors to collect before stopping
    max_errors: Option<usize>,

    /// Continue analysis even after errors
    continue_on_error: bool,

    /// Collect performance statistics
    collect_statistics: bool,

    /// Enable parallel pass execution
    parallel_execution: bool,

    /// Maximum re-export chain depth (prevents infinite loops)
    max_reexport_depth: usize,

    /// Parser mode (Module, Eval, Interactive)
    mode: Mode,
}

impl ParseConfig {
    /// Configuration for standard module parsing
    fn for_module(project_root: Option<std::path::PathBuf>) -> Self {
        Self {
            project_root: project_root.unwrap_or_else(|| std::path::PathBuf::from("<input>")),
            file_path: None, // File path not provided through public API yet
            enable_safety_passes: true, // All passes enabled
            max_errors: None,
            continue_on_error: true,
            collect_statistics: false,
            parallel_execution: false,
            max_reexport_depth: 10,
            mode: Mode::Module,
        }
    }

    /// Configuration for eval mode (single expressions)
    /// Optimized for quick evaluation, fewer passes
    fn for_eval() -> Self {
        Self {
            project_root: std::path::PathBuf::from("<eval>"),
            file_path: None,
            enable_safety_passes: false, // Skip heavy passes for eval
            max_errors: Some(1),         // Stop at first error
            continue_on_error: false,
            collect_statistics: false,
            parallel_execution: false,
            max_reexport_depth: 10,
            mode: Mode::Eval,
        }
    }

    /// Configuration for interactive/REPL mode
    /// Optimized for quick feedback and compact diagnostics
    fn for_interactive() -> Self {
        Self {
            project_root: std::path::PathBuf::from("<interactive>"),
            file_path: None,
            enable_safety_passes: false, // Skip heavy passes for REPL
            max_errors: Some(3),         // Show up to 3 errors
            continue_on_error: true,
            collect_statistics: false,
            parallel_execution: false,
            max_reexport_depth: 5, // Shorter chains for REPL
            mode: Mode::Interactive,
        }
    }
}

/// Complete result of parsing and analysis
#[derive(Debug)]
pub struct ParseResultWithMetadata<'a> {
    /// The parsed module AST
    pub module: &'a Module<'a>,

    /// All diagnostics (errors, warnings, info)
    pub diagnostics: Vec<Diagnostic>,

    /// Symbol table with resolved names
    pub symbol_table: SymbolTable,

    /// Module export registry (for cross-module validation)
    pub exports: ModuleExportRegistry,

    /// Pass execution statistics (if enabled)
    pub statistics: Option<std::collections::HashMap<String, PassStatistics>>,

    /// Recovery actions taken during parsing (for IDE quick fixes)
    pub recovery_actions: Vec<error::RecoveryAction>,

    /// Comment map for documentation and hover information
    pub comment_map: lexer::CommentMap,

    /// Symbol location index for IDE navigation (symbol_name -> definition_span)
    pub symbol_locations: std::collections::HashMap<String, text_size::TextRange>,

    /// Ownership recommendations for the parsed module
    pub ownership_recommendations: OwnershipRecommendations,
}

impl<'a> ParseResultWithMetadata<'a> {
    /// Check if there are any fatal errors
    pub fn has_fatal_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == error::codes::Severity::Fatal)
    }

    /// Check if there are any errors (non-fatal)
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| {
            d.severity == error::codes::Severity::Error
                || d.severity == error::codes::Severity::Fatal
        })
    }

    /// Get only errors
    pub fn errors(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| {
                matches!(
                    d.severity,
                    error::codes::Severity::Error | error::codes::Severity::Fatal
                )
            })
            .collect()
    }

    /// Get only warnings
    pub fn warnings(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == error::codes::Severity::Warning)
            .collect()
    }
}

/// Internal unified parse implementation
fn parse_with_config(
    source: &str,
    config: ParseConfig,
) -> CoralResult<ParseResultWithMetadata<'static>> {
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(source);
    let mut parser = Parser::with_mode(lexer, arena, config.mode);

    let module = match config.mode {
        Mode::Module => parser.parse_module()?,
        Mode::Eval => {
            let expr = parser.parse_eval()?;

            let expr_span = expr.span();
            let stmt = ast::nodes::Stmt::Expr(ast::nodes::ExprStmt {
                value: expr,
                span: expr_span,
            });
            let body = arena.alloc_slice_vec(vec![stmt]);
            arena.alloc(Module {
                body,
                span: expr_span,
                docstring: None,
            })
        }
        Mode::Interactive => {
            let stmt = parser.parse_interactive()?;

            let stmt_span = stmt.span();
            let body = arena.alloc_slice_vec(vec![stmt]);
            arena.alloc(Module {
                body,
                span: stmt_span,
                docstring: None,
            })
        }
    };

    let mut parser_errors: Vec<Diagnostic> = parser
        .errors()
        .iter()
        .map(|e| e.to_diagnostic(source))
        .collect();

    let mut parser_warnings: Vec<Diagnostic> = parser
        .warnings()
        .iter()
        .map(|e| e.to_diagnostic(source))
        .collect();

    let recovery_actions = parser.recovery_manager().actions().to_vec();
    let comment_map = parser.comment_map.clone();

    let mut manager = PassManager::with_config(
        config.project_root.clone(),
        PassManagerConfig {
            parallel_execution: config.parallel_execution,
            continue_on_error: config.continue_on_error,
            max_errors: config.max_errors,
            collect_statistics: config.collect_statistics,
            max_reexport_depth: config.max_reexport_depth,
            ..Default::default()
        },
    );

    if config.enable_safety_passes {
        manager.enable_pass("ownership_check");
        manager.enable_pass("concurrency_check");
        manager.enable_pass("protocol_checking");
    }

    let mut diagnostics = match manager.run_all_passes(module, source, config.file_path.clone()) {
        Ok(_) => Vec::new(),
        Err(diagnostics) => diagnostics,
    };

    parser_errors.append(&mut parser_warnings);
    parser_errors.append(&mut diagnostics);
    let diagnostics = parser_errors;

    let symbol_table = manager.take_symbol_table();
    let exports = manager.take_export_registry();
    let statistics = if config.collect_statistics {
        Some(manager.get_statistics().clone())
    } else {
        None
    };

    let ownership_recommendations = manager.take_ownership_recommendations();

    let symbol_locations = symbol_table.build_location_index();

    Ok(ParseResultWithMetadata {
        module,
        diagnostics,
        symbol_table,
        exports,
        statistics,
        recovery_actions,
        comment_map,
        symbol_locations,
        ownership_recommendations,
    })
}

/// Parse a Coral module with full semantic analysis
///
/// This is the main entry point for parsing Coral source files. It performs:
/// - Lexing and parsing into an AST
/// - Name resolution with symbol table generation
/// - Module system validation with transitive re-export resolution
/// - Type inference and checking
/// - Control flow analysis
/// - Pattern exhaustiveness checking
/// - Ownership and concurrency safety analysis (enabled by default)
/// - Protocol checking
///
/// # Returns
///
/// Returns a [`ParseResultWithMetadata`] containing:
/// - The parsed AST module
/// - All diagnostics (errors, warnings, info messages)
/// - Symbol table with resolved names and types
/// - Module export registry for cross-module validation
/// - Optional performance statistics
///
/// # Examples
///
/// ```no_run
/// use coral_parser::parse;
///
/// let source = r#"
/// def factorial(n: int) -> int:
///     if n <= 1:
///         return 1
///     return n * factorial(n - 1)
/// "#;
///
/// match parse(source) {
///     Ok(result) => {
///         if result.has_errors() {
///             for error in result.errors() {
///                 eprintln!("{:?}", error);
///             }
///         } else {
///             println!("Successfully parsed {} statements", result.module.body.len());
///         }
///     }
///     Err(error) => eprintln!("Fatal error: {}", error),
/// }
/// ```
pub fn parse(source: &str) -> CoralResult<ParseResultWithMetadata<'static>> {
    parse_with_config(source, ParseConfig::for_module(None))
}

/// Parse a single expression (eval mode)
///
/// Optimized for evaluating single expressions, such as in `eval()` functions or
/// interactive calculators. This mode:
/// - Skips heavy safety passes (ownership, concurrency)
/// - Stops at the first error for quick feedback
/// - Wraps the expression in a temporary module for uniformity
///
/// # Returns
///
/// Returns a [`ParseResultWithMetadata`] where `module.body` contains a single
/// expression statement.
///
/// # Examples
///
/// ```no_run
/// use coral_parser::parse_eval;
///
/// let source = "2 + 2 * 3";
/// match parse_eval(source) {
///     Ok(result) => {
///         if !result.has_errors() {
///             println!("Expression parsed successfully");
///         }
///     }
///     Err(error) => eprintln!("Error: {}", error),
/// }
/// ```
pub fn parse_eval(source: &str) -> CoralResult<ParseResultWithMetadata<'static>> {
    parse_with_config(source, ParseConfig::for_eval())
}

/// Parse a statement (interactive/REPL mode)
///
/// Optimized for interactive REPL usage with:
/// - Compact error messages suitable for terminal display
/// - Faster parsing (skips heavy safety passes)
/// - Shows up to 3 errors for immediate feedback
/// - Shorter re-export chain depth limit (5 vs 10)
///
/// # Returns
///
/// Returns a [`ParseResultWithMetadata`] where `module.body` contains a single
/// statement.
///
/// # Examples
///
/// ```no_run
/// use coral_parser::parse_interactive;
///
/// let source = "x = 42";
/// match parse_interactive(source) {
///     Ok(result) => {
///         println!("Statement parsed with {} diagnostics", result.diagnostics.len());
///     }
///     Err(error) => eprintln!("Error: {}", error),
/// }
/// ```
pub fn parse_interactive(source: &str) -> CoralResult<ParseResultWithMetadata<'static>> {
    parse_with_config(source, ParseConfig::for_interactive())
}
