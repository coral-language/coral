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
//! - **Concurrency safety analysis** - Detects data races and deadlocks at compile time
//! - **Module system validation** - Validates imports and exports
//! - **Import resolution** - Resolves and validates module dependencies
//! - **Decorator validation** - Ensures correct decorator usage
//! - **Protocol checking** - Structural subtyping validation (PEP 544)
//!
//! All passes are enabled by default for production safety. Use [`PassManager`]
//! directly for custom configuration.
//!
//! # Examples
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
//!     Ok(module) => println!("Success!"),
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

pub use arena::{Arena, Interner, Symbol};
pub use ast::{Expr, Module, Stmt};
pub use error::{Diagnostic, DiagnosticCollector, ParseError, ParseResult};
pub use lexer::{Lexer, Token};
pub use parser::{Mode, Parser};
pub use visitor::Visitor;

// Re-export key semantic analysis components
pub use semantic::passes::manager::{PassManager, PassManagerConfig, PassPriority};
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

/// Parse a module and run full semantic analysis
///
/// This function performs comprehensive analysis including:
/// - Syntax parsing
/// - Name resolution
/// - Type inference and checking
/// - Control flow analysis
/// - Pattern exhaustiveness checking
/// - Ownership and memory safety analysis
/// - Concurrency safety analysis (data races, deadlocks)
/// - Protocol checking
///
/// All safety passes are **enabled by default** for production use.
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
///     Ok(module) => println!("Analysis passed!"),
///     Err(error) => eprintln!("{}", error),
/// }
/// ```
///
/// For custom configuration, use [`PassManager`] directly:
///
/// ```no_run
/// use coral_parser::{PassManager, PassManagerConfig};
/// use std::path::PathBuf;
/// use std::collections::HashSet;
///
/// let config = PassManagerConfig {
///     collect_statistics: true,
///     max_errors: Some(5),
///     ..Default::default()
/// };
///
/// let mut manager = PassManager::with_config(PathBuf::from("example.coral"), config);
/// // Optionally disable specific passes
/// manager.disable_pass("concurrency_check");
/// ```
pub fn parse(source: &str) -> CoralResult<&'static Module<'static>> {
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, arena);
    let module = parser.parse_module()?;

    // Run semantic analysis with ALL passes enabled by default
    let mut manager = PassManager::new(std::path::PathBuf::from("<input>"));

    // Enable all optional safety passes for production use
    manager.enable_pass("ownership_check");
    manager.enable_pass("concurrency_check");
    manager.enable_pass("protocol_checking");

    // Run all passes and propagate errors
    manager.run_all_passes(module, source)?;

    Ok(module)
}

/// Parse an expression (eval mode) and run semantic analysis
///
/// This function is used for parsing single expressions, such as those
/// in the `eval()` function.
///
/// # Examples
///
/// ```no_run
/// use coral_parser::parse_eval;
///
/// let source = "2 + 2 * 3";
///
/// match parse_eval(source) {
///     Ok(expr) => println!("Expression parsed successfully"),
///     Err(error) => eprintln!("{}", error),
/// }
/// ```
pub fn parse_eval(source: &str) -> CoralResult<Expr<'static>> {
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(source);
    let mut parser = Parser::with_mode(lexer, arena, Mode::Eval);
    let expr = parser.parse_eval()?;

    // Create a temporary module wrapper for semantic analysis
    use crate::ast::nodes::ExprStmt;
    let expr_span = expr.span();
    let stmt = Stmt::Expr(ExprStmt {
        value: expr.clone(),
        span: expr_span,
    });
    let body = arena.alloc_slice_vec(vec![stmt]);
    let module = arena.alloc(Module {
        body,
        span: expr_span,
    });

    // Run semantic analysis with all passes enabled
    let mut manager = PassManager::new(std::path::PathBuf::from("<eval>"));
    manager.enable_pass("ownership_check");
    manager.enable_pass("concurrency_check");
    manager.enable_pass("protocol_checking");

    manager.run_all_passes(module, source)?;

    Ok(expr)
}

/// Parse a statement (interactive mode) and run semantic analysis
///
/// This function is used for parsing single statements, such as those
/// in the interactive REPL.
///
/// # Examples
///
/// ```no_run
/// use coral_parser::parse_interactive;
///
/// let source = "x = 42";
///
/// match parse_interactive(source) {
///     Ok(stmt) => println!("Statement parsed successfully"),
///     Err(error) => eprintln!("{}", error),
/// }
/// ```
pub fn parse_interactive(source: &str) -> CoralResult<Stmt<'static>> {
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(source);
    let mut parser = Parser::with_mode(lexer, arena, Mode::Interactive);
    let stmt = parser.parse_interactive()?;

    // Create a temporary module wrapper for semantic analysis
    let stmt_span = stmt.span();
    let body = arena.alloc_slice_vec(vec![stmt.clone()]);
    let module = arena.alloc(Module {
        body,
        span: stmt_span,
    });

    // Run semantic analysis with all passes enabled
    let mut manager = PassManager::new(std::path::PathBuf::from("<interactive>"));
    manager.enable_pass("ownership_check");
    manager.enable_pass("concurrency_check");
    manager.enable_pass("protocol_checking");

    manager.run_all_passes(module, source)?;

    Ok(stmt)
}
