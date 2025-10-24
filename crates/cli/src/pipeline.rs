//! Pipeline for processing Coral files.
//!
//! # Error Severity Model
//!
//! Coral uses a four-level severity system for diagnostics:
//!
//! - **Fatal**: Unrecoverable errors that stop compilation immediately
//!   - Internal compiler errors
//!   - I/O failures
//!   - Always exit with code 1
//!
//! - **Error**: Recoverable errors that indicate code issues
//!   - Syntax errors (unexpected tokens, missing delimiters, etc.)
//!   - Semantic errors (missing returns, type mismatches, undefined names, etc.)
//!   - Behavior depends on mode:
//!     * Non-strict mode (default): exit code 0 (good for web servers, live reloading)
//!     * Strict mode (`--strict`): exit code 1 (good for CI/CD, pre-commit hooks)
//!
//! - **Warning**: Non-blocking code quality issues
//!   - Unreachable code
//!   - Unused variables
//!   - Always exit with code 0 in both modes
//!
//! - **Info**: Informational messages and suggestions
//!   - Hints for improvement
//!   - Always exit with code 0 in both modes
//!
//! ## Use Cases
//!
//! ### Web Servers (Non-Strict Mode)
//! ```bash
//! coral check routes.coral  # Exit 0 even with recoverable errors
//! ```
//! Allows the server to continue running even if some routes have errors.
//! The problematic routes will fail when called, but the server stays up.
//!
//! ### CI/CD Pipelines (Strict Mode)
//! ```bash
//! coral check --strict src/main.coral  # Exit 1 on any error
//! ```
//! Fails the build if there are any errors, ensuring code quality.

use coral_parser::{
    Arena, CoralError, Lexer, Parser, PassManager,
    error::{config::ErrorConfig, formatter::DiagnosticFormatter},
};
use std::fs;
use std::path::{Path, PathBuf};

/// Result type for pipeline operations
pub type PipelineResult<T> = Result<T, PipelineError>;

/// Pipeline error type
#[derive(Debug)]
pub enum PipelineError {
    /// File I/O error
    IoError(std::io::Error),
    /// Parse or semantic error
    CoralError(CoralError),
}

impl std::fmt::Display for PipelineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PipelineError::IoError(e) => write!(f, "I/O error: {}", e),
            // CoralError is already displayed by display_coral_error()
            #[allow(unused_variables)]
            PipelineError::CoralError(e) => Ok(()),
        }
    }
}

impl std::error::Error for PipelineError {}

impl From<std::io::Error> for PipelineError {
    fn from(error: std::io::Error) -> Self {
        PipelineError::IoError(error)
    }
}

impl From<CoralError> for PipelineError {
    fn from(error: CoralError) -> Self {
        PipelineError::CoralError(error)
    }
}

/// Run a Coral file (placeholder - will execute interpreter in the future)
pub fn run_file(path: &Path) -> PipelineResult<()> {
    // Read the source file
    let source = fs::read_to_string(path)?;

    // Parse with error recovery to collect all errors
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, arena);

    // Try to parse the module - parser will do error recovery
    let parse_result = parser.parse_module();

    // Always check for collected errors first (error recovery mode)
    if parser.has_errors() {
        display_parse_errors_with_filename(parser.errors(), &source, Some(path));
        return Err(PipelineError::CoralError(CoralError::ParseError(Box::new(
            parser.errors()[0].clone(),
        ))));
    }

    // If no collected errors, check if parsing itself failed
    match parse_result {
        Ok(_module) => {
            // Silent success - no output
            Ok(())
        }
        Err(error) => {
            // This shouldn't happen if error recovery is working,
            // but handle it just in case
            display_parse_error_with_filename(&error, &source, Some(path));
            Err(PipelineError::CoralError(CoralError::ParseError(error)))
        }
    }
}

/// Check a Coral file for errors
pub fn check_file(path: &Path, strict: bool) -> PipelineResult<()> {
    // Read the source file
    let source = fs::read_to_string(path)?;

    // Parse with error recovery to collect all errors
    let arena = Box::leak(Box::new(Arena::new()));
    let lexer = Lexer::new(&source);
    let mut parser = Parser::new(lexer, arena);

    // Try to parse the module - parser will do error recovery
    let parse_result = parser.parse_module();

    // Always check for collected errors first (error recovery mode)
    if parser.has_errors() {
        display_parse_errors_with_filename(parser.errors(), &source, Some(path));
        return Err(PipelineError::CoralError(CoralError::ParseError(Box::new(
            parser.errors()[0].clone(),
        ))));
    }

    // If no collected errors, check if parsing itself failed
    match parse_result {
        Ok(module) => {
            // Parse succeeded! Now run semantic analysis
            let mut manager = PassManager::new(path.to_path_buf());

            // Enable all optional safety passes
            manager.enable_pass("ownership_check");
            manager.enable_pass("concurrency_check");
            manager.enable_pass("protocol_checking");

            // Run all passes and check for semantic errors/warnings
            // Pass the file path for proper import resolution
            match manager.run_all_passes(module, &source, Some(path.to_path_buf())) {
                Ok(_) => {
                    // Silent success - no errors or warnings
                    Ok(())
                }
                Err(diagnostics) => {
                    // Display all diagnostics (errors and warnings)
                    display_diagnostics_with_filename(&diagnostics, Some(path));

                    // Determine if we should fail based on severity:
                    // - Fatal errors: Always stop (e.g., internal compiler errors, I/O errors)
                    // - Error severity: Recoverable issues (syntax/semantic errors)
                    //   * In strict mode: treat as fatal (exit 1)
                    //   * In non-strict mode: allow to continue (exit 0) - good for web servers
                    // - Warning: Non-blocking issues (always exit 0)
                    // - Info: Informational messages (always exit 0)

                    let has_fatal_errors = diagnostics
                        .iter()
                        .any(|d| matches!(d.severity, coral_parser::error::Severity::Fatal));

                    let has_errors = diagnostics
                        .iter()
                        .any(|d| matches!(d.severity, coral_parser::error::Severity::Error));

                    if has_fatal_errors || (strict && has_errors) {
                        // Fail in these cases:
                        // 1. Fatal errors (always)
                        // 2. Any errors when in strict mode
                        Err(PipelineError::CoralError(CoralError::SemanticErrors(
                            diagnostics,
                        )))
                    } else {
                        // Recoverable errors and warnings don't stop execution in non-strict mode
                        // This allows web servers and other long-running processes to continue
                        Ok(())
                    }
                }
            }
        }
        Err(error) => {
            // This shouldn't happen if error recovery is working,
            // but handle it just in case
            display_parse_error_with_filename(&error, &source, Some(path));
            Err(PipelineError::CoralError(CoralError::ParseError(error)))
        }
    }
}

/// Find the Coral project root by searching for Coral.toml
/// Returns the project root if found, otherwise None
fn find_project_root(start_path: &Path) -> Option<PathBuf> {
    let mut current = start_path;

    // If start_path is a file, start from its parent directory
    if current.is_file() {
        current = current.parent()?;
    }

    loop {
        // Check if Coral.toml exists in this directory
        let config_path = current.join("Coral.toml");
        if config_path.exists() {
            return Some(current.to_path_buf());
        }

        // Move up to parent directory
        current = current.parent()?;
    }
}

/// Convert a path to a display-friendly string relative to Coral project root
/// If no project root is found, returns just the filename
fn path_to_display_string(path: &Path) -> String {
    // First, try to canonicalize the path to resolve any symlinks or relative components
    // If that fails (e.g., file doesn't exist), try to get absolute path from current dir
    let absolute_path = if let Ok(canonical) = path.canonicalize() {
        canonical
    } else if let Ok(cwd) = std::env::current_dir() {
        cwd.join(path)
    } else {
        path.to_path_buf()
    };

    // Try to find the Coral project root
    if let Some(project_root) = find_project_root(&absolute_path) {
        // Make path relative to project root
        if let Ok(relative) = absolute_path.strip_prefix(&project_root) {
            return relative.display().to_string();
        }
    }

    // Fallback: just show the filename without path
    absolute_path
        .file_name()
        .and_then(|name| name.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| path.display().to_string())
}

/// Display multiple parse errors with filename
fn display_parse_errors_with_filename(
    errors: &[coral_parser::ParseError],
    source: &str,
    filename: Option<&Path>,
) {
    let config = ErrorConfig {
        max_errors: 1000,
        show_codes: true,
        show_context: true,
        context_lines: 2,
        use_colors: true,
        ..Default::default()
    };
    let formatter = DiagnosticFormatter::new(config);

    // Deduplicate errors by span to avoid showing the same error multiple times
    let mut seen_spans = std::collections::HashSet::new();
    let mut count = 0;

    for error in errors.iter() {
        if seen_spans.insert(error.span) {
            if count > 0 {
                eprintln!();
            }
            let mut diagnostic = error.to_diagnostic(source);

            // Add filename to context if available
            if let (Some(filename), Some(context)) = (filename, &mut diagnostic.context) {
                context.filename = Some(path_to_display_string(filename));
            }

            let formatted = formatter.format(&diagnostic);
            eprintln!("{}", formatted);
            count += 1;
        }
    }
}

/// Display a single parse error with filename
fn display_parse_error_with_filename(
    error: &coral_parser::ParseError,
    source: &str,
    filename: Option<&Path>,
) {
    let config = ErrorConfig {
        max_errors: 1000,
        show_codes: true,
        show_context: true,
        context_lines: 2,
        use_colors: true,
        ..Default::default()
    };
    let formatter = DiagnosticFormatter::new(config);

    let mut diagnostic = error.to_diagnostic(source);

    // Add filename to context if available
    if let (Some(filename), Some(context)) = (filename, &mut diagnostic.context) {
        context.filename = Some(path_to_display_string(filename));
    }

    let formatted = formatter.format(&diagnostic);
    eprintln!("{}", formatted);
}

/// Display diagnostics (errors and warnings)
/// Display diagnostics with filename
fn display_diagnostics_with_filename(
    diagnostics: &[coral_parser::Diagnostic],
    filename: Option<&Path>,
) {
    let config = ErrorConfig {
        max_errors: 1000,
        show_codes: true,
        show_context: true,
        context_lines: 2,
        use_colors: true,
        ..Default::default()
    };
    let formatter = DiagnosticFormatter::new(config);

    // Display all diagnostics
    for diagnostic in diagnostics.iter() {
        // Add one line break before the first error, and one line between subsequent errors
        eprintln!();

        // Clone the diagnostic to add filename to context
        let mut diagnostic_with_path = diagnostic.clone();
        if let (Some(filename), Some(context)) = (filename, &mut diagnostic_with_path.context) {
            context.filename = Some(path_to_display_string(filename));
        }

        let formatted = formatter.format(&diagnostic_with_path);
        eprintln!("{}", formatted);
    }
}

/// Display a CoralError with beautiful formatting
#[allow(dead_code)]
fn display_coral_error(error: &CoralError, source: &str, _path: &Path) {
    let config = ErrorConfig {
        max_errors: 1000,
        show_codes: true,
        show_context: true,
        context_lines: 5,
        use_colors: true,
        ..Default::default()
    };
    let formatter = DiagnosticFormatter::new(config);

    match error {
        CoralError::ParseError(parse_error) => {
            // Convert ParseError to Diagnostic for beautiful formatting
            let diagnostic = parse_error.to_diagnostic(source);
            let formatted = formatter.format(&diagnostic);
            eprintln!("{}", formatted);
        }
        CoralError::SemanticErrors(diagnostics) => {
            for (i, diagnostic) in diagnostics.iter().enumerate() {
                if i > 0 {
                    eprintln!();
                }

                // Use the formatter for beautiful error display
                let formatted = formatter.format(diagnostic);
                eprintln!("{}", formatted);
            }
        }
    }
}

/// Format a Coral file (placeholder)
pub fn format_file(path: &Path) -> PipelineResult<()> {
    println!("Format command for {} (coming soon)", path.display());
    Ok(())
}

/// Start interactive REPL (placeholder)
pub fn start_repl() -> PipelineResult<()> {
    println!("Interactive REPL (coming soon)");
    println!("This will provide a read-eval-print loop for Coral code.");
    Ok(())
}
