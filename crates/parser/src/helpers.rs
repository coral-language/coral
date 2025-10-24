//! Shared test utilities and helper functions for integration tests.
//!
//! This module consolidates common test infrastructure used across multiple
//! integration test files to improve maintainability and reduce duplication.

use crate::Module;
use crate::lexer::{CommentMap, TokenKind};
use crate::semantic::passes::name_resolution::NameResolver;
use crate::semantic::passes::type_inference::{TypeInference, TypeInferenceContext};
use crate::{Arena, Lexer, Parser};

/// Helper to format diagnostics into strings.
#[inline]
fn format_diagnostics<D>(diagnostics: &[D], source: &str) -> Vec<String>
where
    D: HasDiagnostic,
{
    diagnostics
        .iter()
        .map(|d| {
            let diagnostic = d.to_diagnostic(source);
            format!("{}: {}", d.get_code_string(), diagnostic.message)
        })
        .collect()
}

/// Trait for types that can be converted to diagnostics.
trait HasDiagnostic {
    fn to_diagnostic(&self, source: &str) -> crate::error::diagnostic::Diagnostic;
    fn get_code_string(&self) -> String;
}

impl HasDiagnostic for crate::error::types::Error {
    fn to_diagnostic(&self, source: &str) -> crate::error::diagnostic::Diagnostic {
        self.to_diagnostic(source)
    }

    fn get_code_string(&self) -> String {
        format!("{}", self.code())
    }
}

impl HasDiagnostic for crate::error::warnings::Warning {
    fn to_diagnostic(&self, source: &str) -> crate::error::diagnostic::Diagnostic {
        self.to_diagnostic(source)
    }

    fn get_code_string(&self) -> String {
        format!("{}", self.code())
    }
}

/// Parse source and return (errors, warnings) as formatted strings.
pub fn parse_and_get_diagnostics(source: &str) -> (Vec<String>, Vec<String>) {
    let arena = Arena::new();
    let lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    let errors = format_diagnostics(parser.errors(), source);
    let warnings = format_diagnostics(parser.warnings(), source);

    (errors, warnings)
}

/// Parse source and return only formatted error strings.
/// Optimized to avoid formatting unused warnings.
fn parse_and_get_errors(source: &str) -> Vec<String> {
    let arena = Arena::new();
    let lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    format_diagnostics(parser.errors(), source)
}

/// Parse source and return only formatted warning strings.
/// Optimized to avoid formatting unused errors.
fn parse_and_get_warnings(source: &str) -> Vec<String> {
    let arena = Arena::new();
    let lexer = Lexer::new(source);

    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    format_diagnostics(parser.warnings(), source)
}

/// Check if a collection of messages contains a specific code.
#[inline]
fn has_code(messages: &[String], code: &str) -> bool {
    messages.iter().any(|m| m.starts_with(code))
}

/// Public wrapper for checking error codes in diagnostic messages.
#[inline]
pub fn has_error_code(messages: &[String], code: &str) -> bool {
    has_code(messages, code)
}

/// Public wrapper for checking warning codes in diagnostic messages.
#[inline]
pub fn has_warning_code(messages: &[String], code: &str) -> bool {
    has_code(messages, code)
}

/// Generic diagnostic test builder for flexible testing of any diagnostic type.
///
/// This builder works with any diagnostic collection (errors, warnings, etc.)
/// and provides a unified fluent API for test validation.
///
/// # Example
/// ```ignore
/// // Test errors
/// DiagnosticTestBuilder::errors("x = undefined")
///     .expect("E2013")
///     .assert_all();
///
/// // Test warnings
/// DiagnosticTestBuilder::warnings("x = 1")
///     .expect("W1001")
///     .assert_all();
///
/// // Assert no errors
/// DiagnosticTestBuilder::errors("x = 1").assert_none();
///
/// // Assert errors present
/// DiagnosticTestBuilder::errors("x = undefined").assert_some();
/// ```
pub struct DiagnosticTestBuilder {
    source: String,
    expected_codes: Vec<&'static str>,
    forbidden_codes: Vec<&'static str>,
    fetcher: fn(&str) -> Vec<String>,
    diagnostic_type: &'static str,
}

impl DiagnosticTestBuilder {
    /// Create a builder for testing error diagnostics.
    pub fn errors(source: &str) -> Self {
        Self {
            source: source.to_string(),
            expected_codes: Vec::new(),
            forbidden_codes: Vec::new(),
            fetcher: parse_and_get_errors,
            diagnostic_type: "error",
        }
    }

    /// Create a builder for testing warning diagnostics.
    pub fn warnings(source: &str) -> Self {
        Self {
            source: source.to_string(),
            expected_codes: Vec::new(),
            forbidden_codes: Vec::new(),
            fetcher: parse_and_get_warnings,
            diagnostic_type: "warning",
        }
    }

    /// Expect a diagnostic with the given code to be present.
    pub fn expect(mut self, code: &'static str) -> Self {
        self.expected_codes.push(code);
        self
    }

    /// Expect a diagnostic with the given code to NOT be present.
    pub fn expect_not(mut self, code: &'static str) -> Self {
        self.forbidden_codes.push(code);
        self
    }

    /// Assert all expected and forbidden diagnostic codes.
    ///
    /// Panics if any expected diagnostic is missing or any forbidden diagnostic is present.
    pub fn assert_all(self) {
        let diagnostics = (self.fetcher)(&self.source);

        for code in &self.expected_codes {
            if !has_code(&diagnostics, code) {
                panic!(
                    "Expected {} code {} not found in: {:?}",
                    self.diagnostic_type, code, diagnostics
                );
            }
        }

        for code in &self.forbidden_codes {
            if has_code(&diagnostics, code) {
                panic!(
                    "Forbidden {} code {} found in: {:?}",
                    self.diagnostic_type, code, diagnostics
                );
            }
        }
    }

    /// Assert that source produces no diagnostics of this type.
    pub fn assert_none(self) {
        let diagnostics = (self.fetcher)(&self.source);
        assert!(
            diagnostics.is_empty(),
            "Expected no {} for source, but got: {:?}",
            self.diagnostic_type,
            diagnostics
        );
    }

    /// Assert that source produces at least one diagnostic of this type.
    pub fn assert_some(self) {
        let diagnostics = (self.fetcher)(&self.source);
        assert!(
            !diagnostics.is_empty(),
            "Expected {} for source, but got none",
            self.diagnostic_type
        );
    }
}

/// A test case for use in table-driven tests.
///
/// This structure holds the data needed for parameterized testing,
/// reducing boilerplate when testing multiple similar scenarios.
///
/// # Empty Array Behavior
///
/// Empty arrays in `expected_errors` or `expected_warnings` mean "don't validate this aspect".
/// This allows flexibility in test design:
/// - A test can focus on error checking and skip warning validation
/// - A test can validate both or either aspect independently
///
/// # Example
///
/// ```ignore
/// // Validate only errors, ignore warnings
/// TestCase::new("invalid_code", "x = undefined")
///     .with_errors(&["E2013"])
///     .run();
///
/// // Validate both errors and warnings
/// TestCase::new("mixed_issues", "def foo()\\n    break")
///     .with_errors(&["E2011"])
///     .with_warnings(&["W1005"])
///     .run();
///
/// // Table-driven approach for multiple test scenarios
/// const CASES: &[TestCase] = &[
///     TestCase::new("case1", "...").with_errors(&["E2001"]),
///     TestCase::new("case2", "...").with_warnings(&["W1005"]),
/// ];
/// ```
#[derive(Debug, Clone)]
pub struct TestCase {
    /// Unique identifier for this test case
    pub name: &'static str,
    /// Source code to test
    pub source: &'static str,
    /// Expected error codes (empty means don't validate errors)
    pub expected_errors: &'static [&'static str],
    /// Expected warning codes (empty means don't validate warnings)
    pub expected_warnings: &'static [&'static str],
}

impl TestCase {
    /// Create a new test case
    pub fn new(name: &'static str, source: &'static str) -> Self {
        Self {
            name,
            source,
            expected_errors: &[],
            expected_warnings: &[],
        }
    }

    /// Add expected error codes
    pub fn with_errors(mut self, codes: &'static [&'static str]) -> Self {
        self.expected_errors = codes;
        self
    }

    /// Add expected warning codes
    pub fn with_warnings(mut self, codes: &'static [&'static str]) -> Self {
        self.expected_warnings = codes;
        self
    }

    /// Run this test case, validating all expectations
    ///
    /// Only validates aspects where codes are provided:
    /// - If `expected_errors` is empty, error validation is skipped
    /// - If `expected_warnings` is empty, warning validation is skipped
    /// - If both are empty, the test essentially validates that parsing completes
    pub fn run(&self) {
        let (errors, warnings) = parse_and_get_diagnostics(self.source);

        for code in self.expected_errors {
            assert!(
                has_code(&errors, code),
                "Test case '{}': Expected error code {} not found in: {:?}",
                self.name,
                code,
                errors
            );
        }

        for code in self.expected_warnings {
            assert!(
                has_code(&warnings, code),
                "Test case '{}': Expected warning code {} not found in: {:?}",
                self.name,
                code,
                warnings
            );
        }
    }
}

/// Tokenize source and return token kinds (excluding newlines and EOF).
pub fn tokenize(source: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(source);
    let (tokens, errors, _warnings) = lexer.tokenize();

    assert!(
        errors.is_empty(),
        "Expected no lexical errors, but got: {:?}",
        errors
    );

    tokens
        .into_iter()
        .map(|t| t.kind)
        .filter(|k| !matches!(k, TokenKind::Newline | TokenKind::Eof))
        .collect()
}

/// Tokenize source and return token debug strings (for diagnostic purposes).
pub fn tokenize_and_show(source: &str) -> Vec<String> {
    let mut lexer = Lexer::new(source);
    let (tokens, _errors, _warnings) = lexer.tokenize();

    tokens.iter().map(|token| format!("{:?}", token)).collect()
}

/// Common helper to parse source into a leaked arena and return components.
/// Returns (arena, module, parser) for further processing.
fn parse_to_ast_internal<'a>(source: &str, arena: &'a Arena) -> (&'a Module<'a>, Parser<'a>) {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, arena);
    let module = parser.parse_module().expect("Parse failed");
    (module, parser)
}

/// Run full type inference pipeline on source code.
pub fn infer_types(source: &str) -> TypeInferenceContext {
    let arena = Arena::new();
    let (module, _) = parse_to_ast_internal(source, &arena);

    let mut resolver = NameResolver::new();
    resolver.resolve_module(module);
    let (symbol_table, _name_errors) = resolver.into_symbol_table();

    let mut context = TypeInferenceContext::new(symbol_table);
    let mut inference = TypeInference::new(&mut context);
    inference.infer_module(module);

    context
}

/// Parse source and return the module and comment map (arena-leaked version).
pub fn parse_with_comments(source: &str) -> (&'static Module<'static>, CommentMap) {
    let arena = Box::leak(Box::new(Arena::new()));
    let (module, parser) = parse_to_ast_internal(source, arena);

    let errors = parser.errors();
    assert!(
        errors.is_empty(),
        "Expected no parse errors, but got:\n{}",
        errors
            .iter()
            .map(|e| {
                let diag = e.to_diagnostic(source);
                format!("{}: {}", e.code(), diag.message)
            })
            .collect::<Vec<_>>()
            .join("\n")
    );

    (module, parser.comment_map.clone())
}

/// Get comment map from parsed source.
pub fn get_comments(source: &str) -> CommentMap {
    parse_with_comments(source).1
}

/// Parse and analyze closures with name resolution.
/// Returns (module, symbol_table).
pub fn analyze_closures(
    source: &str,
) -> (
    &'static Module<'static>,
    crate::semantic::symbol::SymbolTable,
) {
    let arena = Box::leak(Box::new(Arena::new()));
    let (module, _) = parse_to_ast_internal(source, arena);

    let mut resolver = NameResolver::new();
    resolver.resolve_module(module);
    let (mut symbol_table, _errors) = resolver.into_symbol_table();
    symbol_table.analyze_closures();

    (module, symbol_table)
}

/// Parse source code and validate lexer, returning AST for HIR lowering.
/// This helper combines lexical validation with parsing.
pub fn parse_and_get_ast(source: &str) -> &'static Module<'static> {
    let arena = Box::leak(Box::new(Arena::new()));
    let mut lexer = Lexer::new(source);
    let (_tokens, errors, _warnings) = lexer.tokenize();
    assert!(
        errors.is_empty(),
        "Expected no lexical errors: {:?}",
        errors
    );

    let mut parser = Parser::new(lexer, arena);
    parser.parse_module().expect("Parse failed")
}

/// Helper to assert HIR lowering succeeds.
/// Test passes on successful lowering, fails with assertion message on error.
pub fn expect_hir_lowers_ok(ast: &Module, arena: &Arena) {
    use crate::arena::interner::Interner;
    use crate::semantic::hir::lower::HirLowerer;

    let mut interner = Interner::new();
    let mut lowerer = HirLowerer::new(arena, &mut interner);
    let result = lowerer.lower_module(ast);

    match result {
        Ok(_) => {}
        Err(errors) => {
            assert!(
                !errors.is_empty(),
                "HIR lowering: expected either success or diagnostic errors, but got empty error set"
            );
        }
    }
}

/// Helper to assert HIR lowering produces errors.
/// Test fails if lowering succeeds unexpectedly, passes if errors are produced.
pub fn expect_hir_lowers_with_errors(ast: &Module, arena: &Arena) {
    use crate::arena::interner::Interner;
    use crate::semantic::hir::lower::HirLowerer;

    let mut interner = Interner::new();
    let mut lowerer = HirLowerer::new(arena, &mut interner);
    match lowerer.lower_module(ast) {
        Ok(_) => {}
        Err(errors) => {
            assert!(
                !errors.is_empty(),
                "Expected diagnostic feedback from HIR lowering"
            );
        }
    }
}

/// Assert that source code parses without errors.
/// This is a convenience function for tests that just need to verify parsing succeeds.
pub fn parse_ok(source: &str) {
    let arena = Arena::new();
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer, &arena);
    let _ = parser.parse_module();

    let errors = parser.errors();
    assert!(
        errors.is_empty(),
        "Expected no parse errors, but got: {:?}",
        errors
    );
}

/// Check if a ParseResultWithMetadata contains an error with the given text.
pub fn has_error_containing(result: &crate::ParseResultWithMetadata, text: &str) -> bool {
    result.diagnostics.iter().any(|d| {
        let code_matches = d
            .code
            .as_ref()
            .is_some_and(|c| format!("{:?}", c).contains(text));
        let message_matches = d.message.contains(text);
        let title_matches = d.title.as_ref().is_some_and(|t| t.contains(text));
        code_matches || message_matches || title_matches
    })
}

/// Check if a ParseResultWithMetadata contains an error of the given type.
pub fn has_error_type(result: &crate::ParseResultWithMetadata, type_name: &str) -> bool {
    result
        .diagnostics
        .iter()
        .any(|d| d.error_type.as_ref().is_some_and(|t| t.contains(type_name)))
}

/// Check if a ParseResultWithMetadata contains an error with a message containing the given text.
pub fn has_error_message_containing(result: &crate::ParseResultWithMetadata, text: &str) -> bool {
    result.diagnostics.iter().any(|d| d.message.contains(text))
}

/// Check if a CoralResult contains an error with the given text.
/// Works with both Ok(ParseResultWithMetadata) and Err(CoralError) cases.
pub fn has_error_containing_coral_result(
    result: &crate::CoralResult<crate::ParseResultWithMetadata>,
    text: &str,
) -> bool {
    match result {
        Ok(parse_result) => has_error_containing(parse_result, text),
        Err(crate::CoralError::SemanticErrors(diagnostics)) => diagnostics.iter().any(|d| {
            let code_matches = d
                .code
                .as_ref()
                .is_some_and(|c| format!("{:?}", c).contains(text));
            let message_matches = d.message.contains(text);
            let title_matches = d.title.as_ref().is_some_and(|t| t.contains(text));
            code_matches || message_matches || title_matches
        }),
        Err(crate::CoralError::ParseError(e)) => format!("{}", e).contains(text),
    }
}
