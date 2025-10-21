// Pattern matching exhaustiveness checking
//
// This pass validates match statement exhaustiveness:
// 1. Checks if all possible values are covered by patterns
// 2. Detects unreachable patterns (patterns after a catch-all)
// 3. Validates pattern types match subject type
// 4. Warns about redundant patterns

#![allow(clippy::only_used_in_recursion)]

use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::ast::patterns::{MatchCase, MatchSingleton, MatchStmt, Pattern};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::type_inference::TypeInferenceContext;
use crate::semantic::types::Type;

/// Exhaustiveness checker
pub struct ExhaustivenessChecker<'a> {
    context: &'a TypeInferenceContext,
    errors: Vec<Error>,
}

impl<'a> ExhaustivenessChecker<'a> {
    pub fn new(context: &'a TypeInferenceContext) -> Self {
        Self {
            context,
            errors: Vec::new(),
        }
    }

    pub fn check_module(&mut self, module: &Module) -> Vec<Error> {
        for stmt in module.body {
            self.check_stmt(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Match(match_stmt) => {
                self.check_match(match_stmt);
            }
            Stmt::FuncDef(func) => {
                for s in func.body {
                    self.check_stmt(s);
                }
            }
            Stmt::ClassDef(class) => {
                for s in class.body {
                    self.check_stmt(s);
                }
            }
            Stmt::If(if_stmt) => {
                for s in if_stmt.body {
                    self.check_stmt(s);
                }
                for s in if_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::While(while_stmt) => {
                for s in while_stmt.body {
                    self.check_stmt(s);
                }
                for s in while_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::For(for_stmt) => {
                for s in for_stmt.body {
                    self.check_stmt(s);
                }
                for s in for_stmt.orelse {
                    self.check_stmt(s);
                }
            }
            Stmt::Try(try_stmt) => {
                for s in try_stmt.body {
                    self.check_stmt(s);
                }
                for handler in try_stmt.handlers {
                    for s in handler.body {
                        self.check_stmt(s);
                    }
                }
                for s in try_stmt.orelse {
                    self.check_stmt(s);
                }
                for s in try_stmt.finalbody {
                    self.check_stmt(s);
                }
            }
            Stmt::With(with_stmt) => {
                for s in with_stmt.body {
                    self.check_stmt(s);
                }
            }
            _ => {}
        }
    }

    fn check_match(&mut self, match_stmt: &MatchStmt) {
        // Get the subject type
        let subject_type = self.infer_expr_type(&match_stmt.subject);

        // Check for unreachable patterns
        let mut seen_catch_all = false;
        let mut catch_all_index = None;

        for (i, case) in match_stmt.cases.iter().enumerate() {
            if seen_catch_all {
                self.errors.push(*error(
                    ErrorKind::UnreachablePattern {
                        reason: format!(
                            "Pattern comes after catch-all pattern at case {}",
                            catch_all_index.unwrap() + 1
                        ),
                    },
                    case.pattern.span(),
                ));
            }

            // Check if this is a catch-all pattern
            if self.is_catch_all_pattern(&case.pattern) && case.guard.is_none() {
                seen_catch_all = true;
                catch_all_index = Some(i);
            }

            // Validate pattern type compatibility
            self.check_pattern_type(&case.pattern, &subject_type);

            // Validate guard expression type (must be bool)
            if let Some(ref guard) = case.guard {
                let guard_type = self.infer_expr_type(guard);
                if !matches!(guard_type, Type::Bool | Type::Unknown) {
                    self.errors.push(*error(
                        ErrorKind::PatternTypeMismatch {
                            expected: "bool".to_string(),
                            found: guard_type.display_name(),
                        },
                        guard.span(),
                    ));
                }
            }
        }

        // Check exhaustiveness
        if !seen_catch_all {
            let missing = self.find_missing_patterns(match_stmt.cases, &subject_type);
            if !missing.is_empty() {
                let match_header_span = text_size::TextRange::new(
                    match_stmt.span.start(),
                    match_stmt.subject.span().end(),
                );
                self.errors.push(*error(
                    ErrorKind::NonExhaustiveMatch {
                        missing_patterns: missing,
                    },
                    match_header_span,
                ));
            }
        }
    }

    /// Check if a pattern is a catch-all (matches everything)
    fn is_catch_all_pattern(&self, pattern: &Pattern) -> bool {
        match pattern {
            // `_` or `name` without a nested pattern
            Pattern::MatchAs(p) => p.pattern.is_none(),
            // Or pattern is catch-all if any branch is catch-all
            Pattern::MatchOr(p) => p.patterns.iter().any(|pat| self.is_catch_all_pattern(pat)),
            _ => false,
        }
    }

    /// Check pattern type compatibility with subject
    fn check_pattern_type(&mut self, pattern: &Pattern, subject_type: &Type) {
        match pattern {
            Pattern::MatchValue(p) => {
                let value_type = self.infer_expr_type(&p.value);
                if !value_type.is_subtype_of(subject_type)
                    && !matches!(value_type, Type::Unknown)
                    && !matches!(subject_type, Type::Unknown)
                {
                    self.errors.push(*error(
                        ErrorKind::PatternTypeMismatch {
                            expected: subject_type.display_name(),
                            found: value_type.display_name(),
                        },
                        p.span,
                    ));
                }
            }
            Pattern::MatchSingleton(p) => {
                let pattern_type = match p.value {
                    MatchSingleton::True | MatchSingleton::False => Type::Bool,
                    MatchSingleton::None => Type::None,
                };
                if !pattern_type.is_subtype_of(subject_type)
                    && !matches!(subject_type, Type::Unknown)
                {
                    self.errors.push(*error(
                        ErrorKind::PatternTypeMismatch {
                            expected: subject_type.display_name(),
                            found: pattern_type.display_name(),
                        },
                        p.span,
                    ));
                }
            }
            Pattern::MatchSequence(p) => {
                // Check if subject is a sequence type
                if !self.is_sequence_type(subject_type) && !matches!(subject_type, Type::Unknown) {
                    self.errors.push(*error(
                        ErrorKind::PatternTypeMismatch {
                            expected: subject_type.display_name(),
                            found: "sequence".to_string(),
                        },
                        p.span,
                    ));
                }
                // Recursively check nested patterns
                for nested in p.patterns {
                    // For sequences, we'd need to infer element type
                    self.check_pattern_type(nested, &Type::Unknown);
                }
            }
            Pattern::MatchMapping(p) => {
                if !self.is_mapping_type(subject_type) && !matches!(subject_type, Type::Unknown) {
                    self.errors.push(*error(
                        ErrorKind::PatternTypeMismatch {
                            expected: subject_type.display_name(),
                            found: "mapping".to_string(),
                        },
                        p.span,
                    ));
                }
            }
            Pattern::MatchOr(p) => {
                for nested in p.patterns {
                    self.check_pattern_type(nested, subject_type);
                }
            }
            Pattern::MatchAs(p) => {
                if let Some(nested) = &p.pattern {
                    self.check_pattern_type(nested, subject_type);
                }
            }
            Pattern::MatchClass(p) => {
                // Check class patterns
                let _class_type = self.infer_expr_type(&p.cls);
                // Would need proper class type checking here
                for nested in p.patterns {
                    self.check_pattern_type(nested, &Type::Unknown);
                }
                for nested in p.kwd_patterns {
                    self.check_pattern_type(nested, &Type::Unknown);
                }
            }
        }
    }

    /// Find missing patterns for exhaustiveness
    fn find_missing_patterns(&self, cases: &[MatchCase], subject_type: &Type) -> Vec<String> {
        let mut missing = Vec::new();

        match subject_type {
            Type::Bool => {
                let mut has_true = false;
                let mut has_false = false;

                for case in cases {
                    if self.pattern_matches_bool(&case.pattern, true) {
                        has_true = true;
                    }
                    if self.pattern_matches_bool(&case.pattern, false) {
                        has_false = true;
                    }
                }

                if !has_true {
                    missing.push("True".to_string());
                }
                if !has_false {
                    missing.push("False".to_string());
                }
            }
            Type::None => {
                if !cases
                    .iter()
                    .any(|case| self.pattern_matches_none(&case.pattern))
                {
                    missing.push("None".to_string());
                }
            }
            Type::Int | Type::Float | Type::Str => {
                // These are infinite types, so exhaustiveness requires a catch-all
                if !cases
                    .iter()
                    .any(|case| self.is_catch_all_pattern(&case.pattern) && case.guard.is_none())
                {
                    missing.push("_".to_string());
                }
            }
            _ => {
                // For complex types, require a catch-all pattern
                if !cases
                    .iter()
                    .any(|case| self.is_catch_all_pattern(&case.pattern) && case.guard.is_none())
                {
                    missing.push("_".to_string());
                }
            }
        }

        missing
    }

    fn pattern_matches_bool(&self, pattern: &Pattern, value: bool) -> bool {
        match pattern {
            Pattern::MatchSingleton(p) => matches!(
                (value, p.value),
                (true, MatchSingleton::True) | (false, MatchSingleton::False)
            ),
            Pattern::MatchOr(p) => p
                .patterns
                .iter()
                .any(|pat| self.pattern_matches_bool(pat, value)),
            Pattern::MatchAs(p) => {
                p.pattern.is_none()
                    || p.pattern
                        .as_ref()
                        .is_some_and(|pat| self.pattern_matches_bool(pat, value))
            }
            _ => false,
        }
    }

    fn pattern_matches_none(&self, pattern: &Pattern) -> bool {
        match pattern {
            Pattern::MatchSingleton(p) => matches!(p.value, MatchSingleton::None),
            Pattern::MatchOr(p) => p.patterns.iter().any(|pat| self.pattern_matches_none(pat)),
            Pattern::MatchAs(p) => {
                p.pattern.is_none()
                    || p.pattern
                        .as_ref()
                        .is_some_and(|pat| self.pattern_matches_none(pat))
            }
            _ => false,
        }
    }

    fn is_sequence_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::List(_) | Type::Tuple(_))
    }

    fn is_mapping_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Dict(_, _))
    }

    fn infer_expr_type(&self, expr: &Expr) -> Type {
        let span = expr.span();
        let start: usize = span.start().into();
        let end: usize = span.end().into();
        self.context
            .get_type_by_span(start, end)
            .cloned()
            .unwrap_or(Type::Unknown)
    }
}
