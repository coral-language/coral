// Pattern matching exhaustiveness checking
//
// This pass validates match statement exhaustiveness using a pattern matrix algorithm:
// 1. Checks if all possible values are covered by patterns
// 2. Detects unreachable/redundant patterns
// 3. Validates pattern types match subject type
// 4. Handles nested patterns, or-patterns, guards, and star patterns
//
// ## Algorithm
//
// The exhaustiveness checker uses a pattern matrix approach similar to the one
// described in "Warnings for pattern matching" (Luc Maranget, 2007).
//
// A pattern matrix is a table where:
// - Each row represents a match case
// - Each column represents a position in the pattern (for nested patterns)
//
// The algorithm works by:
// 1. Building a pattern matrix from the match cases
// 2. Checking if the matrix is exhaustive by computing witnesses (missing patterns)
// 3. Detecting redundant patterns by checking if each row is useful
//
// ## Complexity
//
// - Time: O(n * m * k) where n = number of cases, m = pattern depth, k = constructor arity
// - Space: O(n * m) for the pattern matrix
// - Redundancy check is now O(n²) with early termination

#![allow(clippy::only_used_in_recursion)]

use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::ast::patterns::{MatchSingleton, MatchStmt, Pattern};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::type_inference::TypeInferenceContext;
use crate::semantic::types::Type;
use std::collections::HashSet;

/// Maximum number of integer samples to consider for exhaustiveness
const MAX_INT_SAMPLES: usize = 128;

/// Maximum number of string samples to consider for exhaustiveness
const MAX_STR_SAMPLES: usize = 64;

/// Constructor patterns used in the pattern matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Constructor {
    /// Literal integer value
    Int(i64),
    /// Literal string value
    Str(String),
    /// Boolean true
    True,
    /// Boolean false
    False,
    /// None singleton
    None,
    /// Tuple with exact arity
    Tuple(usize),
    /// List with exact length
    List(usize),
    /// Star pattern for variable-length sequences (min_length)
    StarList(usize),
    /// Class pattern
    Class(String, usize), // class name, arity
    /// Mapping pattern with specific keys (sorted for hashing)
    MappingKeys(Vec<String>),
    /// Mapping with rest pattern
    MappingRest,
    /// Or-pattern containing alternative branches
    OrPattern(Vec<SimplePattern>),
    /// Wildcard (matches everything)
    Wildcard,
}

impl Constructor {
    /// Check if this constructor is more specific than another (for redundancy)
    fn subsumes(&self, other: &Constructor) -> bool {
        match (self, other) {
            (Constructor::Wildcard, _) => true,
            (a, b) if a == b => true,
            // Star patterns subsume exact-length lists if length >= min_length
            (Constructor::StarList(min), Constructor::List(len)) => len >= min,
            (Constructor::StarList(min), Constructor::Tuple(len)) => len >= min,
            (Constructor::StarList(min1), Constructor::StarList(min2)) => min1 <= min2,
            // Mapping with fewer keys subsumes mapping with more keys
            (Constructor::MappingKeys(keys1), Constructor::MappingKeys(keys2)) => {
                keys1.iter().all(|k| keys2.contains(k))
            }
            (Constructor::MappingRest, Constructor::MappingKeys(_)) => true,
            // Or-patterns: self subsumes other if self subsumes any branch of other
            (self_ctor, Constructor::OrPattern(branches)) => branches
                .iter()
                .any(|branch| self_ctor.subsumes(&branch.constructor)),
            _ => false,
        }
    }
}

/// A simplified pattern for matrix operations
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SimplePattern {
    constructor: Constructor,
    /// Sub-patterns for nested constructors
    sub_patterns: Vec<SimplePattern>,
}

impl SimplePattern {
    fn wildcard() -> Self {
        Self {
            constructor: Constructor::Wildcard,
            sub_patterns: Vec::new(),
        }
    }

    fn from_constructor(ctor: Constructor, subs: Vec<SimplePattern>) -> Self {
        Self {
            constructor: ctor,
            sub_patterns: subs,
        }
    }
}

/// Pattern matrix row
#[derive(Debug, Clone)]
struct PatternRow {
    patterns: Vec<SimplePattern>,
    /// Original case index for error reporting
    case_index: usize,
    /// Whether this row has a guard (affects exhaustiveness)
    has_guard: bool,
    /// Whether this row is useful (not redundant)
    is_useful: bool,
}

/// Pattern matrix for exhaustiveness checking
#[derive(Debug)]
struct PatternMatrix {
    rows: Vec<PatternRow>,
}

impl PatternMatrix {
    fn new() -> Self {
        Self { rows: Vec::new() }
    }

    fn add_row(&mut self, mut row: PatternRow, ctors: &ConstructorSet) {
        // Check if this row is useful before adding
        row.is_useful = self.is_row_useful_incremental(&row, ctors);
        self.rows.push(row);
    }

    /// Check if the matrix is exhaustive
    /// Returns None if exhaustive, or a witness pattern if not exhaustive
    fn is_exhaustive(&self, ctors: &ConstructorSet) -> Option<Vec<SimplePattern>> {
        self.compute_missing_patterns(ctors)
    }

    /// Compute missing patterns (witnesses) for non-exhaustive matches
    fn compute_missing_patterns(&self, ctors: &ConstructorSet) -> Option<Vec<SimplePattern>> {
        // Base case: no patterns left
        if self.rows.is_empty() {
            // No rows means not exhaustive - return a wildcard witness
            return Some(vec![SimplePattern::wildcard()]);
        }

        // Base case: empty pattern (all positions covered)
        if self.rows.iter().all(|r| r.patterns.is_empty()) {
            // At least one row with no patterns means exhaustive
            return None;
        }

        // Filter out rows with guards (they don't guarantee coverage)
        let rows_without_guards: Vec<&PatternRow> =
            self.rows.iter().filter(|r| !r.has_guard).collect();

        if rows_without_guards.is_empty() && !self.rows.is_empty() {
            // All rows have guards - conservatively assume not exhaustive
            return Some(vec![SimplePattern::wildcard()]);
        }

        // Get the first column constructors
        let first_column_ctors: HashSet<Constructor> = rows_without_guards
            .iter()
            .filter(|r| !r.patterns.is_empty())
            .map(|r| r.patterns[0].constructor.clone())
            .collect();

        // Check if there's a wildcard in the first column
        if first_column_ctors.contains(&Constructor::Wildcard) {
            // Wildcard covers all - recurse on remaining columns
            let specialized = self.specialize(&Constructor::Wildcard, 0);
            return specialized.compute_missing_patterns(ctors);
        }

        // Get complete constructor set for this position
        let complete_ctors = ctors.get_complete_set(&first_column_ctors);

        // Check each constructor in the complete set
        for ctor in &complete_ctors {
            let arity = get_constructor_arity(ctor);
            let specialized = self.specialize(ctor, arity);

            if let Some(mut witness) = specialized.compute_missing_patterns(ctors) {
                // Found a witness - prepend the constructor
                let ctor_pattern = SimplePattern::from_constructor(
                    ctor.clone(),
                    witness.drain(..arity.min(witness.len())).collect(),
                );
                let mut result = vec![ctor_pattern];
                result.extend(witness);
                return Some(result);
            }
        }

        // All constructors covered
        None
    }

    /// Specialize the matrix for a given constructor
    /// This filters rows that match the constructor and expands nested patterns
    fn specialize(&self, ctor: &Constructor, arity: usize) -> PatternMatrix {
        let mut result = PatternMatrix::new();

        for row in &self.rows {
            if row.patterns.is_empty() {
                continue;
            }

            let first_pat = &row.patterns[0];
            let rest_patterns = &row.patterns[1..];

            match &first_pat.constructor {
                Constructor::Wildcard => {
                    // Wildcard matches any constructor - expand with arity wildcards
                    let mut new_patterns = vec![SimplePattern::wildcard(); arity];
                    new_patterns.extend(rest_patterns.iter().cloned());
                    result.rows.push(PatternRow {
                        patterns: new_patterns,
                        case_index: row.case_index,
                        has_guard: row.has_guard,
                        is_useful: row.is_useful,
                    });
                }
                c if c == ctor || c.subsumes(ctor) => {
                    // Matching constructor - use sub-patterns
                    let mut new_patterns = first_pat.sub_patterns.clone();
                    new_patterns.extend(rest_patterns.iter().cloned());
                    result.rows.push(PatternRow {
                        patterns: new_patterns,
                        case_index: row.case_index,
                        has_guard: row.has_guard,
                        is_useful: row.is_useful,
                    });
                }
                _ => {
                    // Different constructor - skip this row
                }
            }
        }

        result
    }

    /// Check if a pattern row is useful (not redundant) incrementally
    /// This is called during matrix construction for O(n²) complexity
    fn is_row_useful_incremental(&self, row: &PatternRow, ctors: &ConstructorSet) -> bool {
        // If current matrix is already exhaustive, new row is redundant
        if self.compute_missing_patterns(ctors).is_none() {
            return false;
        }

        // Create a temporary matrix with the new row
        let mut test_matrix = PatternMatrix {
            rows: self.rows.clone(),
        };
        test_matrix.rows.push(row.clone());

        // The row is useful if it changes the witness
        let before_witness = self.compute_missing_patterns(ctors);
        let after_witness = test_matrix.compute_missing_patterns(ctors);

        match (before_witness, after_witness) {
            (Some(_), None) => true,                        // Made it exhaustive
            (Some(before), Some(after)) => before != after, // Changed the witness
            _ => false,
        }
    }

    /// Get all non-useful (redundant) row indices
    fn get_redundant_rows(&self) -> Vec<usize> {
        self.rows
            .iter()
            .enumerate()
            .filter(|(_, row)| !row.is_useful)
            .map(|(i, _)| i)
            .collect()
    }
}

/// Get the arity (number of sub-patterns) for a constructor
fn get_constructor_arity(ctor: &Constructor) -> usize {
    match ctor {
        Constructor::Tuple(n) | Constructor::List(n) | Constructor::StarList(n) => *n,
        Constructor::Class(_, n) => *n,
        Constructor::MappingKeys(keys) => keys.len(),
        Constructor::MappingRest => 0,
        Constructor::OrPattern(_) => 0, // Or-patterns are expanded before arity is checked
        Constructor::True
        | Constructor::False
        | Constructor::None
        | Constructor::Int(_)
        | Constructor::Str(_) => 0,
        Constructor::Wildcard => 0,
    }
}

/// Set of constructors for a given type
struct ConstructorSet {
    /// Known constructors from patterns
    known: HashSet<Constructor>,
    /// Type being matched
    match_type: Type,
}

impl ConstructorSet {
    fn new(match_type: Type) -> Self {
        Self {
            known: HashSet::new(),
            match_type,
        }
    }

    fn add(&mut self, ctor: Constructor) {
        self.known.insert(ctor);
    }

    /// Get the complete constructor set for exhaustiveness checking
    /// Returns all constructors that need to be checked
    fn get_complete_set(&self, used_ctors: &HashSet<Constructor>) -> Vec<Constructor> {
        match &self.match_type {
            Type::Bool => {
                vec![Constructor::True, Constructor::False]
            }
            Type::None => {
                vec![Constructor::None]
            }
            Type::Int => {
                // For integers, collect all literal values seen
                let mut int_ctors: Vec<Constructor> = used_ctors
                    .iter()
                    .filter_map(|c| {
                        if let Constructor::Int(n) = c {
                            Some(Constructor::Int(*n))
                        } else {
                            None
                        }
                    })
                    .collect();

                // If there's already a wildcard, we're done
                if used_ctors.contains(&Constructor::Wildcard) {
                    return vec![Constructor::Wildcard];
                }

                // If we have too many literals, use wildcard
                if int_ctors.len() > MAX_INT_SAMPLES {
                    vec![Constructor::Wildcard]
                } else if int_ctors.is_empty() {
                    // No literals seen - must have wildcard
                    vec![Constructor::Wildcard]
                } else {
                    // Finite set - check each literal + wildcard for others
                    int_ctors.push(Constructor::Wildcard);
                    int_ctors
                }
            }
            Type::Str => {
                // For strings, collect all literal values seen
                let mut str_ctors: Vec<Constructor> = used_ctors
                    .iter()
                    .filter_map(|c| {
                        if let Constructor::Str(s) = c {
                            Some(Constructor::Str(s.clone()))
                        } else {
                            None
                        }
                    })
                    .collect();

                // If there's already a wildcard, we're done
                if used_ctors.contains(&Constructor::Wildcard) {
                    return vec![Constructor::Wildcard];
                }

                // Strings are infinite, so always require a wildcard
                if str_ctors.len() > MAX_STR_SAMPLES {
                    vec![Constructor::Wildcard]
                } else {
                    str_ctors.push(Constructor::Wildcard);
                    str_ctors
                }
            }
            Type::Tuple(types) => {
                vec![Constructor::Tuple(types.len())]
            }
            Type::List(_) => {
                // Check for star patterns
                let has_star = used_ctors
                    .iter()
                    .any(|c| matches!(c, Constructor::StarList(_)));

                if has_star {
                    // Star patterns cover remaining lengths based on fixed prefix/suffix
                    // Analyze all list patterns to find minimum length requirements
                    let mut min_required_length = 0;
                    let mut star_pattern_exists = false;

                    for ctor in used_ctors {
                        if let Constructor::StarList(fixed_count) = ctor {
                            star_pattern_exists = true;
                            min_required_length = min_required_length.max(*fixed_count);
                        } else if let Constructor::List(len) = ctor {
                            min_required_length = min_required_length.max(*len);
                        }
                    }

                    // If star pattern exists with fixed prefix/suffix, it covers all lengths >= min
                    if star_pattern_exists {
                        // Also check if all specific lengths below the minimum are covered
                        let specific_lengths: HashSet<usize> = used_ctors
                            .iter()
                            .filter_map(|c| {
                                if let Constructor::List(len) = c {
                                    Some(*len)
                                } else {
                                    None
                                }
                            })
                            .collect();

                        // Check if we have gaps (e.g., patterns for [1] and [3] but not [2])
                        let mut has_gaps = false;
                        if let Some(&max_specific) = specific_lengths.iter().max() {
                            for i in 0..=max_specific {
                                if !specific_lengths.contains(&i) && i < min_required_length {
                                    has_gaps = true;
                                    break;
                                }
                            }
                        }

                        if has_gaps {
                            // Return missing lengths for better error reporting
                            let missing: Vec<Constructor> = (0..min_required_length)
                                .filter(|i| !specific_lengths.contains(i))
                                .map(Constructor::List)
                                .collect();
                            missing
                        } else {
                            // All cases covered by star pattern and specific patterns
                            vec![]
                        }
                    } else {
                        vec![]
                    }
                } else if used_ctors.contains(&Constructor::Wildcard) {
                    vec![Constructor::Wildcard]
                } else {
                    // Collect all specific lengths
                    let mut lengths: Vec<usize> = used_ctors
                        .iter()
                        .filter_map(|c| {
                            if let Constructor::List(n) = c {
                                Some(*n)
                            } else {
                                None
                            }
                        })
                        .collect();
                    lengths.sort_unstable();
                    lengths.dedup();

                    if lengths.is_empty() {
                        vec![Constructor::Wildcard]
                    } else {
                        // Add wildcard for other lengths
                        let mut result: Vec<Constructor> =
                            lengths.into_iter().map(Constructor::List).collect();
                        result.push(Constructor::Wildcard);
                        result
                    }
                }
            }
            Type::Dict(_, _) => {
                // Check for mapping patterns with specific keys
                let has_wildcard = used_ctors.contains(&Constructor::Wildcard);
                let has_rest = used_ctors.contains(&Constructor::MappingRest);

                if has_wildcard || has_rest {
                    // Wildcard or rest pattern covers all mappings
                    vec![Constructor::Wildcard]
                } else {
                    // Collect all key sets
                    let key_sets: Vec<Vec<String>> = used_ctors
                        .iter()
                        .filter_map(|c| {
                            if let Constructor::MappingKeys(keys) = c {
                                Some(keys.clone())
                            } else {
                                None
                            }
                        })
                        .collect();

                    if key_sets.is_empty() {
                        vec![Constructor::Wildcard]
                    } else {
                        // Compute minimal key coverage
                        // For mapping patterns, we need to check all combinations of key presence
                        // Strategy: collect all unique keys mentioned
                        let mut all_keys = HashSet::new();
                        for keys in &key_sets {
                            for key in keys {
                                all_keys.insert(key.clone());
                            }
                        }

                        // If there are many keys, use wildcard to avoid exponential blow-up
                        if all_keys.len() > 8 {
                            vec![Constructor::Wildcard]
                        } else {
                            // Return the key sets we've seen plus a wildcard for other combinations
                            let mut result: Vec<Constructor> =
                                key_sets.into_iter().map(Constructor::MappingKeys).collect();
                            // Always require wildcard since dicts can have any keys
                            result.push(Constructor::Wildcard);
                            result
                        }
                    }
                }
            }
            _ => {
                // For other types, require a wildcard
                vec![Constructor::Wildcard]
            }
        }
    }
}

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

        // Build pattern matrix
        let mut matrix = PatternMatrix::new();
        let mut ctor_set = ConstructorSet::new(subject_type.clone());

        for (i, case) in match_stmt.cases.iter().enumerate() {
            // Validate pattern type compatibility first
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

            // Lower pattern to simple patterns
            let simple_patterns = self.lower_pattern(&case.pattern);

            // Handle or-patterns by expanding into multiple rows
            let expanded = self.expand_or_patterns(vec![simple_patterns]);

            for pattern_vec in expanded {
                // Collect constructors
                for pat in &pattern_vec {
                    self.collect_constructors(pat, &mut ctor_set);
                }

                // Add row with incremental usefulness check
                matrix.add_row(
                    PatternRow {
                        patterns: pattern_vec,
                        case_index: i,
                        has_guard: case.guard.is_some(),
                        is_useful: true, // Will be set by add_row
                    },
                    &ctor_set,
                );
            }
        }

        // Report redundant patterns
        for row_idx in matrix.get_redundant_rows() {
            // Find the original case index
            let case_idx = matrix.rows[row_idx].case_index;
            if case_idx < match_stmt.cases.len() {
                self.errors.push(*error(
                    ErrorKind::UnreachablePattern {
                        reason: "Pattern is redundant and will never match".to_string(),
                    },
                    match_stmt.cases[case_idx].pattern.span(),
                ));
            }
        }

        // Check exhaustiveness
        if let Some(witness) = matrix.is_exhaustive(&ctor_set) {
            let missing = self.format_witness(&witness);
            let match_header_span =
                text_size::TextRange::new(match_stmt.span.start(), match_stmt.subject.span().end());
            self.errors.push(*error(
                ErrorKind::NonExhaustiveMatch {
                    missing_patterns: vec![missing],
                },
                match_header_span,
            ));
        }
    }

    /// Collect all constructors from a pattern recursively
    fn collect_constructors(&self, pattern: &SimplePattern, ctor_set: &mut ConstructorSet) {
        ctor_set.add(pattern.constructor.clone());
        for sub in &pattern.sub_patterns {
            self.collect_constructors(sub, ctor_set);
        }
    }

    /// Lower an AST pattern to simple patterns for the matrix
    fn lower_pattern(&mut self, pattern: &Pattern) -> SimplePattern {
        match pattern {
            Pattern::MatchValue(p) => {
                // Extract literal value
                if let Expr::Constant(c) = &p.value {
                    let ctor = if let Ok(n) = c.value.parse::<i64>() {
                        Constructor::Int(n)
                    } else {
                        Constructor::Str(c.value.to_string())
                    };
                    SimplePattern::from_constructor(ctor, vec![])
                } else {
                    SimplePattern::wildcard()
                }
            }
            Pattern::MatchSingleton(p) => {
                let ctor = match p.value {
                    MatchSingleton::True => Constructor::True,
                    MatchSingleton::False => Constructor::False,
                    MatchSingleton::None => Constructor::None,
                };
                SimplePattern::from_constructor(ctor, vec![])
            }
            Pattern::MatchSequence(p) => {
                // Check for star patterns - handle multiple MatchAs variants
                let has_star = p.patterns.iter().any(|pat| {
                    match pat {
                        // Star patterns can be represented as MatchAs with no nested pattern
                        Pattern::MatchAs(as_pat) if as_pat.pattern.is_none() => true,
                        _ => false,
                    }
                });

                if has_star {
                    // Handle star patterns - count non-star elements for min length
                    let min_length = p
                        .patterns
                        .iter()
                        .filter(|pat| !matches!(pat, Pattern::MatchAs(as_pat) if as_pat.pattern.is_none()))
                        .count();

                    let sub_patterns: Vec<SimplePattern> = p
                        .patterns
                        .iter()
                        .map(|pat| self.lower_pattern(pat))
                        .collect();

                    SimplePattern::from_constructor(Constructor::StarList(min_length), sub_patterns)
                } else {
                    // Regular sequence - determine if tuple or list based on subject type
                    let sub_patterns: Vec<SimplePattern> = p
                        .patterns
                        .iter()
                        .map(|pat| self.lower_pattern(pat))
                        .collect();

                    // Infer from the pattern's inferred type to distinguish tuples from lists
                    let pattern_type = self.infer_pattern_type(pattern);
                    let ctor = match pattern_type {
                        Type::Tuple(_) => Constructor::Tuple(p.patterns.len()),
                        _ => Constructor::List(p.patterns.len()),
                    };
                    SimplePattern::from_constructor(ctor, sub_patterns)
                }
            }
            Pattern::MatchMapping(p) => {
                // Extract keys and sort them for consistent hashing
                let mut keys: Vec<String> = p
                    .keys
                    .iter()
                    .filter_map(|key| {
                        if let Expr::Constant(c) = key {
                            Some(c.value.to_string())
                        } else {
                            None
                        }
                    })
                    .collect();
                keys.sort();

                let sub_patterns: Vec<SimplePattern> = p
                    .patterns
                    .iter()
                    .map(|pat| self.lower_pattern(pat))
                    .collect();

                let ctor = if p.rest.is_some() {
                    Constructor::MappingRest
                } else {
                    Constructor::MappingKeys(keys)
                };

                SimplePattern::from_constructor(ctor, sub_patterns)
            }
            Pattern::MatchOr(p) => {
                // Lower all branches of the or-pattern
                let branches: Vec<SimplePattern> = p
                    .patterns
                    .iter()
                    .map(|pat| self.lower_pattern(pat))
                    .collect();

                if branches.is_empty() {
                    SimplePattern::wildcard()
                } else if branches.len() == 1 {
                    // Single branch - just return it
                    branches.into_iter().next().unwrap()
                } else {
                    // Multiple branches - create or-pattern constructor
                    SimplePattern::from_constructor(Constructor::OrPattern(branches), vec![])
                }
            }
            Pattern::MatchAs(p) => {
                if let Some(nested) = &p.pattern {
                    self.lower_pattern(nested)
                } else {
                    SimplePattern::wildcard()
                }
            }
            Pattern::MatchClass(p) => {
                let class_name = if let Expr::Name(n) = &p.cls {
                    n.id.to_string()
                } else {
                    "Unknown".to_string()
                };

                let total_arity = p.patterns.len() + p.kwd_patterns.len();
                let sub_patterns: Vec<SimplePattern> = p
                    .patterns
                    .iter()
                    .chain(p.kwd_patterns.iter())
                    .map(|pat| self.lower_pattern(pat))
                    .collect();

                SimplePattern::from_constructor(
                    Constructor::Class(class_name, total_arity),
                    sub_patterns,
                )
            }
        }
    }

    /// Expand or-patterns into multiple pattern rows
    /// This handles nested or-patterns by recursively expanding
    ///
    /// For example, `case (1 | 2, 3 | 4):` expands to:
    /// - `case (1, 3):`
    /// - `case (1, 4):`
    /// - `case (2, 3):`
    /// - `case (2, 4):`
    fn expand_or_patterns(&mut self, patterns: Vec<SimplePattern>) -> Vec<Vec<SimplePattern>> {
        // Base case: no patterns
        if patterns.is_empty() {
            return vec![vec![]];
        }

        // Process first pattern
        let first = &patterns[0];
        let rest = &patterns[1..];

        // Check if first pattern is an or-pattern
        let first_expansions = match &first.constructor {
            Constructor::OrPattern(branches) => {
                // Expand each branch and recursively process its sub-patterns
                branches
                    .iter()
                    .flat_map(|branch| {
                        // For each branch, recursively expand if it contains or-patterns
                        self.expand_simple_pattern(branch)
                    })
                    .collect::<Vec<SimplePattern>>()
            }
            _ => {
                // Not an or-pattern, recursively expand sub-patterns if any
                self.expand_simple_pattern(first)
            }
        };

        // Recursively expand the rest
        let rest_expansions = self.expand_or_patterns(rest.to_vec());

        // Cartesian product: combine each first expansion with each rest expansion
        let mut result = Vec::new();
        for first_pat in &first_expansions {
            for rest_pats in &rest_expansions {
                let mut row = vec![first_pat.clone()];
                row.extend(rest_pats.iter().cloned());
                result.push(row);
            }
        }

        result
    }

    /// Expand a single simple pattern that may contain or-patterns in its sub-patterns
    fn expand_simple_pattern(&mut self, pattern: &SimplePattern) -> Vec<SimplePattern> {
        match &pattern.constructor {
            Constructor::OrPattern(branches) => {
                // This is an or-pattern - return all branches
                branches.clone()
            }
            _ if pattern.sub_patterns.is_empty() => {
                // No sub-patterns - return as-is
                vec![pattern.clone()]
            }
            _ => {
                // Has sub-patterns - recursively expand them
                let sub_expansions = self.expand_or_patterns(pattern.sub_patterns.clone());

                // Create a variant for each expansion of sub-patterns
                sub_expansions
                    .into_iter()
                    .map(|subs| SimplePattern::from_constructor(pattern.constructor.clone(), subs))
                    .collect()
            }
        }
    }

    /// Format a witness pattern for error messages
    fn format_witness(&self, witness: &[SimplePattern]) -> String {
        if witness.is_empty() {
            return "_".to_string();
        }

        let patterns: Vec<String> = witness
            .iter()
            .map(|pat| self.format_simple_pattern(pat))
            .collect();

        if patterns.len() == 1 {
            patterns[0].clone()
        } else {
            format!("({})", patterns.join(", "))
        }
    }

    /// Format a simple pattern for display with improved readability
    fn format_simple_pattern(&self, pat: &SimplePattern) -> String {
        match &pat.constructor {
            Constructor::Int(n) => n.to_string(),
            Constructor::Str(s) => format!("\"{}\"", s),
            Constructor::True => "True".to_string(),
            Constructor::False => "False".to_string(),
            Constructor::None => "None".to_string(),
            Constructor::Tuple(n) => {
                if pat.sub_patterns.is_empty() {
                    // Format as (_, _, _) for better readability
                    let wildcards = vec!["_"; *n];
                    format!("({})", wildcards.join(", "))
                } else {
                    let subs: Vec<String> = pat
                        .sub_patterns
                        .iter()
                        .map(|p| self.format_simple_pattern(p))
                        .collect();
                    if subs.len() == 1 {
                        format!("({},)", subs[0]) // Single-element tuple
                    } else {
                        format!("({})", subs.join(", "))
                    }
                }
            }
            Constructor::List(n) => {
                if pat.sub_patterns.is_empty() {
                    // Format as [_, _, _] for better readability
                    let wildcards = vec!["_"; *n];
                    format!("[{}]", wildcards.join(", "))
                } else {
                    let subs: Vec<String> = pat
                        .sub_patterns
                        .iter()
                        .map(|p| self.format_simple_pattern(p))
                        .collect();
                    format!("[{}]", subs.join(", "))
                }
            }
            Constructor::StarList(min_length) => {
                if *min_length == 0 {
                    "[*_]".to_string()
                } else {
                    let wildcards = vec!["_"; *min_length];
                    format!("[{}, *_]", wildcards.join(", "))
                }
            }
            Constructor::Class(name, _) => {
                if pat.sub_patterns.is_empty() {
                    format!("{}(_)", name)
                } else {
                    let subs: Vec<String> = pat
                        .sub_patterns
                        .iter()
                        .map(|p| self.format_simple_pattern(p))
                        .collect();
                    format!("{}({})", name, subs.join(", "))
                }
            }
            Constructor::MappingKeys(keys) => {
                if keys.is_empty() {
                    "{}".to_string()
                } else {
                    let key_patterns: Vec<String> =
                        keys.iter().map(|k| format!("\"{}\": _", k)).collect();
                    format!("{{{}}}", key_patterns.join(", "))
                }
            }
            Constructor::MappingRest => "{**_}".to_string(),
            Constructor::OrPattern(branches) => {
                let branch_strs: Vec<String> = branches
                    .iter()
                    .map(|b| self.format_simple_pattern(b))
                    .collect();
                branch_strs.join(" | ")
            }
            Constructor::Wildcard => "_".to_string(),
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
                if let Some(elem_type) = self.get_sequence_element_type(subject_type) {
                    for nested in p.patterns {
                        self.check_pattern_type(nested, &elem_type);
                    }
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
                // Check value patterns against dict value type
                if let Type::Dict(_key_type, value_type) = subject_type {
                    for nested in p.patterns {
                        self.check_pattern_type(nested, value_type);
                    }
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

    fn is_sequence_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::List(_) | Type::Tuple(_))
    }

    fn is_mapping_type(&self, ty: &Type) -> bool {
        matches!(ty, Type::Dict(_, _))
    }

    fn get_sequence_element_type(&self, ty: &Type) -> Option<Type> {
        match ty {
            Type::List(elem) => Some((**elem).clone()),
            Type::Tuple(elems) if !elems.is_empty() => {
                // For tuples, return union of all element types
                if elems.iter().all(|t| t == &elems[0]) {
                    Some(elems[0].clone())
                } else {
                    Some(Type::Union(elems.clone()))
                }
            }
            _ => None,
        }
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

    /// Infer the type of a pattern from type inference context
    fn infer_pattern_type(&self, pattern: &Pattern) -> Type {
        let span = pattern.span();
        let start: usize = span.start().into();
        let end: usize = span.end().into();
        self.context
            .get_type_by_span(start, end)
            .cloned()
            .unwrap_or(Type::Unknown)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constructor_subsumption() {
        assert!(Constructor::Wildcard.subsumes(&Constructor::Int(42)));
        assert!(Constructor::Int(42).subsumes(&Constructor::Int(42)));
        assert!(!Constructor::Int(42).subsumes(&Constructor::Int(43)));
        assert!(!Constructor::Int(42).subsumes(&Constructor::Wildcard));

        // Star patterns subsume exact lengths
        assert!(Constructor::StarList(2).subsumes(&Constructor::List(2)));
        assert!(Constructor::StarList(2).subsumes(&Constructor::List(3)));
        assert!(!Constructor::StarList(3).subsumes(&Constructor::List(2)));
    }

    #[test]
    fn test_simple_pattern_wildcard() {
        let pat = SimplePattern::wildcard();
        assert_eq!(pat.constructor, Constructor::Wildcard);
        assert!(pat.sub_patterns.is_empty());
    }

    #[test]
    fn test_constructor_arity() {
        assert_eq!(get_constructor_arity(&Constructor::Int(42)), 0);
        assert_eq!(get_constructor_arity(&Constructor::Tuple(3)), 3);
        assert_eq!(get_constructor_arity(&Constructor::List(2)), 2);
        assert_eq!(get_constructor_arity(&Constructor::StarList(1)), 1);
        assert_eq!(
            get_constructor_arity(&Constructor::Class("Point".to_string(), 2)),
            2
        );

        let keys: Vec<String> = vec!["a".to_string(), "b".to_string()];
        assert_eq!(get_constructor_arity(&Constructor::MappingKeys(keys)), 2);
    }

    #[test]
    fn test_bool_exhaustiveness() {
        let ctors = ConstructorSet::new(Type::Bool);
        let complete = ctors.get_complete_set(&HashSet::new());
        assert_eq!(complete.len(), 2);
        assert!(complete.contains(&Constructor::True));
        assert!(complete.contains(&Constructor::False));
    }

    #[test]
    fn test_int_with_wildcard() {
        let ctors = ConstructorSet::new(Type::Int);
        let mut used = HashSet::new();
        used.insert(Constructor::Wildcard);
        let complete = ctors.get_complete_set(&used);
        assert_eq!(complete.len(), 1);
        assert!(complete.contains(&Constructor::Wildcard));
    }

    #[test]
    fn test_star_pattern_subsumption() {
        // [*x] subsumes [], [_], [_, _], etc.
        assert!(Constructor::StarList(0).subsumes(&Constructor::List(0)));
        assert!(Constructor::StarList(0).subsumes(&Constructor::List(5)));

        // [_, *x] subsumes [_], [_, _], but not []
        assert!(Constructor::StarList(1).subsumes(&Constructor::List(1)));
        assert!(Constructor::StarList(1).subsumes(&Constructor::List(2)));
        assert!(!Constructor::StarList(1).subsumes(&Constructor::List(0)));

        // Star patterns also subsume tuples
        assert!(Constructor::StarList(0).subsumes(&Constructor::Tuple(3)));
        assert!(Constructor::StarList(2).subsumes(&Constructor::Tuple(2)));
        assert!(!Constructor::StarList(3).subsumes(&Constructor::Tuple(2)));
    }
}
