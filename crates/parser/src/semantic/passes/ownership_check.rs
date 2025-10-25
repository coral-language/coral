// Ownership analysis for implicit smart memory management
//
// This pass analyzes variable lifetimes and usage patterns to provide
// recommendations for code generation:
// 1. Move vs copy decisions based on last-use analysis
// 2. Stack vs heap allocation based on escape analysis
// 3. Weak reference insertion for cycle breaking
// 4. Resource cleanup validation
//
// Coral allows Python-like permissive syntax while the compiler determines
// optimal memory strategies through usage analysis and control flow graphs.

use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::ast::patterns::Pattern;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::control_flow::{BlockId, ControlFlowGraph, StmtKind, Terminator};
use std::collections::{HashMap, HashSet, VecDeque};
use text_size::TextRange;

/// Recommendations from ownership analysis for code generation
#[derive(Debug, Clone, Default)]
pub struct OwnershipRecommendations {
    /// Variables that can be moved at specific locations (var_name -> locations)
    pub move_candidates: HashMap<String, Vec<MoveCandidate>>,
    /// Variables needing weak references to break cycles
    pub cycle_breaks: Vec<(String, String)>,
    /// Variables safe for stack allocation (don't escape function)
    pub stack_safe_vars: HashSet<String>,
    /// Per-function analysis results
    pub function_analyses: HashMap<String, FunctionOwnershipAnalysis>,
}

#[derive(Debug, Clone)]
pub struct MoveCandidate {
    /// Location where variable can be moved (last use)
    pub location: TextRange,
    /// Why this is safe to move
    pub reason: MoveReason,
}

#[derive(Debug, Clone)]
pub enum MoveReason {
    /// Last use in function, never used again
    LastUseInFunction,
    /// Last use before conditional that doesn't use it
    LastUseBeforeBranch,
    /// Used as function argument and not used after call
    FunctionArgument { call_site: TextRange },
}

#[derive(Debug, Clone)]
pub struct FunctionOwnershipAnalysis {
    /// All variable last-use locations
    pub last_uses: HashMap<String, Vec<TextRange>>,
    /// Variables that escape the function
    pub escaped_vars: HashSet<String>,
}

/// Memory safety checker
pub struct OwnershipChecker {
    /// Current lifetime states of variables
    lifetime_states: HashMap<String, LifetimeState>,
    /// Reference counting information (compile-time analysis)
    references: HashMap<String, ReferenceInfo>,
    /// Lifetime tracking for automatic cleanup
    lifetimes: HashMap<String, Lifetime>,
    /// Current scope depth
    scope_depth: usize,
    /// Resources that need cleanup
    resources: Vec<Resource>,
    /// Escape analysis results (for optimization - stack allocation hints)
    escape_status: HashMap<String, EscapeStatus>,
    /// Variables marked as weak references
    weak_references: HashMap<String, Vec<String>>,
    /// Errors found during checking
    errors: Vec<Error>,
    /// Recommendations being built during analysis
    recommendations: OwnershipRecommendations,
}

/// Represents the lifetime state of a value (for automatic memory management)
#[derive(Debug, Clone, PartialEq)]
enum LifetimeState {
    /// Value is alive and can be used
    Alive,
    /// Value's lifetime has ended (out of scope)
    OutOfScope { end_span: TextRange },
}

/// Tracks reference count information for compile-time analysis
#[derive(Debug, Clone)]
struct ReferenceInfo {
    /// Number of strong references to this value
    ref_count: usize,
    /// Number of weak references (don't prevent cleanup)
    weak_ref_count: usize,
    /// Locations where references are created
    ref_spans: Vec<TextRange>,
    /// Whether this value is part of a potential cycle
    in_cycle: bool,
    /// Variables that this variable references (for cycle detection)
    references_to: Vec<String>,
}

/// Escape analysis result for optimization
///
/// Results are consumed by recommendations to codegen for allocation strategy.
#[derive(Debug, Clone, Copy, PartialEq)]
enum EscapeStatus {
    /// Value never escapes current function (can be stack-allocated)
    NoEscape,
    /// Value escapes or might escape via return or assignment to outer scope
    Escapes,
}

/// Tracks variable lifetime scope
#[derive(Debug, Clone)]
struct Lifetime {
    /// When the variable was created
    creation_span: TextRange,
    /// When the variable goes out of scope (None if still alive)
    end_span: Option<TextRange>,
    /// Scope depth (for nested scopes)
    scope_depth: usize,
    /// Whether automatic cleanup is needed at scope exit
    needs_cleanup: bool,
}

/// Tracks resource types that need cleanup
#[derive(Debug, Clone, PartialEq)]
enum ResourceType {
    File,
    Connection,
    Lock,
    Transaction,
    Iterator,
}

impl ResourceType {
    fn from_type_name(type_name: &str) -> Option<Self> {
        match type_name {
            "File" | "TextIOWrapper" | "BufferedReader" | "BufferedWriter" => Some(Self::File),
            "Connection" | "DatabaseConnection" | "Socket" => Some(Self::Connection),
            "Lock" | "RLock" | "Semaphore" => Some(Self::Lock),
            "Transaction" => Some(Self::Transaction),
            "Iterator" => Some(Self::Iterator),
            _ => None,
        }
    }
}

impl std::fmt::Display for ResourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::File => write!(f, "File"),
            Self::Connection => write!(f, "Connection"),
            Self::Lock => write!(f, "Lock"),
            Self::Transaction => write!(f, "Transaction"),
            Self::Iterator => write!(f, "Iterator"),
        }
    }
}

/// Resource tracking information
#[derive(Debug, Clone)]
struct Resource {
    resource_type: ResourceType,
    var_name: String,
    creation_span: TextRange,
    is_cleaned_up: bool,
}

impl Default for OwnershipChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl OwnershipChecker {
    pub fn new() -> Self {
        Self {
            lifetime_states: HashMap::new(),
            references: HashMap::new(),
            lifetimes: HashMap::new(),
            scope_depth: 0,
            resources: Vec::new(),
            escape_status: HashMap::new(),
            weak_references: HashMap::new(),
            errors: Vec::new(),
            recommendations: OwnershipRecommendations::default(),
        }
    }

    pub fn check_module(&mut self, module: &Module) -> (Vec<Error>, OwnershipRecommendations) {
        for stmt in module.body {
            self.analyze_stmt(stmt);
        }

        // Build final recommendations
        self.finalize_recommendations();

        // Detect reference cycles in the object graph
        self.detect_cycles();

        // Optimize reference counting operations
        self.optimize_references();

        // Check for uncleaned resources at the end
        self.check_resource_cleanup();

        (self.errors.clone(), self.recommendations.clone())
    }

    /// Analyze a function using its control flow graph
    fn analyze_function(
        &mut self,
        func_name: &str,
        func_def: &crate::ast::nodes::FuncDefStmt,
        cfg: &ControlFlowGraph,
    ) {
        let mut analysis = FunctionOwnershipAnalysis {
            last_uses: HashMap::new(),
            escaped_vars: HashSet::new(),
        };

        // Compute last uses per variable using CFG
        let last_uses = self.compute_last_uses(cfg);

        // Determine which variables escape
        let escaped = self.compute_escaped_vars(func_def, cfg);

        analysis.last_uses = last_uses;
        analysis.escaped_vars = escaped;

        self.recommendations
            .function_analyses
            .insert(func_name.to_string(), analysis.clone());

        // Mark move candidates based on analysis
        self.mark_move_candidates(&analysis);
    }

    /// Compute last uses for all variables in the CFG
    fn compute_last_uses(&self, cfg: &ControlFlowGraph) -> HashMap<String, Vec<TextRange>> {
        let mut last_uses: HashMap<String, Vec<TextRange>> = HashMap::new();

        // For each variable used in the CFG
        for (block_id, block) in &cfg.blocks {
            for stmt in &block.statements {
                for var_used in &stmt.uses {
                    // Check if this is the last use in any path
                    if self.is_last_use_in_paths(var_used, block_id, stmt.span, cfg) {
                        last_uses
                            .entry(var_used.clone())
                            .or_default()
                            .push(stmt.span);
                    }
                }
            }
        }

        last_uses
    }

    /// Check if variable use is last use on all forward paths
    fn is_last_use_in_paths(
        &self,
        var_name: &str,
        from_block: &BlockId,
        use_span: TextRange,
        cfg: &ControlFlowGraph,
    ) -> bool {
        // Traverse all successor paths from this block
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(from_block.clone());

        while let Some(block_id) = queue.pop_front() {
            if !visited.insert(block_id.clone()) {
                continue;
            }

            let block = match cfg.blocks.get(&block_id) {
                Some(b) => b,
                None => continue,
            };

            // Check if variable is used again in this block (after use_span)
            for stmt in &block.statements {
                if stmt.span.start() > use_span.end() && stmt.uses.contains(&var_name.to_string()) {
                    return false; // Used again - not last use
                }
            }

            // Add successors to queue
            for (succ_id, _) in &block.successors {
                queue.push_back(succ_id.clone());
            }
        }

        true // Not used again on any path
    }

    /// Determine which variables escape the function scope
    fn compute_escaped_vars(
        &self,
        func_def: &crate::ast::nodes::FuncDefStmt,
        cfg: &ControlFlowGraph,
    ) -> HashSet<String> {
        let mut escaped = HashSet::new();

        // 1. Variables returned from function escape
        for block in cfg.blocks.values() {
            if let Some(Terminator::Return) = block.terminator {
                for stmt in &block.statements {
                    if matches!(stmt.kind, StmtKind::Return) {
                        escaped.extend(stmt.uses.iter().cloned());
                    }
                }
            }
        }

        // 2. Variables captured by nested functions escape
        // Walk the function body to find nested function definitions
        self.detect_nested_function_captures(func_def, &mut escaped);

        // 3. Variables assigned to outer scope (would need outer scope access - not available)
        // This would require access to parent scope, which isn't available during function analysis.
        // Assumption: Variables are only escaping via return statements or nested function captures.

        escaped
    }

    /// Detect variables captured by nested functions
    fn detect_nested_function_captures(
        &self,
        func_def: &crate::ast::nodes::FuncDefStmt,
        escaped: &mut HashSet<String>,
    ) {
        for stmt in func_def.body {
            self.find_captured_vars_in_stmt(stmt, escaped);
        }
    }

    /// Recursively find captured variables in nested functions
    fn find_captured_vars_in_stmt(&self, stmt: &Stmt, escaped: &mut HashSet<String>) {
        match stmt {
            Stmt::FuncDef(nested_func) => {
                // Any variable used in nested function that's not a parameter escapes
                self.find_captured_vars_in_function(nested_func, escaped);
            }
            Stmt::ClassDef(class_def) => {
                // Check methods in class
                for stmt in class_def.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::If(if_stmt) => {
                for stmt in if_stmt.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
                for stmt in if_stmt.orelse {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::For(for_stmt) => {
                for stmt in for_stmt.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
                for stmt in for_stmt.orelse {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::While(while_stmt) => {
                for stmt in while_stmt.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
                for stmt in while_stmt.orelse {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::Try(try_stmt) => {
                for stmt in try_stmt.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
                for handler in try_stmt.handlers {
                    for stmt in handler.body {
                        self.find_captured_vars_in_stmt(stmt, escaped);
                    }
                }
                for stmt in try_stmt.finalbody {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::With(with_stmt) => {
                for stmt in with_stmt.body {
                    self.find_captured_vars_in_stmt(stmt, escaped);
                }
            }
            Stmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    for stmt in case.body {
                        self.find_captured_vars_in_stmt(stmt, escaped);
                    }
                }
            }
            _ => {}
        }
    }

    /// Find variables used in a nested function
    fn find_captured_vars_in_function(
        &self,
        nested_func: &crate::ast::nodes::FuncDefStmt,
        escaped: &mut HashSet<String>,
    ) {
        // Get parameter names so we don't count them as captured
        let mut param_names = HashSet::new();
        for param in nested_func.args.args {
            param_names.insert(param.arg.to_string());
        }

        // Walk the nested function body and collect all variables used
        let mut used_vars = HashSet::new();
        self.collect_used_vars_in_stmts(nested_func.body, &mut used_vars);

        // Variables that are used but not parameters are captured (escape parent scope)
        for var in used_vars {
            if !param_names.contains(&var) {
                escaped.insert(var);
            }
        }
    }

    /// Collect all variables used in a sequence of statements
    fn collect_used_vars_in_stmts(&self, stmts: &[Stmt], used_vars: &mut HashSet<String>) {
        for stmt in stmts {
            self.collect_used_vars_in_stmt(stmt, used_vars);
        }
    }

    /// Collect all variables used in a statement (recursive)
    fn collect_used_vars_in_stmt(&self, stmt: &Stmt, used_vars: &mut HashSet<String>) {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                self.collect_used_vars_in_expr(&expr_stmt.value, used_vars);
            }
            Stmt::Assign(assign) => {
                self.collect_used_vars_in_expr(&assign.value, used_vars);
            }
            Stmt::AnnAssign(ann_assign) => {
                if let Some(value) = &ann_assign.value {
                    self.collect_used_vars_in_expr(value, used_vars);
                }
            }
            Stmt::Return(ret) => {
                if let Some(value) = &ret.value {
                    self.collect_used_vars_in_expr(value, used_vars);
                }
            }
            Stmt::If(if_stmt) => {
                self.collect_used_vars_in_expr(&if_stmt.test, used_vars);
                self.collect_used_vars_in_stmts(if_stmt.body, used_vars);
                self.collect_used_vars_in_stmts(if_stmt.orelse, used_vars);
            }
            Stmt::For(for_stmt) => {
                self.collect_used_vars_in_expr(&for_stmt.iter, used_vars);
                self.collect_used_vars_in_stmts(for_stmt.body, used_vars);
            }
            Stmt::While(while_stmt) => {
                self.collect_used_vars_in_expr(&while_stmt.test, used_vars);
                self.collect_used_vars_in_stmts(while_stmt.body, used_vars);
            }
            _ => {}
        }
    }

    /// Collect all variables used in an expression
    #[allow(clippy::only_used_in_recursion)]
    fn collect_used_vars_in_expr(&self, expr: &Expr, used_vars: &mut HashSet<String>) {
        match expr {
            Expr::Name(name_expr) => {
                used_vars.insert(name_expr.id.to_string());
            }
            Expr::Call(call_expr) => {
                self.collect_used_vars_in_expr(call_expr.func, used_vars);
                for arg in call_expr.args {
                    self.collect_used_vars_in_expr(arg, used_vars);
                }
            }
            Expr::Attribute(attr_expr) => {
                self.collect_used_vars_in_expr(attr_expr.value, used_vars);
            }
            Expr::Subscript(subscript_expr) => {
                self.collect_used_vars_in_expr(subscript_expr.value, used_vars);
                self.collect_used_vars_in_expr(subscript_expr.slice, used_vars);
            }
            Expr::BinOp(binop_expr) => {
                self.collect_used_vars_in_expr(binop_expr.left, used_vars);
                self.collect_used_vars_in_expr(binop_expr.right, used_vars);
            }
            Expr::UnaryOp(unary_expr) => {
                self.collect_used_vars_in_expr(unary_expr.operand, used_vars);
            }
            Expr::Compare(compare_expr) => {
                self.collect_used_vars_in_expr(compare_expr.left, used_vars);
                for comp in compare_expr.comparators {
                    self.collect_used_vars_in_expr(comp, used_vars);
                }
            }
            Expr::List(list_expr) => {
                for elt in list_expr.elts {
                    self.collect_used_vars_in_expr(elt, used_vars);
                }
            }
            Expr::Tuple(tuple_expr) => {
                for elt in tuple_expr.elts {
                    self.collect_used_vars_in_expr(elt, used_vars);
                }
            }
            Expr::Dict(dict_expr) => {
                for key in dict_expr.keys.iter().flatten() {
                    self.collect_used_vars_in_expr(key, used_vars);
                }
                for value in dict_expr.values {
                    self.collect_used_vars_in_expr(value, used_vars);
                }
            }
            _ => {}
        }
    }

    /// Mark variables as move candidates based on last-use analysis
    fn mark_move_candidates(&mut self, function_analysis: &FunctionOwnershipAnalysis) {
        for (var_name, last_use_locations) in &function_analysis.last_uses {
            // Don't mark as move if it escapes
            if function_analysis.escaped_vars.contains(var_name) {
                continue;
            }

            // Get reference info
            let ref_info = match self.references.get(var_name) {
                Some(info) => info,
                None => continue,
            };

            // If single reference and doesn't escape -> move candidate
            if ref_info.ref_count == 1 && !ref_info.in_cycle {
                for location in last_use_locations {
                    self.recommendations
                        .move_candidates
                        .entry(var_name.clone())
                        .or_default()
                        .push(MoveCandidate {
                            location: *location,
                            reason: MoveReason::LastUseInFunction,
                        });
                }
            }
        }
    }

    /// Build CFG for a function (uses built-in CFG construction)
    fn build_cfg_for_function(
        &self,
        _func_def: &crate::ast::nodes::FuncDefStmt,
    ) -> ControlFlowGraph {
        // CFG is built during the control_flow pass which runs before ownership_check.
        // For now, we return an empty default CFG since:
        // 1. This pass currently doesn't have access to pre-built CFGs from AnalysisContext
        // 2. The current reference counting + scope tracking provides sufficient analysis
        // 3. Full CFG integration for last-use analysis is a future optimization
        //
        // A future enhancement would be to:
        // - Access AnalysisContext.cfg_cache (once it's exposed)
        // - Build function-specific CFGs on-demand
        // - Use CFG for precise path-sensitive analysis
        ControlFlowGraph::default()
    }

    /// Analyze a statement
    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDef(func_def) => {
                // Build CFG for this function
                let cfg = self.build_cfg_for_function(func_def);

                self.enter_scope();

                // Declare parameters
                for param in func_def.args.args {
                    self.declare_variable(param.arg, func_def.span, None);
                }

                // Visit body
                for stmt in func_def.body {
                    self.analyze_stmt(stmt);
                }

                // Analyze with CFG for last-use detection
                let func_name = func_def.name.to_string();
                self.analyze_function(&func_name, func_def, &cfg);

                self.exit_scope(func_def.span);
            }
            Stmt::ClassDef(class_def) => {
                self.enter_scope();
                for stmt in class_def.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(class_def.span);
            }
            Stmt::For(for_stmt) => {
                // Visit the iterable
                self.analyze_expr(&for_stmt.iter);

                // Declare loop variable
                if let Expr::Name(name_expr) = &for_stmt.target {
                    self.declare_variable(name_expr.id, for_stmt.target.span(), None);
                }

                self.enter_scope();
                for stmt in for_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(for_stmt.span);

                // Visit else clause if present
                for stmt in for_stmt.orelse {
                    self.analyze_stmt(stmt);
                }
            }
            Stmt::While(while_stmt) => {
                self.analyze_expr(&while_stmt.test);
                self.enter_scope();
                for stmt in while_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(while_stmt.span);

                for stmt in while_stmt.orelse {
                    self.analyze_stmt(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                self.analyze_expr(&if_stmt.test);
                self.enter_scope();
                for stmt in if_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(if_stmt.span);

                for stmt in if_stmt.orelse {
                    self.analyze_stmt(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                // With statements automatically manage resource cleanup
                for item in with_stmt.items {
                    self.analyze_expr(&item.context_expr);
                    if let Some(optional_vars) = &item.optional_vars
                        && let Some(var_name) = self.extract_var_name(optional_vars)
                    {
                        self.declare_variable(&var_name, optional_vars.span(), None);
                        // Mark as cleaned up since 'with' handles it
                        self.mark_resource_cleaned(&var_name);
                    }
                }

                self.enter_scope();
                for stmt in with_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(with_stmt.span);
            }
            Stmt::Try(try_stmt) => {
                // Analyze try body
                self.enter_scope();
                for stmt in try_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope(try_stmt.span);

                // Analyze exception handlers
                for handler in try_stmt.handlers {
                    self.enter_scope();
                    if let Some(name) = &handler.name {
                        self.declare_variable(name, handler.span, None);
                    }
                    for stmt in handler.body {
                        self.analyze_stmt(stmt);
                    }
                    self.exit_scope(handler.span);
                }

                // Analyze finally
                if !try_stmt.finalbody.is_empty() {
                    self.enter_scope();
                    for stmt in try_stmt.finalbody {
                        self.analyze_stmt(stmt);
                    }
                    self.exit_scope(try_stmt.span);
                }
            }
            Stmt::Match(match_stmt) => {
                self.analyze_expr(&match_stmt.subject);
                for case in match_stmt.cases {
                    self.enter_scope();
                    self.analyze_pattern(&case.pattern);
                    for stmt in case.body {
                        self.analyze_stmt(stmt);
                    }
                    self.exit_scope(match_stmt.span);
                }
            }
            Stmt::Assign(assign_stmt) => {
                // Visit the value first
                self.analyze_expr(&assign_stmt.value);

                // Check if this is a weak reference creation
                if let Some((_, target)) = self.is_weak_reference(&assign_stmt.value) {
                    // Handle weak reference assignment
                    for assign_target in assign_stmt.targets {
                        if let Some(source) = self.extract_var_name(assign_target) {
                            self.create_weak_reference(&source, &target, assign_target.span());
                        }
                    }
                } else {
                    // Assignment creates references, not moves
                    // Declare/update targets
                    for target in assign_stmt.targets {
                        if let Some(var_name) = self.extract_var_name(target) {
                            // If variable already exists, we're reassigning (old value can be freed)
                            if self.lifetime_states.contains_key(&var_name) {
                                // Automatic cleanup of old value happens here
                                // Decrease ref count of the old value
                                let can_be_freed = self.release_reference(&var_name);

                                // If ref count reaches zero, mark resource as available for cleanup
                                if can_be_freed {
                                    self.mark_resource_cleaned(&var_name);
                                }
                            }

                            self.declare_variable(&var_name, target.span(), None);

                            // Track reference from RHS to LHS
                            if let Some(rhs_var) = self.extract_var_name(&assign_stmt.value) {
                                self.add_reference(&rhs_var, assign_stmt.value.span());

                                // Track reference graph for cycle detection
                                if let Some(ref_info) = self.references.get_mut(&var_name) {
                                    ref_info.references_to.push(rhs_var.clone());
                                }
                            }
                        }
                    }
                }
            }
            Stmt::AnnAssign(ann_assign) => {
                if let Some(value) = &ann_assign.value {
                    self.analyze_expr(value);
                }

                if let Some(var_name) = self.extract_var_name(&ann_assign.target) {
                    // Extract type annotation if available
                    let type_name = if let Expr::Name(name_expr) = &ann_assign.annotation {
                        Some(name_expr.id)
                    } else {
                        None
                    };

                    self.declare_variable(&var_name, ann_assign.target.span(), type_name);
                }
            }
            Stmt::Return(return_stmt) => {
                if let Some(value) = &return_stmt.value {
                    self.analyze_expr(value);

                    // Check if returning a reference to a local variable
                    if let Some(var_name) = self.extract_var_name(value)
                        && let Some(lifetime) = self.lifetimes.get(&var_name)
                        && lifetime.scope_depth > 0
                    {
                        // Returning a local variable - this is OK
                        // The value is copied/ref-counted, not moved
                        // But warn if it's a reference that will dangle
                        if let Some(state) = self.lifetime_states.get(&var_name)
                            && matches!(state, LifetimeState::OutOfScope { .. })
                        {
                            self.errors.push(*error(
                                ErrorKind::DanglingReference {
                                    var_name,
                                    scope_end_span: lifetime.end_span.unwrap_or(value.span()),
                                },
                                value.span(),
                            ));
                        }
                    }
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.analyze_expr(&expr_stmt.value);
            }
            _ => {
                // For other statements, no ownership implications
            }
        }
    }

    /// Analyze pattern for variable bindings
    fn analyze_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::MatchAs(p) => {
                // MatchAs can bind a variable, so declare it
                if let Some(name) = &p.name {
                    self.declare_variable(name, p.span, None);
                }
                // Recursively analyze nested patterns
                if let Some(inner) = &p.pattern {
                    self.analyze_pattern(inner);
                }
            }
            Pattern::MatchOr(p) => {
                // All alternatives in OR pattern declare the same variables
                // (enforced by parser - all branches must bind same names)
                for pat in p.patterns {
                    self.analyze_pattern(pat);
                }
            }
            // Other pattern types don't introduce new variable bindings:
            // - MatchValue: matches literal or attribute, no binding
            // - MatchSingleton: matches None/True/False, no binding
            // - MatchSequence: destructuring handled as sub-patterns (recursion above)
            // - MatchMapping: key-value matching handled as sub-patterns
            // - MatchClass: class pattern matching handled as sub-patterns
            // - MatchStar: unpacking, variable binding handled via MatchAs wrapper
            _ => {}
        }
    }

    /// Analyze an expression
    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Name(name_expr) => {
                // Check if this variable can be used
                self.check_variable_use(name_expr.id, expr.span());
            }
            Expr::Call(call_expr) => {
                self.analyze_expr(call_expr.func);

                // Track call arguments for move optimization
                for arg in call_expr.args {
                    if let Some(_arg_var) = self.extract_var_name(arg) {
                        let _call_span = call_expr.span;
                        // This will be refined during CFG analysis
                    }
                    self.analyze_expr(arg);
                }

                // Check for cleanup methods
                if let Expr::Attribute(attr_expr) = call_expr.func
                    && (attr_expr.attr == "close"
                        || attr_expr.attr == "release"
                        || attr_expr.attr == "commit"
                        || attr_expr.attr == "rollback")
                    && let Some(var_name) = self.extract_var_name(attr_expr.value)
                {
                    self.mark_resource_cleaned(&var_name);
                }
            }
            Expr::Attribute(attr_expr) => {
                self.analyze_expr(attr_expr.value);

                // Track that base object is borrowed through attribute access
                if let Some(base_var) = self.extract_var_name(attr_expr.value) {
                    self.add_reference(&base_var, attr_expr.span);
                }
            }
            Expr::BinOp(binop_expr) => {
                self.analyze_expr(binop_expr.left);
                self.analyze_expr(binop_expr.right);
            }
            Expr::UnaryOp(unary_expr) => {
                self.analyze_expr(unary_expr.operand);
            }
            Expr::Compare(compare_expr) => {
                self.analyze_expr(compare_expr.left);
                for comp in compare_expr.comparators {
                    self.analyze_expr(comp);
                }
            }
            Expr::List(list_expr) => {
                for elt in list_expr.elts {
                    self.analyze_expr(elt);
                }
            }
            Expr::Tuple(tuple_expr) => {
                for elt in tuple_expr.elts {
                    self.analyze_expr(elt);
                }
            }
            Expr::Dict(dict_expr) => {
                for key in dict_expr.keys.iter().flatten() {
                    self.analyze_expr(key);
                }
                for value in dict_expr.values {
                    self.analyze_expr(value);
                }
            }
            Expr::Subscript(subscript_expr) => {
                self.analyze_expr(subscript_expr.value);
                self.analyze_expr(subscript_expr.slice);

                // Track that collection is borrowed through subscript
                if let Some(coll_var) = self.extract_var_name(subscript_expr.value) {
                    self.add_reference(&coll_var, subscript_expr.span);
                }
            }
            _ => {
                // Other expression types don't have ownership implications:
                // - Constant: literals (int, str, bool, None)
                // - BoolOp: already handled left and right in analyze_expr calls
                // - IfExp: condition and values analyzed separately
                // - Lambda: creates function object, no variable capture tracking here
                // - NamedExpr: walrus operator, assignment handled in Stmt::Assign
                // - Starred: unpacking, element analyzed separately
                // - Await, Yield: async/generator constructs handled elsewhere
            }
        }
    }

    /// Enter a new scope
    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Exit current scope, automatically inserting cleanup for local variables
    fn exit_scope(&mut self, scope_end_span: TextRange) {
        // Find variables that need cleanup in this scope
        let vars_to_cleanup: Vec<String> = self
            .lifetimes
            .iter()
            .filter(|(_, lifetime)| {
                lifetime.scope_depth == self.scope_depth && lifetime.needs_cleanup
            })
            .map(|(name, _)| name.clone())
            .collect();

        // Mark their lifetimes as ended (automatic cleanup happens here)
        for var_name in vars_to_cleanup {
            if let Some(lifetime) = self.lifetimes.get_mut(&var_name) {
                lifetime.end_span = Some(scope_end_span);
            }
            self.lifetime_states.insert(
                var_name.clone(),
                LifetimeState::OutOfScope {
                    end_span: scope_end_span,
                },
            );

            // Decrease reference counts for values this variable referenced
            if let Some(ref_info) = self.references.get_mut(&var_name) {
                ref_info.ref_count = ref_info.ref_count.saturating_sub(1);
            }
        }

        self.scope_depth = self.scope_depth.saturating_sub(1);
    }

    /// Track a new variable declaration (automatic lifetime tracking)
    fn declare_variable(&mut self, name: &str, span: TextRange, type_name: Option<&str>) {
        let needs_cleanup = if let Some(type_name) = type_name {
            ResourceType::from_type_name(type_name).is_some()
        } else {
            false
        };

        self.lifetime_states
            .insert(name.to_string(), LifetimeState::Alive);

        self.references.insert(
            name.to_string(),
            ReferenceInfo {
                ref_count: 1,
                weak_ref_count: 0,
                ref_spans: vec![span],
                in_cycle: false,
                references_to: Vec::new(),
            },
        );

        self.lifetimes.insert(
            name.to_string(),
            Lifetime {
                creation_span: span,
                end_span: None,
                scope_depth: self.scope_depth,
                needs_cleanup,
            },
        );

        // Check if this is a resource type that needs tracking
        if let Some(type_name) = type_name
            && let Some(resource_type) = ResourceType::from_type_name(type_name)
        {
            self.resources.push(Resource {
                resource_type,
                var_name: name.to_string(),
                creation_span: span,
                is_cleaned_up: false,
            });
        }
    }

    /// Check if variable can be used (not out of scope)
    fn check_variable_use(&mut self, name: &str, use_span: TextRange) {
        if let Some(state) = self.lifetime_states.get(name) {
            match state {
                LifetimeState::OutOfScope { end_span } => {
                    self.errors.push(*error(
                        ErrorKind::DanglingReference {
                            var_name: name.to_string(),
                            scope_end_span: *end_span,
                        },
                        use_span,
                    ));
                }
                LifetimeState::Alive => {
                    // Valid use - increment reference count
                    if let Some(ref_info) = self.references.get_mut(name) {
                        ref_info.ref_count += 1;
                        ref_info.ref_spans.push(use_span);
                    }
                }
            }
        }
    }

    /// Add a reference to a variable (for reference tracking)
    fn add_reference(&mut self, name: &str, ref_span: TextRange) {
        if let Some(ref_info) = self.references.get_mut(name) {
            ref_info.ref_count += 1;
            ref_info.ref_spans.push(ref_span);
        }
    }

    /// Decrease reference count (called when a reference is dropped or reassigned)
    /// Returns true if the ref count reaches zero (value can be freed)
    fn release_reference(&mut self, name: &str) -> bool {
        if let Some(ref_info) = self.references.get_mut(name)
            && ref_info.ref_count > 0
        {
            ref_info.ref_count -= 1;
            // Value can be freed if no strong references remain
            // (weak references don't keep the value alive)
            return ref_info.ref_count == 0;
        }
        false
    }

    /// Mark a resource as cleaned up
    fn mark_resource_cleaned(&mut self, var_name: &str) {
        for resource in &mut self.resources {
            if resource.var_name == var_name {
                resource.is_cleaned_up = true;
            }
        }
    }

    /// Check for resources that weren't cleaned up
    fn check_resource_cleanup(&mut self) {
        for resource in &self.resources {
            if !resource.is_cleaned_up {
                self.errors.push(*error(
                    ErrorKind::ResourceLeak {
                        resource_type: resource.resource_type.to_string(),
                        var_name: resource.var_name.clone(),
                    },
                    resource.creation_span,
                ));
            }
        }
    }

    /// Extract variable name from expression
    fn extract_var_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Name(name_expr) => Some(name_expr.id.to_string()),
            _ => None,
        }
    }

    /// Create a weak reference to break reference cycles
    fn create_weak_reference(&mut self, source: &str, target: &str, _ref_span: TextRange) {
        // Track the weak reference relationship
        self.weak_references
            .entry(source.to_string())
            .or_default()
            .push(target.to_string());

        // Increment weak ref count on the target
        if let Some(ref_info) = self.references.get_mut(target) {
            ref_info.weak_ref_count += 1;
        }

        // Add to reference graph but with weak strength
        if let Some(ref_info) = self.references.get_mut(source) {
            ref_info.references_to.push(target.to_string());
        }
    }

    /// Check if an expression represents a weak reference creation
    fn is_weak_reference(&self, expr: &Expr) -> Option<(String, String)> {
        // Pattern: weakref.ref(target) or WeakRef(target)
        if let Expr::Call(call) = expr {
            if let Expr::Attribute(attr) = call.func {
                if let Expr::Name(name) = attr.value
                    && name.id == "weakref"
                    && attr.attr == "ref"
                    && let Some(first_arg) = call.args.first()
                    && let Some(target) = self.extract_var_name(first_arg)
                {
                    return Some(("weakref".to_string(), target));
                }
            } else if let Expr::Name(name) = call.func
                && name.id == "WeakRef"
                && let Some(first_arg) = call.args.first()
                && let Some(target) = self.extract_var_name(first_arg)
            {
                return Some(("WeakRef".to_string(), target));
            }
        }
        None
    }

    /// Analyze escape behavior of a variable
    fn analyze_escape(&mut self, var_name: &str) -> EscapeStatus {
        // Check if already analyzed
        if let Some(status) = self.escape_status.get(var_name) {
            return *status;
        }

        let mut status = EscapeStatus::NoEscape;

        // Get lifetime info
        if let Some(lifetime) = self.lifetimes.get(var_name) {
            if let Some(ref_info) = self.references.get(var_name) {
                // If referenced from multiple scopes, it escapes
                if ref_info.ref_count > 1 {
                    status = EscapeStatus::Escapes;
                }

                // If it references other variables, it might escape
                if !ref_info.references_to.is_empty() {
                    status = EscapeStatus::Escapes;
                }
            }

            // Variables in inner scopes (higher depth) that are still alive may escape
            if lifetime.scope_depth > 1
                && matches!(
                    self.lifetime_states.get(var_name),
                    Some(LifetimeState::Alive)
                )
            {
                status = EscapeStatus::Escapes;
            }
        }

        self.escape_status.insert(var_name.to_string(), status);
        status
    }

    /// Detect reference cycles in the object graph
    fn detect_cycles(&mut self) {
        // Build adjacency list from references_to
        let mut graph: HashMap<String, Vec<String>> = HashMap::new();
        for (var, ref_info) in &self.references {
            graph.insert(var.clone(), ref_info.references_to.clone());
        }

        // Use DFS to find strongly connected components (Tarjan's algorithm simplified)
        let mut visited = HashMap::new();
        let mut rec_stack = HashMap::new();

        for var in graph.keys() {
            if !visited.contains_key(var) {
                self.dfs_cycle_detect(var, &graph, &mut visited, &mut rec_stack, &mut Vec::new());
            }
        }
    }

    /// DFS helper for cycle detection
    fn dfs_cycle_detect(
        &mut self,
        var: &str,
        graph: &HashMap<String, Vec<String>>,
        visited: &mut HashMap<String, bool>,
        rec_stack: &mut HashMap<String, bool>,
        path: &mut Vec<String>,
    ) {
        visited.insert(var.to_string(), true);
        rec_stack.insert(var.to_string(), true);
        path.push(var.to_string());

        if let Some(neighbors) = graph.get(var) {
            for neighbor in neighbors {
                if !visited.contains_key(neighbor) {
                    self.dfs_cycle_detect(neighbor, graph, visited, rec_stack, path);
                } else if *rec_stack.get(neighbor).unwrap_or(&false) {
                    // Found a cycle!
                    let cycle_start = path.iter().position(|v| v == neighbor).unwrap_or(0);
                    let cycle_vars: Vec<String> = path[cycle_start..].to_vec();

                    // Mark all variables in cycle
                    for cycle_var in &cycle_vars {
                        if let Some(ref_info) = self.references.get_mut(cycle_var) {
                            ref_info.in_cycle = true;
                        }
                    }

                    // Check if cycle has weak references
                    let has_weak_ref = cycle_vars
                        .iter()
                        .any(|v| self.weak_references.contains_key(v));

                    if !has_weak_ref {
                        // Report circular reference error
                        if let Some(lifetime) = self.lifetimes.get(var) {
                            self.errors.push(*error(
                                ErrorKind::CircularReference {
                                    var_names: cycle_vars,
                                },
                                lifetime.creation_span,
                            ));
                        }
                    }
                }
            }
        }

        rec_stack.insert(var.to_string(), false);
        path.pop();
    }

    /// Optimize reference counting operations
    fn optimize_references(&mut self) {
        for (var_name, ref_info) in &mut self.references {
            // If variable never escapes and has only one strong reference, no ref counting needed
            if ref_info.ref_count <= 1
                && ref_info.weak_ref_count == 0
                && let Some(escape) = self.escape_status.get(var_name)
                && matches!(escape, EscapeStatus::NoEscape)
            {
                // This variable can be stack-allocated, no cleanup needed
                if let Some(lifetime) = self.lifetimes.get_mut(var_name) {
                    lifetime.needs_cleanup = false;
                }
            }
        }
    }

    /// Finalize recommendations from analysis results
    fn finalize_recommendations(&mut self) {
        // Mark cycle breaks from weak references
        for (source, targets) in &self.weak_references {
            for target in targets {
                self.recommendations
                    .cycle_breaks
                    .push((source.clone(), target.clone()));
            }
        }

        // Mark stack-safe variables from escape analysis
        let var_names: Vec<String> = self.references.keys().cloned().collect();
        for var_name in var_names {
            let escape_status = self.analyze_escape(&var_name);
            if matches!(escape_status, EscapeStatus::NoEscape) {
                self.recommendations.stack_safe_vars.insert(var_name);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ownership_checker_creation() {
        let checker = OwnershipChecker::new();
        assert_eq!(checker.scope_depth, 0);
        assert!(checker.lifetime_states.is_empty());
        assert!(checker.references.is_empty());
    }

    #[test]
    fn test_resource_type_detection() {
        assert!(ResourceType::from_type_name("File").is_some());
        assert!(ResourceType::from_type_name("Connection").is_some());
        assert!(ResourceType::from_type_name("Lock").is_some());
        assert!(ResourceType::from_type_name("SomeOtherType").is_none());
    }

    #[test]
    fn test_basic_lifetime_tracking() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_variable("x", span, None);
        assert_eq!(
            checker.lifetime_states.get("x"),
            Some(&LifetimeState::Alive)
        );
    }

    #[test]
    fn test_out_of_scope_detection() {
        let mut checker = OwnershipChecker::new();
        let span1 = TextRange::new(0.into(), 10.into());
        let span2 = TextRange::new(20.into(), 30.into());

        checker.enter_scope();
        checker.declare_variable("x", span1, Some("File"));

        assert_eq!(
            checker.lifetime_states.get("x"),
            Some(&LifetimeState::Alive)
        );

        checker.exit_scope(span2);

        assert!(matches!(
            checker.lifetime_states.get("x"),
            Some(LifetimeState::OutOfScope { .. })
        ));
    }

    #[test]
    fn test_automatic_reference_counting() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_variable("x", span, None);

        if let Some(ref_info) = checker.references.get("x") {
            assert_eq!(ref_info.ref_count, 1);
        }

        checker.add_reference("x", TextRange::new(20.into(), 30.into()));

        if let Some(ref_info) = checker.references.get("x") {
            assert_eq!(ref_info.ref_count, 2);
        }
    }

    #[test]
    fn test_weak_reference_tracking() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_variable("parent", span, None);
        checker.declare_variable("child", span, None);

        checker.create_weak_reference("child", "parent", span);

        if let Some(ref_info) = checker.references.get("parent") {
            assert_eq!(ref_info.weak_ref_count, 1);
            assert_eq!(ref_info.ref_count, 1);
        }

        assert!(checker.weak_references.contains_key("child"));
    }

    #[test]
    fn test_escape_analysis() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.enter_scope();
        checker.declare_variable("local", span, None);
        let status = checker.analyze_escape("local");
        assert!(matches!(status, EscapeStatus::NoEscape));
        checker.exit_scope(span);

        checker.declare_variable("shared", span, None);
        checker.add_reference("shared", TextRange::new(20.into(), 30.into()));
        let status = checker.analyze_escape("shared");
        assert!(matches!(status, EscapeStatus::Escapes));
    }

    #[test]
    fn test_reference_optimization() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.enter_scope();
        checker.declare_variable("temp", span, None);

        checker.analyze_escape("temp");

        checker.optimize_references();

        if let Some(lifetime) = checker.lifetimes.get("temp") {
            assert!(!lifetime.needs_cleanup);
        }
    }

    #[test]
    fn test_cycle_detection() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_variable("a", span, None);
        checker.declare_variable("b", span, None);

        if let Some(ref_info) = checker.references.get_mut("a") {
            ref_info.references_to.push("b".to_string());
        }

        if let Some(ref_info) = checker.references.get_mut("b") {
            ref_info.references_to.push("a".to_string());
        }

        checker.detect_cycles();

        assert!(checker.references.get("a").unwrap().in_cycle);
        assert!(checker.references.get("b").unwrap().in_cycle);

        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0].kind,
            ErrorKind::CircularReference { .. }
        ));
    }

    #[test]
    fn test_cycle_with_weak_reference() {
        let mut checker = OwnershipChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_variable("parent", span, None);
        checker.declare_variable("child", span, None);

        if let Some(ref_info) = checker.references.get_mut("parent") {
            ref_info.references_to.push("child".to_string());
        }

        checker.create_weak_reference("child", "parent", span);

        let initial_error_count = checker.errors.len();
        checker.detect_cycles();

        assert_eq!(checker.errors.len(), initial_error_count);
    }
}
