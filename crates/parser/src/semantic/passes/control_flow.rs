#![allow(clippy::only_used_in_recursion)]

use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::error::diagnostic::Diagnostic;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use std::collections::{HashMap, HashSet, VecDeque};
use text_size::{TextRange, TextSize};

/// Control flow analysis pass - detects unreachable code, validates return paths,
/// and checks exception handling.
///
/// This pass:
/// - Builds a control flow graph (CFG) from the AST
/// - Detects unreachable code (statements after return/raise/etc.)
/// - Validates that all paths return a value (for non-None return types)
/// - Checks exception handling coverage
/// - Validates finally block execution guarantees
///
/// A basic block in the control flow graph
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

impl BlockId {
    pub fn new(id: usize) -> Self {
        BlockId(id)
    }
}

/// Type of control flow edge
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeType {
    /// Normal sequential flow
    Normal,
    /// Conditional branch (true branch)
    True,
    /// Conditional branch (false branch)
    False,
    /// Loop back edge
    Loop,
    /// Exception edge
    Exception,
    /// Finally block edge (always executes)
    Finally,
}

/// A basic block in the control flow graph
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct BasicBlock {
    /// Unique identifier
    id: BlockId,
    /// Statements in this block
    statements: Vec<StmtInfo>,
    /// Successor blocks
    successors: Vec<(BlockId, EdgeType)>,
    /// Whether this block terminates (return, raise, break, continue)
    terminates: bool,
    /// Type of termination (if any)
    terminator: Option<Terminator>,
}

/// Statement information for CFG
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct StmtInfo {
    span: TextRange,
    kind: StmtKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum StmtKind {
    Normal,
    Return,
    Raise,
    Break,
    Continue,
    Pass,
}

/// How a block terminates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum Terminator {
    Return,
    Raise,
    Break,
    Continue,
    /// Implicit (falls through to next block)
    Fallthrough,
}

/// Control flow graph
#[derive(Debug)]
#[allow(dead_code)]
pub struct ControlFlowGraph {
    /// All basic blocks
    blocks: HashMap<BlockId, BasicBlock>,
    /// Entry block (function start)
    entry: BlockId,
    /// Exit block (function end)
    exit: Option<BlockId>,
    /// Next block ID to allocate
    next_id: usize,
    /// Conditions associated with conditional edges (for type narrowing)
    pub edge_conditions: HashMap<(BlockId, BlockId), ConditionInfo>,
}

/// Information about a condition that guards an edge
#[derive(Debug, Clone)]
pub struct ConditionInfo {
    /// The variable being tested
    pub variable: String,
    /// The kind of condition
    pub kind: ConditionKind,
}

/// Type of condition for type narrowing
#[derive(Debug, Clone, PartialEq)]
pub enum ConditionKind {
    /// isinstance(var, Type) check
    IsInstance { check_type: String },
    /// var is None
    IsNone,
    /// var is not None
    IsNotNone,
    /// Truthiness check (if var:)
    Truthy,
    /// Falsy check (if not var:)
    Falsy,
}

impl ControlFlowGraph {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        let entry = BlockId::new(0);
        let mut blocks = HashMap::new();
        blocks.insert(
            entry.clone(),
            BasicBlock {
                id: entry.clone(),
                statements: Vec::new(),
                successors: Vec::new(),
                terminates: false,
                terminator: None,
            },
        );

        ControlFlowGraph {
            blocks,
            entry,
            exit: None,
            next_id: 1,
            edge_conditions: HashMap::new(),
        }
    }
}

impl ControlFlowGraph {
    #[allow(dead_code)]
    fn new_block(&mut self) -> BlockId {
        let id = BlockId::new(self.next_id);
        self.next_id += 1;

        self.blocks.insert(
            id.clone(),
            BasicBlock {
                id: id.clone(),
                statements: Vec::new(),
                successors: Vec::new(),
                terminates: false,
                terminator: None,
            },
        );

        id
    }

    #[allow(dead_code)]
    fn add_edge(&mut self, from: BlockId, to: BlockId, edge_type: EdgeType) {
        if let Some(block) = self.blocks.get_mut(&from)
            && !block.successors.iter().any(|(id, _)| id == &to)
        {
            block.successors.push((to, edge_type));
        }
    }

    #[allow(dead_code)]
    fn add_statement(&mut self, block: BlockId, span: TextRange, kind: StmtKind) {
        if let Some(b) = self.blocks.get_mut(&block) {
            b.statements.push(StmtInfo { span, kind });

            // Mark block as terminating if statement is a terminator
            if matches!(
                kind,
                StmtKind::Return | StmtKind::Raise | StmtKind::Break | StmtKind::Continue
            ) {
                b.terminates = true;
                b.terminator = Some(match kind {
                    StmtKind::Return => Terminator::Return,
                    StmtKind::Raise => Terminator::Raise,
                    StmtKind::Break => Terminator::Break,
                    StmtKind::Continue => Terminator::Continue,
                    _ => unreachable!(),
                });
            }
        }
    }

    /// Find all reachable blocks using BFS
    #[allow(dead_code)]
    fn reachable_blocks(&self) -> HashSet<BlockId> {
        let mut reachable = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(self.entry.clone());
        reachable.insert(self.entry.clone());

        while let Some(block_id) = queue.pop_front() {
            if let Some(block) = self.blocks.get(&block_id) {
                for (successor, _) in &block.successors {
                    if reachable.insert(successor.clone()) {
                        queue.push_back(successor.clone());
                    }
                }
            }
        }

        reachable
    }

    /// Add a condition to an edge (for type narrowing)
    pub fn set_edge_condition(&mut self, from: BlockId, to: BlockId, condition: ConditionInfo) {
        self.edge_conditions.insert((from, to), condition);
    }

    /// Get the condition associated with an edge
    pub fn get_edge_condition(&self, from: &BlockId, to: &BlockId) -> Option<&ConditionInfo> {
        self.edge_conditions.get(&(from.clone(), to.clone()))
    }

    /// Get all predecessors of a block
    pub fn get_predecessors(&self, block: &BlockId) -> Vec<BlockId> {
        let mut predecessors = Vec::new();
        for (id, b) in &self.blocks {
            if b.successors.iter().any(|(succ, _)| succ == block) {
                predecessors.push(id.clone());
            }
        }
        predecessors
    }

    /// Get the entry block
    pub fn entry_block(&self) -> &BlockId {
        &self.entry
    }

    /// Get all blocks
    pub fn blocks(&self) -> impl Iterator<Item = &BlockId> {
        self.blocks.keys()
    }
}

/// Control flow analysis warnings
#[derive(Debug, Clone)]
pub enum ControlFlowWarning {
    /// Finally block may not execute in all cases
    FinallyNotGuaranteed { span: TextRange, reason: String },
}

/// Control flow analyzer
pub struct ControlFlowAnalyzer {
    errors: Vec<Error>,
    warnings: Vec<ControlFlowWarning>,
    /// Current function being analyzed
    current_function: Option<String>,
    /// Loop depth (for break/continue validation)
    loop_depth: usize,
}

impl Default for ControlFlowAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl ControlFlowAnalyzer {
    pub fn new() -> Self {
        ControlFlowAnalyzer {
            errors: Vec::new(),
            warnings: Vec::new(),
            current_function: None,
            loop_depth: 0,
        }
    }

    /// Get a more precise span for a statement (e.g., just the first keyword or identifier)
    /// This makes error messages more readable by not underlining entire blocks
    fn get_statement_keyword_span(&self, stmt: &Stmt) -> TextRange {
        let full_span = stmt.span();

        match stmt {
            // For simple statements, the span is already just the keyword
            Stmt::Pass(_) | Stmt::Break(_) | Stmt::Continue(_) => full_span,

            // For other statements, estimate the first keyword/identifier length
            Stmt::Return(_) => {
                // "return" is 6 characters
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Raise(_) => {
                // "raise" is 5 characters
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::If(_) => {
                // "if" is 2 characters
                let end = full_span.start() + TextSize::from(2u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::While(_) => {
                // "while" is 5 characters
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::For(_) => {
                // "for" is 3 characters
                let end = full_span.start() + TextSize::from(3u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Try(_) => {
                // "try" is 3 characters
                let end = full_span.start() + TextSize::from(3u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::With(_) => {
                // "with" is 4 characters
                let end = full_span.start() + TextSize::from(4u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Match(_) => {
                // "match" is 5 characters
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Assert(_) => {
                // "assert" is 6 characters
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Import(_) => {
                // "import" is 6 characters
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::From(_) => {
                // "from" is 4 characters
                let end = full_span.start() + TextSize::from(4u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Export(_) => {
                // "export" is 6 characters
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::FuncDef(func) => {
                // Function name span (skip "def ")
                let def_offset = TextSize::from(4u32);
                let name_len = TextSize::from(func.name.len() as u32);
                let name_start = full_span.start() + def_offset;
                TextRange::new(name_start, name_start + name_len)
            }
            Stmt::ClassDef(class) => {
                // Class name span (skip "class ")
                let class_offset = TextSize::from(6u32);
                let name_len = TextSize::from(class.name.len() as u32);
                let name_start = full_span.start() + class_offset;
                TextRange::new(name_start, name_start + name_len)
            }
            // For expression statements, try to get a more precise span from the expression
            Stmt::Expr(expr_stmt) => {
                match &expr_stmt.value {
                    Expr::Call(call) => {
                        // Use the function/method name being called
                        call.func.span()
                    }
                    _ => full_span,
                }
            }
            // For assignments, use the target span
            Stmt::Assign(assign) => {
                if let Some(first_target) = assign.targets.first() {
                    first_target.span()
                } else {
                    full_span
                }
            }
            Stmt::AnnAssign(ann_assign) => ann_assign.target.span(),
            Stmt::AugAssign(aug_assign) => aug_assign.target.span(),
            // For other statements, use the full span
            _ => full_span,
        }
    }

    /// Analyze a module for control flow issues
    pub fn analyze_module(&mut self, module: &Module) {
        for stmt in module.body {
            self.analyze_stmt(stmt);
        }
    }

    /// Analyze a statement
    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDef(func) => {
                self.analyze_function(func);
            }
            Stmt::If(if_stmt) => {
                self.analyze_if(if_stmt);
            }
            Stmt::While(while_stmt) => {
                self.analyze_while(while_stmt);
            }
            Stmt::For(for_stmt) => {
                self.analyze_for(for_stmt);
            }
            Stmt::Try(try_stmt) => {
                self.analyze_try(try_stmt);
            }
            Stmt::Break(span) => {
                if self.loop_depth == 0 {
                    self.errors.push(*error(ErrorKind::BreakOutsideLoop, *span));
                }
            }
            Stmt::Continue(span) => {
                if self.loop_depth == 0 {
                    self.errors
                        .push(*error(ErrorKind::ContinueOutsideLoop, *span));
                }
            }
            Stmt::ClassDef(class) => {
                // Analyze class methods
                for stmt in class.body {
                    self.analyze_stmt(stmt);
                }
            }
            _ => {
                // Other statements don't need special control flow analysis
            }
        }
    }

    /// Analyze a function for control flow issues
    fn analyze_function(&mut self, func: &crate::ast::nodes::FuncDefStmt) {
        let prev_function = self.current_function.clone();
        self.current_function = Some(func.name.to_string());

        // Check for unreachable code in function body
        self.check_unreachable_in_block(func.body);

        // Check if all paths return (if function has non-None return type)
        if let Some(ref returns) = func.returns
            && !self.is_none_type(returns)
        {
            let returns_on_all_paths = self.check_all_paths_return(func.body);
            if !returns_on_all_paths {
                // Use just the function name span - need to skip "def " (4 chars) to get to the name
                let def_offset = TextSize::from(4u32); // "def " is 4 characters
                let name_len = TextSize::from(func.name.len() as u32);
                let name_start = func.span.start() + def_offset;
                let signature_span = TextRange::new(name_start, name_start + name_len);

                self.errors.push(*error(
                    ErrorKind::MissingReturn {
                        function: func.name.to_string(),
                        expected_type: "int".to_string(), // TODO: get actual return type
                    },
                    signature_span,
                ));
            }
        }

        self.current_function = prev_function;
    }

    /// Check if an expression represents the None type
    fn is_none_type(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::Name(name) if name.id == "None")
    }

    /// Check for unreachable code in a block of statements
    fn check_unreachable_in_block(&mut self, stmts: &[Stmt]) {
        let mut found_terminator = false;
        let mut terminator_reason = String::new();

        for stmt in stmts {
            if found_terminator {
                // Everything after a terminator is unreachable
                // Use the more precise keyword span for better error display
                self.errors.push(*error(
                    ErrorKind::UnreachableCode {
                        reason: terminator_reason.clone(),
                    },
                    self.get_statement_keyword_span(stmt),
                ));
                continue;
            }

            // Check if this statement terminates
            match stmt {
                Stmt::Return(_) => {
                    found_terminator = true;
                    terminator_reason = "return statement".to_string();
                }
                Stmt::Raise(_) => {
                    found_terminator = true;
                    terminator_reason = "raise statement".to_string();
                }
                Stmt::Break(_) if self.loop_depth > 0 => {
                    found_terminator = true;
                    terminator_reason = "break statement".to_string();
                }
                Stmt::Continue(_) if self.loop_depth > 0 => {
                    found_terminator = true;
                    terminator_reason = "continue statement".to_string();
                }
                _ => {}
            }

            // Recursively check nested blocks
            self.analyze_stmt(stmt);
        }
    }

    /// Check if all paths through a block return a value
    fn check_all_paths_return(&mut self, stmts: &[Stmt]) -> bool {
        if stmts.is_empty() {
            return false;
        }

        // Check if any statement is a return
        for stmt in stmts {
            match stmt {
                Stmt::Return(_) => return true,
                Stmt::If(if_stmt) => {
                    // If has both branches, check both
                    let then_returns = self.check_all_paths_return(if_stmt.body);
                    let else_returns = if !if_stmt.orelse.is_empty() {
                        self.check_all_paths_return(if_stmt.orelse)
                    } else {
                        false
                    };

                    // Both branches must return
                    if then_returns && else_returns {
                        return true;
                    }
                }
                Stmt::Try(try_stmt) => {
                    // Check if try and all handlers return
                    let try_returns = self.check_all_paths_return(try_stmt.body);
                    let all_handlers_return = try_stmt
                        .handlers
                        .iter()
                        .all(|h| self.check_all_paths_return(h.body));

                    if try_returns && all_handlers_return && !try_stmt.handlers.is_empty() {
                        return true;
                    }
                }
                _ => {}
            }
        }

        false
    }

    /// Analyze an if statement
    fn analyze_if(&mut self, if_stmt: &crate::ast::nodes::IfStmt) {
        self.check_unreachable_in_block(if_stmt.body);
        if !if_stmt.orelse.is_empty() {
            self.check_unreachable_in_block(if_stmt.orelse);
        }
    }

    /// Analyze a while loop
    fn analyze_while(&mut self, while_stmt: &crate::ast::nodes::WhileStmt) {
        self.loop_depth += 1;
        self.check_unreachable_in_block(while_stmt.body);
        if !while_stmt.orelse.is_empty() {
            self.check_unreachable_in_block(while_stmt.orelse);
        }
        self.loop_depth -= 1;
    }

    /// Analyze a for loop
    fn analyze_for(&mut self, for_stmt: &crate::ast::nodes::ForStmt) {
        self.loop_depth += 1;
        self.check_unreachable_in_block(for_stmt.body);
        if !for_stmt.orelse.is_empty() {
            self.check_unreachable_in_block(for_stmt.orelse);
        }
        self.loop_depth -= 1;
    }

    /// Analyze a try/except/finally statement
    fn analyze_try(&mut self, try_stmt: &crate::ast::nodes::TryStmt) {
        // Check try body
        self.check_unreachable_in_block(try_stmt.body);

        // Check exception handlers
        let mut caught_exceptions = HashSet::new();
        for handler in try_stmt.handlers {
            // Check if this exception type was already caught
            if let Some(ref exc_type) = handler.typ
                && let Expr::Name(name) = exc_type
                && !caught_exceptions.insert(name.id.to_string())
            {
                // Use the exception type span for more precise error location
                self.errors.push(*error(
                    ErrorKind::UnreachableExceptionHandler {
                        exception_type: name.id.to_string(),
                    },
                    exc_type.span(),
                ));
            }

            self.check_unreachable_in_block(handler.body);
        }

        // Check else clause
        if !try_stmt.orelse.is_empty() {
            self.check_unreachable_in_block(try_stmt.orelse);
        }

        // Check finally clause
        if !try_stmt.finalbody.is_empty() {
            self.check_unreachable_in_block(try_stmt.finalbody);
        }
    }

    /// Get all warnings
    pub fn warnings(&self) -> &[ControlFlowWarning] {
        &self.warnings
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Check if there are any warnings
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    /// Get the errors collected during analysis
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }
}

impl ControlFlowWarning {
    /// Convert to a diagnostic for display
    pub fn to_diagnostic(&self, source: &str) -> Diagnostic {
        match self {
            ControlFlowWarning::FinallyNotGuaranteed { span, reason } => Diagnostic::from_source(
                crate::error::codes::Severity::Warning,
                format!("Finally block may not execute: {}", reason),
                source.to_string(),
                *span,
            )
            .with_error_type("ControlFlowWarning".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cfg_creation() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let block2 = cfg.new_block();

        cfg.add_edge(cfg.entry.clone(), block1.clone(), EdgeType::Normal);
        cfg.add_edge(block1.clone(), block2.clone(), EdgeType::Normal);

        assert_eq!(cfg.blocks.len(), 3); // entry + block1 + block2
    }

    #[test]
    fn test_reachable_blocks() {
        let mut cfg = ControlFlowGraph::new();
        let block1 = cfg.new_block();
        let block2 = cfg.new_block();
        let _unreachable = cfg.new_block();

        cfg.add_edge(cfg.entry.clone(), block1.clone(), EdgeType::Normal);
        cfg.add_edge(block1.clone(), block2.clone(), EdgeType::Normal);

        let reachable = cfg.reachable_blocks();
        assert_eq!(reachable.len(), 3); // entry + block1 + block2
    }

    #[test]
    fn test_analyzer_creation() {
        let analyzer = ControlFlowAnalyzer::new();
        assert!(!analyzer.has_errors());
        assert!(!analyzer.has_warnings());
    }
}
