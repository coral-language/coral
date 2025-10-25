#![allow(clippy::only_used_in_recursion)]
#![allow(clippy::needless_borrow)]
#![allow(clippy::collapsible_if)]
#![allow(clippy::borrow_deref_ref)]
#![allow(clippy::if_same_then_else)]

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
pub struct BasicBlock {
    /// Unique identifier
    pub id: BlockId,
    /// Statements in this block
    pub statements: Vec<StmtInfo>,
    /// Successor blocks
    pub successors: Vec<(BlockId, EdgeType)>,
    /// Whether this block terminates (return, raise, break, continue)
    pub terminates: bool,
    /// Type of termination (if any)
    pub terminator: Option<Terminator>,
    /// All variables defined in this block (union of all statement defs)
    pub defs: HashSet<String>,
    /// All variables used in this block (union of all statement uses)
    pub uses: HashSet<String>,
    /// Variables defined with constant values in this block
    pub constant_defs: HashSet<String>,
}

/// Statement information for CFG
#[derive(Debug, Clone)]
pub struct StmtInfo {
    pub span: TextRange,
    pub kind: StmtKind,
    /// Variables defined (written) by this statement
    pub defs: Vec<String>,
    /// Variables used (read) by this statement
    pub uses: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StmtKind {
    Normal,
    Return,
    Raise,
    Break,
    Continue,
    Pass,
}

/// How a block terminates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Terminator {
    Return,
    Raise,
    Break,
    Continue,
    /// Implicit (falls through to next block)
    Fallthrough,
}

/// Control flow graph
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    /// All basic blocks
    pub blocks: HashMap<BlockId, BasicBlock>,
    /// Entry block (function start)
    pub entry: BlockId,
    /// Exit block (function end)
    pub exit: Option<BlockId>,
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
                defs: HashSet::new(),
                uses: HashSet::new(),
                constant_defs: HashSet::new(),
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
                defs: HashSet::new(),
                uses: HashSet::new(),
                constant_defs: HashSet::new(),
            },
        );

        id
    }

    fn add_edge(&mut self, from: BlockId, to: BlockId, edge_type: EdgeType) {
        if let Some(block) = self.blocks.get_mut(&from)
            && !block.successors.iter().any(|(id, _)| id == &to)
        {
            block.successors.push((to, edge_type));
        }
    }

    fn add_statement(
        &mut self,
        block: BlockId,
        span: TextRange,
        kind: StmtKind,
        defs: Vec<String>,
        uses: Vec<String>,
    ) {
        self.add_statement_with_constants(block, span, kind, defs, uses, Vec::new())
    }

    fn add_statement_with_constants(
        &mut self,
        block: BlockId,
        span: TextRange,
        kind: StmtKind,
        defs: Vec<String>,
        uses: Vec<String>,
        constant_defs: Vec<String>,
    ) {
        if let Some(b) = self.blocks.get_mut(&block) {
            for def in &defs {
                b.defs.insert(def.clone());
            }
            for use_var in &uses {
                b.uses.insert(use_var.clone());
            }
            for const_def in &constant_defs {
                b.constant_defs.insert(const_def.clone());
            }

            b.statements.push(StmtInfo {
                span,
                kind,
                defs,
                uses,
            });

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

/// Context for loop constructs (for break/continue handling)
#[derive(Debug, Clone)]
struct LoopContext {
    /// Block to jump to on break
    break_target: BlockId,
    /// Block to jump to on continue
    continue_target: BlockId,
}

/// Context for try constructs (for exception handling)
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct TryContext {
    /// Exception handler blocks
    handler_blocks: Vec<BlockId>,
    /// Finally block (if present)
    finally_block: Option<BlockId>,
}

/// Control flow analyzer
pub struct ControlFlowAnalyzer {
    errors: Vec<Error>,
    warnings: Vec<ControlFlowWarning>,
    /// Current function being analyzed
    current_function: Option<String>,
    /// Loop depth (for break/continue validation)
    loop_depth: usize,
    /// Stack of loop contexts for break/continue resolution
    loop_stack: Vec<LoopContext>,
    /// Stack of try contexts for exception flow
    try_stack: Vec<TryContext>,
    /// CFGs stored per function name for dataflow analysis
    pub function_cfgs: HashMap<String, ControlFlowGraph>,
    /// Track if we're currently analyzing a protocol
    in_protocol: bool,
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
            loop_stack: Vec::new(),
            try_stack: Vec::new(),
            function_cfgs: HashMap::new(),
            in_protocol: false,
        }
    }

    /// Get the CFG for a specific function
    pub fn get_function_cfg(&self, function_name: &str) -> Option<&ControlFlowGraph> {
        self.function_cfgs.get(function_name)
    }

    /// Extract variables defined and used by a statement
    fn extract_defs_uses(&self, stmt: &Stmt) -> (Vec<String>, Vec<String>) {
        match stmt {
            Stmt::Assign(assign) => {
                let defs = self.extract_targets(&assign.targets);
                let uses = self.extract_expr_vars(&assign.value);
                (defs, uses)
            }
            Stmt::AnnAssign(ann_assign) => {
                let defs = self.extract_expr_vars_as_names(&ann_assign.target);
                let mut uses = self.extract_expr_vars(&ann_assign.annotation);
                if let Some(ref value) = ann_assign.value {
                    uses.extend(self.extract_expr_vars(value));
                }
                (defs, uses)
            }
            Stmt::AugAssign(aug_assign) => {
                let target_vars = self.extract_expr_vars_as_names(&aug_assign.target);
                let mut uses = target_vars.clone(); // Augmented assignment reads the target
                uses.extend(self.extract_expr_vars(&aug_assign.value));
                (target_vars, uses)
            }
            Stmt::For(for_stmt) => {
                let defs = self.extract_expr_vars_as_names(&for_stmt.target);
                let uses = self.extract_expr_vars(&for_stmt.iter);
                (defs, uses)
            }
            Stmt::Return(ret) => {
                let uses = if let Some(ref value) = ret.value {
                    self.extract_expr_vars(value)
                } else {
                    Vec::new()
                };
                (Vec::new(), uses)
            }
            Stmt::Raise(raise) => {
                let mut uses = Vec::new();
                if let Some(ref exc) = raise.exc {
                    uses.extend(self.extract_expr_vars(exc));
                }
                if let Some(ref cause) = raise.cause {
                    uses.extend(self.extract_expr_vars(cause));
                }
                (Vec::new(), uses)
            }
            Stmt::Assert(assert_stmt) => {
                let mut uses = self.extract_expr_vars(&assert_stmt.test);
                if let Some(ref msg) = assert_stmt.msg {
                    uses.extend(self.extract_expr_vars(msg));
                }
                (Vec::new(), uses)
            }
            Stmt::Expr(expr_stmt) => {
                let uses = self.extract_expr_vars(&expr_stmt.value);
                (Vec::new(), uses)
            }
            Stmt::Delete(delete) => {
                let uses = delete
                    .targets
                    .iter()
                    .flat_map(|t| self.extract_expr_vars(t))
                    .collect();
                (Vec::new(), uses)
            }
            Stmt::Global(global) => (
                global.names.iter().map(|n| n.to_string()).collect(),
                Vec::new(),
            ),
            Stmt::Nonlocal(nonlocal) => (
                nonlocal.names.iter().map(|n| n.to_string()).collect(),
                Vec::new(),
            ),
            Stmt::FuncDef(func) => {
                let defs = vec![func.name.to_string()];
                let uses = func
                    .decorators
                    .iter()
                    .flat_map(|dec| self.extract_expr_vars(dec))
                    .collect();
                (defs, uses)
            }
            Stmt::ClassDef(class) => {
                let defs = vec![class.name.to_string()];
                let mut uses: Vec<String> = Vec::new();

                for base in class.bases {
                    uses.extend(self.extract_expr_vars(base));
                }

                for decorator in class.decorators {
                    uses.extend(self.extract_expr_vars(decorator));
                }

                (defs, uses)
            }

            _ => (Vec::new(), Vec::new()),
        }
    }

    /// Extract variable names from assignment targets
    fn extract_targets(&self, targets: &[Expr]) -> Vec<String> {
        targets
            .iter()
            .flat_map(|t| self.extract_expr_vars_as_names(t))
            .collect()
    }

    /// Extract variable names from an expression (as lvalues)
    fn extract_expr_vars_as_names(&self, expr: &Expr) -> Vec<String> {
        match expr {
            Expr::Name(name) => vec![name.id.to_string()],
            Expr::Tuple(tuple) => tuple
                .elts
                .iter()
                .flat_map(|e| self.extract_expr_vars_as_names(e))
                .collect(),
            Expr::List(list) => list
                .elts
                .iter()
                .flat_map(|e| self.extract_expr_vars_as_names(e))
                .collect(),
            Expr::Subscript(sub) => self.extract_expr_vars_as_names(&sub.value),
            Expr::Attribute(attr) => self.extract_expr_vars_as_names(&attr.value),
            _ => Vec::new(),
        }
    }

    /// Extract variables used (read) in an expression
    fn extract_expr_vars(&self, expr: &Expr) -> Vec<String> {
        match expr {
            Expr::Name(name) => vec![name.id.to_string()],
            Expr::BinOp(binop) => {
                let mut vars = self.extract_expr_vars(&binop.left);
                vars.extend(self.extract_expr_vars(&binop.right));
                vars
            }
            Expr::UnaryOp(unary) => self.extract_expr_vars(&unary.operand),
            Expr::Lambda(lambda) => self.extract_expr_vars(&lambda.body),
            Expr::IfExp(if_exp) => {
                let mut vars = self.extract_expr_vars(&if_exp.test);
                vars.extend(self.extract_expr_vars(&if_exp.body));
                vars.extend(self.extract_expr_vars(&if_exp.orelse));
                vars
            }
            Expr::Dict(dict) => {
                let mut vars = Vec::new();
                for key in dict.keys.iter().flatten() {
                    vars.extend(self.extract_expr_vars(key));
                }
                for value in dict.values {
                    vars.extend(self.extract_expr_vars(value));
                }
                vars
            }
            Expr::Set(set) => set
                .elts
                .iter()
                .flat_map(|e| self.extract_expr_vars(e))
                .collect(),
            Expr::ListComp(comp) => {
                let mut vars = self.extract_expr_vars(&comp.elt);
                for generator in comp.generators {
                    vars.extend(self.extract_expr_vars(&generator.iter));
                    for if_clause in generator.ifs {
                        vars.extend(self.extract_expr_vars(if_clause));
                    }
                }
                vars
            }
            Expr::SetComp(comp) => {
                let mut vars = self.extract_expr_vars(&comp.elt);
                for generator in comp.generators {
                    vars.extend(self.extract_expr_vars(&generator.iter));
                    for if_clause in generator.ifs {
                        vars.extend(self.extract_expr_vars(if_clause));
                    }
                }
                vars
            }
            Expr::DictComp(comp) => {
                let mut vars = self.extract_expr_vars(&comp.key);
                vars.extend(self.extract_expr_vars(&comp.value));
                for generator in comp.generators {
                    vars.extend(self.extract_expr_vars(&generator.iter));
                    for if_clause in generator.ifs {
                        vars.extend(self.extract_expr_vars(if_clause));
                    }
                }
                vars
            }
            Expr::GeneratorExp(genexp) => {
                let mut vars = self.extract_expr_vars(&genexp.elt);
                for generator in genexp.generators {
                    vars.extend(self.extract_expr_vars(&generator.iter));
                    for if_clause in generator.ifs {
                        vars.extend(self.extract_expr_vars(if_clause));
                    }
                }
                vars
            }
            Expr::Await(await_expr) => self.extract_expr_vars(&await_expr.value),
            Expr::Yield(yield_expr) => {
                if let Some(ref value) = yield_expr.value {
                    self.extract_expr_vars(value)
                } else {
                    Vec::new()
                }
            }
            Expr::YieldFrom(yield_from) => self.extract_expr_vars(&yield_from.value),
            Expr::Compare(compare) => {
                let mut vars = self.extract_expr_vars(&compare.left);
                for comparator in compare.comparators {
                    vars.extend(self.extract_expr_vars(comparator));
                }
                vars
            }
            Expr::Call(call) => {
                let mut vars = self.extract_expr_vars(&call.func);
                for arg in call.args {
                    vars.extend(self.extract_expr_vars(arg));
                }
                for keyword in call.keywords {
                    vars.extend(self.extract_expr_vars(&keyword.value));
                }
                vars
            }
            Expr::FormattedValue(fval) => self.extract_expr_vars(&fval.value),
            Expr::JoinedStr(joined) => joined
                .values
                .iter()
                .flat_map(|v| self.extract_expr_vars(v))
                .collect(),
            Expr::Attribute(attr) => self.extract_expr_vars(&attr.value),
            Expr::Subscript(sub) => {
                let mut vars = self.extract_expr_vars(&sub.value);
                vars.extend(self.extract_expr_vars(&sub.slice));
                vars
            }
            Expr::Starred(starred) => self.extract_expr_vars(&starred.value),
            Expr::List(list) => list
                .elts
                .iter()
                .flat_map(|e| self.extract_expr_vars(e))
                .collect(),
            Expr::Tuple(tuple) => tuple
                .elts
                .iter()
                .flat_map(|e| self.extract_expr_vars(e))
                .collect(),
            Expr::Slice(slice) => {
                let mut vars = Vec::new();
                if let Some(ref lower) = slice.lower {
                    vars.extend(self.extract_expr_vars(lower));
                }
                if let Some(ref upper) = slice.upper {
                    vars.extend(self.extract_expr_vars(upper));
                }
                if let Some(ref step) = slice.step {
                    vars.extend(self.extract_expr_vars(step));
                }
                vars
            }

            Expr::Constant(_) | Expr::TString(_) | Expr::Complex(_) | Expr::Bytes(_) => Vec::new(),

            Expr::NamedExpr(named) => self.extract_expr_vars(&named.value),

            Expr::BoolOp(boolop) => boolop
                .values
                .iter()
                .flat_map(|v| self.extract_expr_vars(v))
                .collect(),

            Expr::ModuleIntrospection(_) => Vec::new(),
        }
    }

    /// Extract condition information from an expression for type narrowing
    fn extract_condition_info(&self, expr: &Expr, var_name: &str) -> Option<ConditionInfo> {
        match expr {
            Expr::Call(call) => {
                if let Expr::Name(func_name) = &*call.func {
                    if func_name.id == "isinstance" && call.args.len() >= 2 {
                        if let Expr::Name(arg_name) = &call.args[0] {
                            if arg_name.id == var_name {
                                let type_name = if let Expr::Name(type_name) = &call.args[1] {
                                    type_name.id.to_string()
                                } else {
                                    "Unknown".to_string()
                                };
                                return Some(ConditionInfo {
                                    variable: var_name.to_string(),
                                    kind: ConditionKind::IsInstance {
                                        check_type: type_name,
                                    },
                                });
                            }
                        }
                    }
                }
                None
            }

            Expr::Compare(compare) => {
                if let Expr::Name(left_name) = &*compare.left {
                    if left_name.id == var_name {
                        if compare.ops.len() == 1 {
                            if let Some(Expr::Constant(c)) = compare.comparators.first() {
                                if format!("{:?}", c.value).contains("None") {
                                    return Some(ConditionInfo {
                                        variable: var_name.to_string(),
                                        kind: if compare.ops[0] == "is" {
                                            ConditionKind::IsNone
                                        } else if compare.ops[0] == "is not" {
                                            ConditionKind::IsNotNone
                                        } else {
                                            return None;
                                        },
                                    });
                                }
                            }
                        }
                    }
                }
                None
            }

            Expr::UnaryOp(unary) => {
                if unary.op == "not" {
                    if let Expr::Name(name) = &*unary.operand {
                        if name.id == var_name {
                            return Some(ConditionInfo {
                                variable: var_name.to_string(),
                                kind: ConditionKind::Falsy,
                            });
                        }
                    }
                }
                None
            }

            Expr::Name(name) => {
                if name.id == var_name {
                    Some(ConditionInfo {
                        variable: var_name.to_string(),
                        kind: ConditionKind::Truthy,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get a more precise span for a statement (e.g., just the first keyword or identifier)
    /// This makes error messages more readable by not underlining entire blocks
    fn get_statement_keyword_span(&self, stmt: &Stmt) -> TextRange {
        let full_span = stmt.span();

        match stmt {
            Stmt::Pass(_) | Stmt::Break(_) | Stmt::Continue(_) => full_span,

            Stmt::Return(_) => {
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Raise(_) => {
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::If(_) => {
                let end = full_span.start() + TextSize::from(2u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::While(_) => {
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::For(_) => {
                let end = full_span.start() + TextSize::from(3u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Try(_) => {
                let end = full_span.start() + TextSize::from(3u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::With(_) => {
                let end = full_span.start() + TextSize::from(4u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Match(_) => {
                let end = full_span.start() + TextSize::from(5u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Assert(_) => {
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Import(_) => {
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::From(_) => {
                let end = full_span.start() + TextSize::from(4u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::Export(_) => {
                let end = full_span.start() + TextSize::from(6u32);
                TextRange::new(full_span.start(), end.min(full_span.end()))
            }
            Stmt::FuncDef(func) => {
                let def_offset = TextSize::from(4u32);
                let name_len = TextSize::from(func.name.len() as u32);
                let name_start = full_span.start() + def_offset;
                TextRange::new(name_start, name_start + name_len)
            }
            Stmt::ClassDef(class) => {
                let class_offset = TextSize::from(6u32);
                let name_len = TextSize::from(class.name.len() as u32);
                let name_start = full_span.start() + class_offset;
                TextRange::new(name_start, name_start + name_len)
            }

            Stmt::Expr(expr_stmt) => match &expr_stmt.value {
                Expr::Call(call) => call.func.span(),
                _ => full_span,
            },

            Stmt::Assign(assign) => {
                if let Some(first_target) = assign.targets.first() {
                    first_target.span()
                } else {
                    full_span
                }
            }
            Stmt::AnnAssign(ann_assign) => ann_assign.target.span(),
            Stmt::AugAssign(aug_assign) => aug_assign.target.span(),

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
                let prev_protocol = self.in_protocol;
                self.in_protocol = class.is_protocol;

                for stmt in class.body {
                    self.analyze_stmt(stmt);
                }
                self.in_protocol = prev_protocol;
            }
            _ => {}
        }
    }

    /// Analyze a function for control flow issues
    fn analyze_function(&mut self, func: &crate::ast::nodes::FuncDefStmt) {
        let prev_function = self.current_function.clone();
        let function_name = func.name.to_string();
        self.current_function = Some(function_name.clone());

        let mut cfg = ControlFlowGraph::new();
        let entry = cfg.entry.clone();

        let _exit = self.build_cfg_from_stmts(func.body, &mut cfg, entry);

        self.check_unreachable_code_cfg(&cfg);

        let is_empty_body =
            func.body.iter().all(|stmt| matches!(stmt, Stmt::Pass(_))) || func.body.is_empty();

        if !self.in_protocol
            && !is_empty_body
            && let Some(ref returns) = func.returns
            && !self.is_none_type(returns)
            && !self.check_all_paths_return_cfg(&cfg)
        {
            let def_offset = TextSize::from(4u32);
            let name_len = TextSize::from(func.name.len() as u32);
            let name_start = func.span.start() + def_offset;
            let signature_span = TextRange::new(name_start, name_start + name_len);

            let expected_type = self.extract_type_name(returns);

            self.errors.push(*error(
                ErrorKind::MissingReturn {
                    function: func.name.to_string(),
                    expected_type,
                },
                signature_span,
            ));
        }

        self.function_cfgs.insert(function_name, cfg);

        self.current_function = prev_function;
    }

    /// Check if an expression represents the None type
    fn is_none_type(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::Name(name) if name.id == "None")
    }

    /// Extract a human-readable type name from a type annotation expression
    fn extract_type_name(&self, expr: &Expr) -> String {
        match expr {
            Expr::Name(name) => name.id.to_string(),
            Expr::Attribute(attr) => {
                format!("{}.{}", self.extract_type_name(attr.value), attr.attr)
            }
            Expr::Subscript(sub) => {
                let base = self.extract_type_name(sub.value);
                let param = self.extract_type_name(sub.slice);
                format!("{}[{}]", base, param)
            }
            Expr::Tuple(tup) => {
                let types: Vec<String> =
                    tup.elts.iter().map(|e| self.extract_type_name(e)).collect();
                format!("({})", types.join(", "))
            }
            Expr::BinOp(binop) if binop.op == "|" => {
                format!(
                    "{} | {}",
                    self.extract_type_name(binop.left),
                    self.extract_type_name(binop.right)
                )
            }
            Expr::Constant(c) => c.value.to_string(),
            _ => "unknown".to_string(),
        }
    }

    /// Check if an expression is a compile-time constant
    /// This is used for constant propagation analysis
    fn is_constant_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Constant(_) | Expr::Complex(_) | Expr::Bytes(_) => true,

            Expr::Name(name) if matches!(name.id, "True" | "False" | "None") => true,

            Expr::Tuple(tup) => tup.elts.iter().all(|e| self.is_constant_expr(e)),
            Expr::List(list) => list.elts.iter().all(|e| self.is_constant_expr(e)),
            Expr::Set(set) => set.elts.iter().all(|e| self.is_constant_expr(e)),

            Expr::Dict(dict) => {
                dict.keys.iter().all(|k| {
                    k.as_ref()
                        .map(|e| self.is_constant_expr(e))
                        .unwrap_or(false)
                }) && dict.values.iter().all(|v| self.is_constant_expr(v))
            }

            Expr::BinOp(binop) => {
                self.is_constant_expr(binop.left) && self.is_constant_expr(binop.right)
            }

            Expr::UnaryOp(unop) => self.is_constant_expr(unop.operand),
            _ => false,
        }
    }

    /// Build CFG from a sequence of statements
    /// Returns the exit block (last block that doesn't terminate)
    fn build_cfg_from_stmts(
        &mut self,
        stmts: &[Stmt],
        cfg: &mut ControlFlowGraph,
        mut current: BlockId,
    ) -> BlockId {
        for stmt in stmts {
            if let Some(block) = cfg.blocks.get(&current)
                && block.terminates
            {
                break;
            }

            current = self.build_cfg_from_stmt(stmt, cfg, current);
        }
        current
    }

    /// Build CFG from a single statement
    /// Returns the next block to continue from
    fn build_cfg_from_stmt(
        &mut self,
        stmt: &Stmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let (defs, uses) = self.extract_defs_uses(stmt);

        match stmt {
            Stmt::Assign(assign) => {
                let constant_defs = if self.is_constant_expr(&assign.value) {
                    defs.clone()
                } else {
                    Vec::new()
                };
                cfg.add_statement_with_constants(
                    current.clone(),
                    stmt.span(),
                    StmtKind::Normal,
                    defs,
                    uses,
                    constant_defs,
                );
                current
            }
            Stmt::AnnAssign(ann_assign) => {
                let constant_defs = if let Some(ref value) = ann_assign.value
                    && self.is_constant_expr(value)
                {
                    defs.clone()
                } else {
                    Vec::new()
                };
                cfg.add_statement_with_constants(
                    current.clone(),
                    stmt.span(),
                    StmtKind::Normal,
                    defs,
                    uses,
                    constant_defs,
                );
                current
            }

            Stmt::Expr(_)
            | Stmt::AugAssign(_)
            | Stmt::Pass(_)
            | Stmt::Import(_)
            | Stmt::From(_)
            | Stmt::Export(_)
            | Stmt::Delete(_)
            | Stmt::Global(_)
            | Stmt::Nonlocal(_)
            | Stmt::Assert(_)
            | Stmt::TypeAlias(_)
            | Stmt::Yield(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Normal, defs, uses);
                current
            }

            Stmt::Return(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Return, defs, uses);
                current
            }

            Stmt::Raise(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Raise, defs, uses);
                current
            }

            Stmt::Break(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Break, defs, uses);

                if let Some(loop_ctx) = self.loop_stack.last() {
                    cfg.add_edge(
                        current.clone(),
                        loop_ctx.break_target.clone(),
                        EdgeType::Normal,
                    );
                }
                current
            }

            Stmt::Continue(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Continue, defs, uses);

                if let Some(loop_ctx) = self.loop_stack.last() {
                    cfg.add_edge(
                        current.clone(),
                        loop_ctx.continue_target.clone(),
                        EdgeType::Loop,
                    );
                }
                current
            }

            Stmt::If(if_stmt) => self.build_cfg_if(if_stmt, cfg, current),
            Stmt::While(while_stmt) => self.build_cfg_while(while_stmt, cfg, current),
            Stmt::For(for_stmt) => self.build_cfg_for(for_stmt, cfg, current),
            Stmt::Try(try_stmt) => self.build_cfg_try(try_stmt, cfg, current),
            Stmt::With(with_stmt) => self.build_cfg_with(with_stmt, cfg, current),
            Stmt::Match(match_stmt) => self.build_cfg_match(match_stmt, cfg, current),

            Stmt::FuncDef(_) | Stmt::ClassDef(_) => {
                cfg.add_statement(current.clone(), stmt.span(), StmtKind::Normal, defs, uses);
                current
            }
        }
    }

    /// Build CFG for if statement
    fn build_cfg_if(
        &mut self,
        if_stmt: &crate::ast::nodes::IfStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let then_block = cfg.new_block();
        let else_block = cfg.new_block();
        let merge_block = cfg.new_block();

        cfg.add_edge(current.clone(), then_block.clone(), EdgeType::True);
        cfg.add_edge(current.clone(), else_block.clone(), EdgeType::False);

        if let Some(var_name) = self.extract_simple_var_name(&if_stmt.test) {
            if let Some(condition_info) = self.extract_condition_info(&if_stmt.test, &var_name) {
                cfg.set_edge_condition(current.clone(), then_block.clone(), condition_info.clone());

                let negated_kind = match condition_info.kind {
                    ConditionKind::IsNone => Some(ConditionKind::IsNotNone),
                    ConditionKind::IsNotNone => Some(ConditionKind::IsNone),
                    ConditionKind::Truthy => Some(ConditionKind::Falsy),
                    ConditionKind::Falsy => Some(ConditionKind::Truthy),
                    _ => None,
                };
                if let Some(kind) = negated_kind {
                    cfg.set_edge_condition(
                        current,
                        else_block.clone(),
                        ConditionInfo {
                            variable: var_name,
                            kind,
                        },
                    );
                }
            }
        }

        let then_exit = self.build_cfg_from_stmts(if_stmt.body, cfg, then_block);

        if let Some(block) = cfg.blocks.get(&then_exit)
            && !block.terminates
        {
            cfg.add_edge(then_exit, merge_block.clone(), EdgeType::Normal);
        }

        let else_exit = if !if_stmt.orelse.is_empty() {
            self.build_cfg_from_stmts(if_stmt.orelse, cfg, else_block)
        } else {
            else_block
        };

        if let Some(block) = cfg.blocks.get(&else_exit)
            && !block.terminates
        {
            cfg.add_edge(else_exit, merge_block.clone(), EdgeType::Normal);
        }

        merge_block
    }

    /// Extract a simple variable name from an expression (for condition tracking)
    fn extract_simple_var_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Name(name) => Some(name.id.to_string()),
            Expr::Compare(compare) => {
                if let Expr::Name(name) = &*compare.left {
                    Some(name.id.to_string())
                } else {
                    None
                }
            }
            Expr::Call(call) => {
                if call.args.is_empty() {
                    return None;
                }
                if let Expr::Name(name) = &call.args.first()? {
                    Some(name.id.to_string())
                } else {
                    None
                }
            }
            Expr::UnaryOp(unary) => {
                if let Expr::Name(name) = &*unary.operand {
                    Some(name.id.to_string())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Build CFG for while loop
    fn build_cfg_while(
        &mut self,
        while_stmt: &crate::ast::nodes::WhileStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let header_block = cfg.new_block();
        let body_block = cfg.new_block();
        let exit_block = cfg.new_block();

        cfg.add_edge(current, header_block.clone(), EdgeType::Normal);

        cfg.add_edge(header_block.clone(), body_block.clone(), EdgeType::True);
        cfg.add_edge(header_block.clone(), exit_block.clone(), EdgeType::False);

        self.loop_stack.push(LoopContext {
            break_target: exit_block.clone(),
            continue_target: header_block.clone(),
        });
        self.loop_depth += 1;

        let body_exit = self.build_cfg_from_stmts(while_stmt.body, cfg, body_block);

        if let Some(block) = cfg.blocks.get(&body_exit)
            && !block.terminates
        {
            cfg.add_edge(body_exit, header_block, EdgeType::Loop);
        }

        self.loop_depth -= 1;
        self.loop_stack.pop();

        if !while_stmt.orelse.is_empty() {
            self.build_cfg_from_stmts(while_stmt.orelse, cfg, exit_block.clone())
        } else {
            exit_block
        }
    }

    /// Build CFG for for loop
    fn build_cfg_for(
        &mut self,
        for_stmt: &crate::ast::nodes::ForStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let header_block = cfg.new_block();
        let body_block = cfg.new_block();
        let exit_block = cfg.new_block();

        cfg.add_edge(current, header_block.clone(), EdgeType::Normal);

        cfg.add_edge(header_block.clone(), body_block.clone(), EdgeType::True);
        cfg.add_edge(header_block.clone(), exit_block.clone(), EdgeType::False);

        self.loop_stack.push(LoopContext {
            break_target: exit_block.clone(),
            continue_target: header_block.clone(),
        });
        self.loop_depth += 1;

        let body_exit = self.build_cfg_from_stmts(for_stmt.body, cfg, body_block);

        if let Some(block) = cfg.blocks.get(&body_exit)
            && !block.terminates
        {
            cfg.add_edge(body_exit, header_block, EdgeType::Loop);
        }

        self.loop_depth -= 1;
        self.loop_stack.pop();

        if !for_stmt.orelse.is_empty() {
            self.build_cfg_from_stmts(for_stmt.orelse, cfg, exit_block.clone())
        } else {
            exit_block
        }
    }

    /// Build CFG for try/except/finally
    fn build_cfg_try(
        &mut self,
        try_stmt: &crate::ast::nodes::TryStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let try_block = cfg.new_block();
        let mut handler_blocks = Vec::new();
        let finally_block = if !try_stmt.finalbody.is_empty() {
            Some(cfg.new_block())
        } else {
            None
        };
        let exit_block = cfg.new_block();

        cfg.add_edge(current, try_block.clone(), EdgeType::Normal);

        for _ in try_stmt.handlers {
            handler_blocks.push(cfg.new_block());
        }

        self.try_stack.push(TryContext {
            handler_blocks: handler_blocks.clone(),
            finally_block: finally_block.clone(),
        });

        let try_exit = self.build_cfg_from_stmts(try_stmt.body, cfg, try_block.clone());

        for handler_block in &handler_blocks {
            cfg.add_edge(
                try_block.clone(),
                handler_block.clone(),
                EdgeType::Exception,
            );
        }

        let mut handler_exits = Vec::new();
        for (i, handler) in try_stmt.handlers.iter().enumerate() {
            let handler_exit =
                self.build_cfg_from_stmts(handler.body, cfg, handler_blocks[i].clone());
            handler_exits.push(handler_exit);
        }

        let else_exit = if !try_stmt.orelse.is_empty() {
            self.build_cfg_from_stmts(try_stmt.orelse, cfg, try_exit.clone())
        } else {
            try_exit.clone()
        };

        self.try_stack.pop();

        if let Some(finally) = finally_block {
            if let Some(block) = cfg.blocks.get(&else_exit)
                && !block.terminates
            {
                cfg.add_edge(else_exit, finally.clone(), EdgeType::Finally);
            }

            for handler_exit in handler_exits {
                if let Some(block) = cfg.blocks.get(&handler_exit)
                    && !block.terminates
                {
                    cfg.add_edge(handler_exit, finally.clone(), EdgeType::Finally);
                }
            }

            let finally_exit = self.build_cfg_from_stmts(try_stmt.finalbody, cfg, finally);

            if let Some(block) = cfg.blocks.get(&finally_exit)
                && !block.terminates
            {
                cfg.add_edge(finally_exit, exit_block.clone(), EdgeType::Normal);
            }
        } else {
            if let Some(block) = cfg.blocks.get(&else_exit)
                && !block.terminates
            {
                cfg.add_edge(else_exit, exit_block.clone(), EdgeType::Normal);
            }

            for handler_exit in handler_exits {
                if let Some(block) = cfg.blocks.get(&handler_exit)
                    && !block.terminates
                {
                    cfg.add_edge(handler_exit, exit_block.clone(), EdgeType::Normal);
                }
            }
        }

        exit_block
    }

    /// Build CFG for with statement
    fn build_cfg_with(
        &mut self,
        with_stmt: &crate::ast::nodes::WithStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let body_block = cfg.new_block();
        let exit_block = cfg.new_block();

        cfg.add_edge(current, body_block.clone(), EdgeType::Normal);

        let body_exit = self.build_cfg_from_stmts(with_stmt.body, cfg, body_block);

        if let Some(block) = cfg.blocks.get(&body_exit)
            && !block.terminates
        {
            cfg.add_edge(body_exit, exit_block.clone(), EdgeType::Normal);
        }

        exit_block
    }

    /// Build CFG for match statement
    fn build_cfg_match(
        &mut self,
        match_stmt: &crate::ast::patterns::MatchStmt,
        cfg: &mut ControlFlowGraph,
        current: BlockId,
    ) -> BlockId {
        let merge_block = cfg.new_block();

        for case in match_stmt.cases {
            let case_block = cfg.new_block();
            cfg.add_edge(current.clone(), case_block.clone(), EdgeType::Normal);

            let case_exit = self.build_cfg_from_stmts(case.body, cfg, case_block);

            if let Some(block) = cfg.blocks.get(&case_exit)
                && !block.terminates
            {
                cfg.add_edge(case_exit, merge_block.clone(), EdgeType::Normal);
            }
        }

        merge_block
    }

    /// Check for unreachable code using CFG
    fn check_unreachable_code_cfg(&mut self, cfg: &ControlFlowGraph) {
        let reachable = cfg.reachable_blocks();

        for (block_id, block) in &cfg.blocks {
            if !reachable.contains(block_id) {
                for stmt_info in &block.statements {
                    self.errors.push(*error(
                        ErrorKind::UnreachableCode {
                            reason: "unreachable block".to_string(),
                        },
                        stmt_info.span,
                    ));
                }
            }
        }
    }

    /// Check if all paths return using CFG
    fn check_all_paths_return_cfg(&self, cfg: &ControlFlowGraph) -> bool {
        let mut has_return_on_all_paths = true;

        for (block_id, block) in &cfg.blocks {
            let reachable = cfg.reachable_blocks();
            if !reachable.contains(block_id) {
                continue;
            }

            if block.successors.is_empty() && block.terminator != Some(Terminator::Return) {
                let has_only_normal_stmts = block
                    .statements
                    .iter()
                    .all(|stmt| matches!(stmt.kind, StmtKind::Normal));

                if !has_only_normal_stmts {
                    has_return_on_all_paths = false;
                    break;
                }
            }
        }

        has_return_on_all_paths
    }

    /// Check for unreachable code in a block of statements
    #[allow(dead_code)]
    fn check_unreachable_in_block(&mut self, stmts: &[Stmt]) {
        let mut found_terminator = false;
        let mut terminator_reason = String::new();

        for stmt in stmts {
            if found_terminator {
                self.errors.push(*error(
                    ErrorKind::UnreachableCode {
                        reason: terminator_reason.clone(),
                    },
                    self.get_statement_keyword_span(stmt),
                ));
                continue;
            }

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

            self.analyze_stmt(stmt);
        }
    }

    /// Check if all paths through a block return a value
    #[allow(dead_code)]
    fn check_all_paths_return(&mut self, stmts: &[Stmt]) -> bool {
        if stmts.is_empty() {
            return false;
        }

        for stmt in stmts {
            match stmt {
                Stmt::Return(_) => return true,
                Stmt::If(if_stmt) => {
                    let then_returns = self.check_all_paths_return(if_stmt.body);
                    let else_returns = if !if_stmt.orelse.is_empty() {
                        self.check_all_paths_return(if_stmt.orelse)
                    } else {
                        false
                    };

                    if then_returns && else_returns {
                        return true;
                    }
                }
                Stmt::Try(try_stmt) => {
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
        self.check_unreachable_in_block(try_stmt.body);

        let mut caught_exceptions = HashSet::new();
        for handler in try_stmt.handlers {
            if let Some(ref exc_type) = handler.typ
                && let Expr::Name(name) = exc_type
                && !caught_exceptions.insert(name.id.to_string())
            {
                self.errors.push(*error(
                    ErrorKind::UnreachableExceptionHandler {
                        exception_type: name.id.to_string(),
                    },
                    exc_type.span(),
                ));
            }

            self.check_unreachable_in_block(handler.body);
        }

        if !try_stmt.orelse.is_empty() {
            self.check_unreachable_in_block(try_stmt.orelse);
        }

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

/// Trait for dataflow analyses
///
/// This trait provides a generic framework for implementing dataflow analyses
/// such as definite assignment analysis, constant propagation, etc.
pub trait DataflowAnalysis: Clone {
    /// The type of values computed by this analysis
    type Value: Clone + PartialEq + std::fmt::Debug;

    /// Get the initial value for the entry block
    fn initial_value(&self) -> Self::Value;

    /// Transfer function: compute output value given input value and block
    fn transfer(&self, block: &BasicBlock, input: Self::Value) -> Self::Value;

    /// Meet/Join operation: combine values from multiple predecessors
    fn meet(&self, values: Vec<Self::Value>) -> Self::Value;

    /// Direction of the analysis (true for forward, false for backward)
    fn is_forward(&self) -> bool {
        true
    }
}

/// Result of a dataflow analysis: IN and OUT sets for each block
#[derive(Debug, Clone)]
pub struct DataflowResult<V> {
    /// Input values for each block
    pub in_values: HashMap<BlockId, V>,
    /// Output values for each block
    pub out_values: HashMap<BlockId, V>,
}

/// Solve a dataflow problem using the worklist algorithm
///
/// This is a generic implementation that works for any dataflow analysis
/// that implements the DataflowAnalysis trait.
pub fn solve_dataflow<A: DataflowAnalysis>(
    cfg: &ControlFlowGraph,
    analysis: &A,
) -> DataflowResult<A::Value> {
    let mut in_values: HashMap<BlockId, A::Value> = HashMap::new();
    let mut out_values: HashMap<BlockId, A::Value> = HashMap::new();

    for block_id in cfg.blocks.keys() {
        in_values.insert(block_id.clone(), analysis.initial_value());
        out_values.insert(block_id.clone(), analysis.initial_value());
    }

    in_values.insert(cfg.entry.clone(), analysis.initial_value());

    let mut worklist: VecDeque<BlockId> = cfg.blocks.keys().cloned().collect();
    let mut iteration = 0;
    const MAX_ITERATIONS: usize = 1000; // Prevent infinite loops

    while let Some(block_id) = worklist.pop_front() {
        iteration += 1;
        if iteration > MAX_ITERATIONS {
            eprintln!("Warning: Dataflow analysis exceeded maximum iterations");
            break;
        }

        let block = cfg.blocks.get(&block_id).unwrap();

        let predecessors = cfg.get_predecessors(&block_id);
        let pred_values: Vec<A::Value> = predecessors
            .iter()
            .filter_map(|pred| out_values.get(pred).cloned())
            .collect();

        let new_in = if pred_values.is_empty() {
            if block_id == cfg.entry {
                analysis.initial_value()
            } else {
                analysis.initial_value()
            }
        } else {
            analysis.meet(pred_values)
        };

        let new_out = analysis.transfer(block, new_in.clone());

        let in_changed = in_values.get(&block_id) != Some(&new_in);
        let out_changed = out_values.get(&block_id) != Some(&new_out);

        if in_changed || out_changed {
            in_values.insert(block_id.clone(), new_in);
            out_values.insert(block_id.clone(), new_out);

            for (succ, _) in &block.successors {
                if !worklist.contains(succ) {
                    worklist.push_back(succ.clone());
                }
            }
        }
    }

    DataflowResult {
        in_values,
        out_values,
    }
}

/// Example: Definite Assignment Analysis
///
/// Tracks which variables are definitely assigned on all paths
#[derive(Clone)]
pub struct DefiniteAssignmentAnalysis;

impl DataflowAnalysis for DefiniteAssignmentAnalysis {
    type Value = HashSet<String>;

    fn initial_value(&self) -> Self::Value {
        HashSet::new()
    }

    fn transfer(&self, block: &BasicBlock, mut input: Self::Value) -> Self::Value {
        for def in &block.defs {
            input.insert(def.clone());
        }
        input
    }

    fn meet(&self, values: Vec<Self::Value>) -> Self::Value {
        if values.is_empty() {
            return HashSet::new();
        }

        let mut result = values[0].clone();
        for value in values.iter().skip(1) {
            result = result.intersection(value).cloned().collect();
        }
        result
    }

    fn is_forward(&self) -> bool {
        true
    }
}

/// Example: Available Expressions Analysis (for constant propagation)
///
/// This is a simplified version - real constant propagation would need
/// to track actual values, not just variable names
#[derive(Clone)]
pub struct AvailableExpressionsAnalysis;

impl DataflowAnalysis for AvailableExpressionsAnalysis {
    /// Set of variables with known values
    type Value = HashSet<String>;

    fn initial_value(&self) -> Self::Value {
        HashSet::new()
    }

    fn transfer(&self, block: &BasicBlock, mut input: Self::Value) -> Self::Value {
        for def in &block.defs {
            input.remove(def);
        }

        for const_def in &block.constant_defs {
            input.insert(const_def.clone());
        }

        input
    }

    fn meet(&self, values: Vec<Self::Value>) -> Self::Value {
        if values.is_empty() {
            return HashSet::new();
        }

        let mut result = values[0].clone();
        for value in values.iter().skip(1) {
            result = result.intersection(value).cloned().collect();
        }
        result
    }

    fn is_forward(&self) -> bool {
        true
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
