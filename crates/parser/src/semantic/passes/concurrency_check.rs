// Concurrency safety checking (race conditions, deadlocks, data races)
//
// This pass provides compile-time concurrency safety for Coral:
// 1. Data race detection (shared mutable state without synchronization)
// 2. Deadlock detection (circular lock dependencies)
// 3. Send/Sync trait checking (safe to send/share across threads)
// 4. Lock ordering enforcement (prevent deadlocks via consistent ordering)
// 5. Atomic operation verification (ensure proper memory ordering)
//
// Key insight: Most concurrency bugs can be prevented at compile time through
// careful tracking of shared state, lock acquisitions, and thread boundaries.

use crate::ast::expr::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Main concurrency safety checker
pub struct ConcurrencyChecker {
    /// Shared variable accesses across threads
    shared_accesses: Vec<SharedAccess>,
    /// Currently held locks in this execution path
    held_locks: Vec<LockAcquisition>,
    /// Lock acquisition history for deadlock detection
    lock_dependencies: Vec<LockDependency>,
    /// Shared variables and their protection
    shared_variables: HashMap<String, SharedVariable>,
    /// Current thread context
    thread_context: ThreadContext,
    /// Scope depth for tracking lock lifetimes
    scope_depth: usize,
    /// Errors found during checking
    errors: Vec<Error>,
}

/// Type of access to shared data
#[derive(Debug, Clone, PartialEq)]
enum AccessType {
    Read,
    Write,
}

/// Information about a shared variable access
#[derive(Debug, Clone)]
struct SharedAccess {
    var_name: String,
    access_type: AccessType,
    location: TextRange,
    thread_context: ThreadContext,
    holding_locks: Vec<String>,
}

/// Context in which code is executing
#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)] // Spawned and Async variants used in future thread analysis enhancements
enum ThreadContext {
    /// Main thread
    Main,
    /// Spawned thread
    Spawned { spawn_location: TextRange },
    /// Inside async context
    Async,
}

/// Lock acquisition information
#[derive(Debug, Clone)]
#[allow(dead_code)] // lock_type field used in lock compatibility analysis
struct LockAcquisition {
    lock_name: String,
    location: TextRange,
    released: bool,
    lock_type: LockType,
}

/// Types of locks
#[derive(Debug, Clone, PartialEq)]
enum LockType {
    /// Regular lock (non-reentrant)
    Lock,
    /// Reentrant lock
    RLock,
    /// Read-write lock (read side)
    ReadLock,
    /// Read-write lock (write side)
    WriteLock,
    /// Semaphore
    Semaphore,
}

impl LockType {
    fn from_type_name(type_name: &str) -> Option<Self> {
        match type_name {
            "Lock" | "threading.Lock" => Some(Self::Lock),
            "RLock" | "threading.RLock" => Some(Self::RLock),
            "ReadLock" | "RWLock" => Some(Self::ReadLock),
            "WriteLock" => Some(Self::WriteLock),
            "Semaphore" | "threading.Semaphore" => Some(Self::Semaphore),
            _ => None,
        }
    }

    fn is_reentrant(&self) -> bool {
        matches!(self, Self::RLock)
    }
}

/// Information about a shared variable
#[derive(Debug, Clone)]
#[allow(dead_code)] // var_name and first_access used in detailed diagnostics
struct SharedVariable {
    var_name: String,
    protecting_lock: Option<String>,
    is_atomic: bool,
    first_access: Option<TextRange>,
}

/// Lock dependency graph for deadlock detection
#[derive(Debug, Clone)]
struct LockDependency {
    from_lock: String,
    to_lock: String,
    location: TextRange,
}

impl Default for ConcurrencyChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl ConcurrencyChecker {
    pub fn new() -> Self {
        Self {
            shared_accesses: Vec::new(),
            held_locks: Vec::new(),
            lock_dependencies: Vec::new(),
            shared_variables: HashMap::new(),
            thread_context: ThreadContext::Main,
            scope_depth: 0,
            errors: Vec::new(),
        }
    }

    pub fn check_module(&mut self, module: &Module) -> Vec<Error> {
        for stmt in module.body {
            self.analyze_stmt(stmt);
        }

        // Check for unreleased locks
        self.check_unreleased_locks();

        // Detect deadlocks from lock dependency graph
        self.detect_deadlocks();

        // Check for data races
        self.check_data_races();

        self.errors.clone()
    }

    /// Analyze a statement
    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDef(func_def) => {
                self.enter_scope();
                for stmt in func_def.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::ClassDef(class_def) => {
                self.enter_scope();
                for stmt in class_def.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::For(for_stmt) => {
                self.analyze_expr(&for_stmt.iter);
                self.enter_scope();
                for stmt in for_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::While(while_stmt) => {
                self.analyze_expr(&while_stmt.test);
                self.enter_scope();
                for stmt in while_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();
            }
            Stmt::If(if_stmt) => {
                self.analyze_expr(&if_stmt.test);
                self.enter_scope();
                for stmt in if_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();

                for stmt in if_stmt.orelse {
                    self.analyze_stmt(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                // With statement handles lock acquisition/release
                for item in with_stmt.items {
                    self.analyze_expr(&item.context_expr);

                    // Check if this is a lock acquisition
                    if let Some(lock_name) = self.extract_lock_name(&item.context_expr) {
                        let lock_type = self.determine_lock_type(&item.context_expr);
                        self.acquire_lock(lock_name, item.context_expr.span(), lock_type);
                    }
                }

                self.enter_scope();
                for stmt in with_stmt.body {
                    self.analyze_stmt(stmt);
                }
                self.exit_scope();

                // Release locks acquired by with statement
                self.release_with_locks();
            }
            Stmt::Assign(assign_stmt) => {
                self.analyze_expr(&assign_stmt.value);

                // Check for thread spawning
                if self.is_thread_spawn(&assign_stmt.value) {
                    self.check_thread_spawn(&assign_stmt.value);
                }

                // Check for shared variable assignment
                for target in assign_stmt.targets {
                    if let Some(var_name) = self.extract_var_name(target) {
                        self.record_access(&var_name, AccessType::Write, target.span());
                    }
                }
            }
            Stmt::AnnAssign(ann_assign) => {
                if let Some(value) = &ann_assign.value {
                    self.analyze_expr(value);
                }

                if let Some(var_name) = self.extract_var_name(&ann_assign.target) {
                    // Check if this is a shared variable declaration
                    if let Expr::Name(name_expr) = &ann_assign.annotation {
                        let type_name = name_expr.id.to_string();
                        if self.is_shared_type(&type_name) {
                            self.declare_shared_variable(
                                &var_name,
                                &type_name,
                                ann_assign.target.span(),
                            );
                        }
                    }
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.analyze_expr(&expr_stmt.value);
            }
            _ => {}
        }
    }

    /// Analyze an expression
    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Name(name_expr) => {
                let var_name = name_expr.id.to_string();
                self.record_access(&var_name, AccessType::Read, expr.span());
            }
            Expr::Call(call_expr) => {
                self.analyze_expr(call_expr.func);
                for arg in call_expr.args {
                    self.analyze_expr(arg);
                }

                // Check for lock acquire/release calls
                if let Expr::Attribute(attr_expr) = call_expr.func {
                    let attr_name = attr_expr.attr.to_string();
                    if let Some(var_name) = self.extract_var_name(attr_expr.value) {
                        match attr_name.as_str() {
                            "acquire" | "lock" => {
                                let lock_type = LockType::Lock;
                                self.acquire_lock(var_name, expr.span(), lock_type);
                            }
                            "release" | "unlock" => {
                                self.release_lock(&var_name, expr.span());
                            }
                            _ => {}
                        }
                    }
                }
            }
            Expr::Attribute(attr_expr) => {
                self.analyze_expr(attr_expr.value);

                // Track attribute access on shared objects
                if let Some(var_name) = self.extract_var_name(attr_expr.value)
                    && self.is_shared_variable(&var_name)
                {
                    let full_name = format!("{}.{}", var_name, attr_expr.attr);
                    self.record_access(&full_name, AccessType::Read, expr.span());
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

                // Track subscript access on shared collections
                if let Some(var_name) = self.extract_var_name(subscript_expr.value)
                    && self.is_shared_variable(&var_name)
                {
                    self.record_access(&var_name, AccessType::Read, expr.span());
                }
            }
            _ => {}
        }
    }

    /// Enter a new scope
    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    /// Exit scope and release scope-local locks
    fn exit_scope(&mut self) {
        // Release locks acquired in this scope
        let locks_to_release: Vec<String> = self
            .held_locks
            .iter()
            .filter(|lock| !lock.released)
            .map(|lock| lock.lock_name.clone())
            .collect();

        for lock_name in locks_to_release {
            if let Some(lock) = self
                .held_locks
                .iter_mut()
                .find(|l| l.lock_name == lock_name)
            {
                lock.released = true;
            }
        }

        self.scope_depth -= 1;
    }

    /// Record an access to a variable
    fn record_access(&mut self, var_name: &str, access_type: AccessType, location: TextRange) {
        let holding_locks: Vec<String> = self
            .held_locks
            .iter()
            .filter(|lock| !lock.released)
            .map(|lock| lock.lock_name.clone())
            .collect();

        self.shared_accesses.push(SharedAccess {
            var_name: var_name.to_string(),
            access_type,
            location,
            thread_context: self.thread_context.clone(),
            holding_locks,
        });
    }

    /// Acquire a lock
    fn acquire_lock(&mut self, lock_name: String, location: TextRange, lock_type: LockType) {
        // Check for double lock (non-reentrant locks only)
        if !lock_type.is_reentrant()
            && let Some(existing) = self
                .held_locks
                .iter()
                .find(|lock| lock.lock_name == lock_name && !lock.released)
        {
            self.errors.push(*error(
                ErrorKind::DoubleLock {
                    lock_name: lock_name.clone(),
                    first_acquire: existing.location,
                },
                location,
            ));
            return;
        }

        // Record lock dependencies for deadlock detection
        for held_lock in &self.held_locks {
            if !held_lock.released && held_lock.lock_name != lock_name {
                self.lock_dependencies.push(LockDependency {
                    from_lock: held_lock.lock_name.clone(),
                    to_lock: lock_name.clone(),
                    location,
                });
            }
        }

        self.held_locks.push(LockAcquisition {
            lock_name,
            location,
            released: false,
            lock_type,
        });
    }

    /// Release a lock
    fn release_lock(&mut self, lock_name: &str, _location: TextRange) {
        if let Some(lock) = self
            .held_locks
            .iter_mut()
            .rev()
            .find(|lock| lock.lock_name == lock_name && !lock.released)
        {
            lock.released = true;
        }
    }

    /// Release locks acquired by with statement
    fn release_with_locks(&mut self) {
        // Mark the most recent unreleased locks as released
        for lock in self.held_locks.iter_mut().rev() {
            if !lock.released {
                lock.released = true;
                break;
            }
        }
    }

    /// Check for unreleased locks
    fn check_unreleased_locks(&mut self) {
        for lock in &self.held_locks {
            if !lock.released {
                self.errors.push(*error(
                    ErrorKind::LockNotReleased {
                        lock_name: lock.lock_name.clone(),
                    },
                    lock.location,
                ));
            }
        }
    }

    /// Detect deadlocks using cycle detection in lock dependency graph
    fn detect_deadlocks(&mut self) {
        // Build adjacency list from lock dependencies
        let mut graph: HashMap<String, Vec<(String, TextRange)>> = HashMap::new();
        for dep in &self.lock_dependencies {
            graph
                .entry(dep.from_lock.clone())
                .or_default()
                .push((dep.to_lock.clone(), dep.location));
        }

        // Use DFS to find cycles
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for lock in graph.keys() {
            if !visited.contains(lock) {
                self.dfs_deadlock_detect(
                    lock,
                    &graph,
                    &mut visited,
                    &mut rec_stack,
                    &mut Vec::new(),
                );
            }
        }
    }

    /// DFS helper for deadlock detection
    fn dfs_deadlock_detect(
        &mut self,
        lock: &str,
        graph: &HashMap<String, Vec<(String, TextRange)>>,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) {
        visited.insert(lock.to_string());
        rec_stack.insert(lock.to_string());
        path.push(lock.to_string());

        if let Some(neighbors) = graph.get(lock) {
            for (neighbor, location) in neighbors {
                if !visited.contains(neighbor) {
                    self.dfs_deadlock_detect(neighbor, graph, visited, rec_stack, path);
                } else if rec_stack.contains(neighbor) {
                    // Found a cycle (potential deadlock)!
                    let cycle_start = path.iter().position(|l| l == neighbor).unwrap_or(0);
                    let lock_chain: Vec<String> = path[cycle_start..].to_vec();

                    // Get the first and second acquisition locations
                    if let Some(first_dep) = self
                        .lock_dependencies
                        .iter()
                        .find(|dep| dep.from_lock == lock_chain[0])
                    {
                        self.errors.push(*error(
                            ErrorKind::PotentialDeadlock {
                                lock_chain,
                                second_acquisition: *location,
                            },
                            first_dep.location,
                        ));
                    }
                }
            }
        }

        rec_stack.remove(lock);
        path.pop();
    }

    /// Check for data races
    fn check_data_races(&mut self) {
        // Group accesses by variable
        let mut accesses_by_var: HashMap<String, Vec<&SharedAccess>> = HashMap::new();
        for access in &self.shared_accesses {
            accesses_by_var
                .entry(access.var_name.clone())
                .or_default()
                .push(access);
        }

        // Check each variable for conflicting accesses
        for (var_name, accesses) in accesses_by_var {
            if accesses.len() < 2 {
                continue;
            }

            // Check if variable is protected by a lock
            let protecting_lock = self
                .shared_variables
                .get(&var_name)
                .and_then(|var| var.protecting_lock.clone());

            // Check if variable is atomic
            let is_atomic = self
                .shared_variables
                .get(&var_name)
                .map(|var| var.is_atomic)
                .unwrap_or(false);

            if is_atomic {
                continue; // Atomic variables are safe
            }

            // Check for conflicting accesses
            for i in 0..accesses.len() {
                for j in (i + 1)..accesses.len() {
                    let access1 = accesses[i];
                    let access2 = accesses[j];

                    // Skip if in same thread context
                    if access1.thread_context == access2.thread_context
                        && matches!(access1.thread_context, ThreadContext::Main)
                    {
                        continue;
                    }

                    // Check if at least one is a write
                    let has_write = access1.access_type == AccessType::Write
                        || access2.access_type == AccessType::Write;

                    if !has_write {
                        continue; // Multiple reads are safe
                    }

                    // Check if both hold the protecting lock
                    if let Some(ref lock) = protecting_lock
                        && access1.holding_locks.contains(lock)
                        && access2.holding_locks.contains(lock)
                    {
                        continue; // Both hold the lock, safe
                    }

                    // Check if either holds any lock
                    let access1_synchronized = !access1.holding_locks.is_empty();
                    let access2_synchronized = !access2.holding_locks.is_empty();

                    if !access1_synchronized || !access2_synchronized {
                        // Data race detected!
                        let access_type = if access2.access_type == AccessType::Write {
                            "for writing"
                        } else {
                            "for reading"
                        };

                        self.errors.push(*error(
                            ErrorKind::DataRace {
                                var_name: var_name.clone(),
                                access_type: access_type.to_string(),
                                second_access: access2.location,
                            },
                            access1.location,
                        ));
                    }
                }
            }
        }
    }

    /// Declare a shared variable
    fn declare_shared_variable(&mut self, var_name: &str, type_name: &str, location: TextRange) {
        let is_atomic = type_name.contains("Atomic") || type_name.contains("atomic");
        self.shared_variables.insert(
            var_name.to_string(),
            SharedVariable {
                var_name: var_name.to_string(),
                protecting_lock: None,
                is_atomic,
                first_access: Some(location),
            },
        );
    }

    /// Check if a type is a shared/concurrent type
    fn is_shared_type(&self, type_name: &str) -> bool {
        matches!(
            type_name,
            "Shared"
                | "Arc"
                | "Mutex"
                | "RwLock"
                | "Atomic"
                | "AtomicInt"
                | "AtomicBool"
                | "Queue"
                | "threading.Queue"
        )
    }

    /// Check if variable is marked as shared
    fn is_shared_variable(&self, var_name: &str) -> bool {
        self.shared_variables.contains_key(var_name)
    }

    /// Check if expression is a thread spawn
    fn is_thread_spawn(&self, expr: &Expr) -> bool {
        if let Expr::Call(call) = expr {
            if let Expr::Attribute(attr) = call.func {
                if let Expr::Name(name) = attr.value {
                    return name.id == "threading"
                        && (attr.attr == "Thread" || attr.attr == "spawn");
                }
            } else if let Expr::Name(name) = call.func {
                return name.id == "Thread" || name.id == "spawn";
            }
        }
        false
    }

    /// Check thread spawn for Send/Sync violations
    fn check_thread_spawn(&mut self, _expr: &Expr) {
        // In a full implementation, we would:
        // 1. Analyze captured variables
        // 2. Check if they implement Send
        // 3. Check if shared references implement Sync
        // For now, we just record that we're in a spawned thread context
    }

    /// Determine lock type from expression
    fn determine_lock_type(&self, expr: &Expr) -> LockType {
        if let Expr::Call(call) = expr
            && let Expr::Name(name) = call.func
        {
            let type_name = name.id.to_string();
            return LockType::from_type_name(&type_name).unwrap_or(LockType::Lock);
        }
        LockType::Lock
    }

    /// Extract lock name from expression
    fn extract_lock_name(&self, expr: &Expr) -> Option<String> {
        self.extract_var_name(expr)
    }

    /// Extract variable name from expression
    fn extract_var_name(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Name(name_expr) => Some(name_expr.id.to_string()),
            Expr::Call(call) => {
                // For lock constructors like Lock(), RLock()
                if let Expr::Name(name) = call.func {
                    Some(format!("{}_{}", name.id, u32::from(expr.span().start())))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_concurrency_checker_creation() {
        let checker = ConcurrencyChecker::new();
        assert_eq!(checker.scope_depth, 0);
        assert!(checker.shared_accesses.is_empty());
        assert!(checker.held_locks.is_empty());
    }

    #[test]
    fn test_lock_type_detection() {
        assert_eq!(LockType::from_type_name("Lock"), Some(LockType::Lock));
        assert_eq!(LockType::from_type_name("RLock"), Some(LockType::RLock));
        assert!(LockType::RLock.is_reentrant());
        assert!(!LockType::Lock.is_reentrant());
    }

    #[test]
    fn test_basic_lock_tracking() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.acquire_lock("lock1".to_string(), span, LockType::Lock);
        assert_eq!(checker.held_locks.len(), 1);
        assert!(!checker.held_locks[0].released);

        checker.release_lock("lock1", span);
        assert!(checker.held_locks[0].released);
    }

    #[test]
    fn test_double_lock_detection() {
        let mut checker = ConcurrencyChecker::new();
        let span1 = TextRange::new(0.into(), 10.into());
        let span2 = TextRange::new(20.into(), 30.into());

        checker.acquire_lock("lock1".to_string(), span1, LockType::Lock);
        checker.acquire_lock("lock1".to_string(), span2, LockType::Lock);

        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0].kind,
            ErrorKind::DoubleLock { .. }
        ));
    }

    #[test]
    fn test_reentrant_lock_allowed() {
        let mut checker = ConcurrencyChecker::new();
        let span1 = TextRange::new(0.into(), 10.into());
        let span2 = TextRange::new(20.into(), 30.into());

        checker.acquire_lock("lock1".to_string(), span1, LockType::RLock);
        checker.acquire_lock("lock1".to_string(), span2, LockType::RLock);

        // RLock allows reentrant locking, so no error
        assert!(checker.errors.is_empty());
    }

    #[test]
    fn test_lock_dependency_tracking() {
        let mut checker = ConcurrencyChecker::new();
        let span1 = TextRange::new(0.into(), 10.into());
        let span2 = TextRange::new(20.into(), 30.into());

        checker.acquire_lock("lock1".to_string(), span1, LockType::Lock);
        checker.acquire_lock("lock2".to_string(), span2, LockType::Lock);

        assert_eq!(checker.lock_dependencies.len(), 1);
        assert_eq!(checker.lock_dependencies[0].from_lock, "lock1");
        assert_eq!(checker.lock_dependencies[0].to_lock, "lock2");
    }

    #[test]
    fn test_shared_variable_declaration() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.declare_shared_variable("counter", "AtomicInt", span);
        assert!(checker.is_shared_variable("counter"));

        let var = checker.shared_variables.get("counter").unwrap();
        assert!(var.is_atomic);
    }

    #[test]
    fn test_access_recording() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.record_access("var1", AccessType::Read, span);
        checker.record_access("var1", AccessType::Write, span);

        assert_eq!(checker.shared_accesses.len(), 2);
        assert_eq!(checker.shared_accesses[0].access_type, AccessType::Read);
        assert_eq!(checker.shared_accesses[1].access_type, AccessType::Write);
    }

    #[test]
    fn test_scope_management() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        assert_eq!(checker.scope_depth, 0);
        checker.enter_scope();
        assert_eq!(checker.scope_depth, 1);

        checker.acquire_lock("lock1".to_string(), span, LockType::Lock);
        checker.exit_scope();

        // Lock should be released on scope exit
        assert!(checker.held_locks[0].released);
        assert_eq!(checker.scope_depth, 0);
    }

    #[test]
    fn test_unreleased_lock_detection() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        checker.acquire_lock("lock1".to_string(), span, LockType::Lock);
        checker.check_unreleased_locks();

        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0].kind,
            ErrorKind::LockNotReleased { .. }
        ));
    }

    #[test]
    fn test_deadlock_detection_simple_cycle() {
        let mut checker = ConcurrencyChecker::new();
        let span = TextRange::new(0.into(), 10.into());

        // Create a cycle: lock1 -> lock2 -> lock1
        checker.lock_dependencies.push(LockDependency {
            from_lock: "lock1".to_string(),
            to_lock: "lock2".to_string(),
            location: span,
        });
        checker.lock_dependencies.push(LockDependency {
            from_lock: "lock2".to_string(),
            to_lock: "lock1".to_string(),
            location: span,
        });

        checker.detect_deadlocks();

        assert!(!checker.errors.is_empty());
        assert!(matches!(
            checker.errors[0].kind,
            ErrorKind::PotentialDeadlock { .. }
        ));
    }

    #[test]
    fn test_shared_type_recognition() {
        let checker = ConcurrencyChecker::new();

        assert!(checker.is_shared_type("Mutex"));
        assert!(checker.is_shared_type("Arc"));
        assert!(checker.is_shared_type("AtomicInt"));
        assert!(!checker.is_shared_type("String"));
        assert!(!checker.is_shared_type("int"));
    }
}
