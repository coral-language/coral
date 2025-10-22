use super::scope::{BindingKind, Scope, ScopeType, Symbol};
use crate::semantic::metrics::MetricsCollector;
use crate::semantic::types::Type;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use text_size::TextRange;

/// A symbol table managing all scopes in a module
#[derive(Debug)]
pub struct SymbolTable {
    /// All scopes, indexed by scope ID
    scopes: Vec<Scope>,
    /// Current scope stack (scope IDs)
    scope_stack: Vec<usize>,
    /// Module scope ID (always 0)
    module_scope_id: usize,
}

/// Thread-safe wrapper for SymbolTable using coarse-grained locking
#[derive(Debug, Clone)]
pub struct SyncSymbolTable {
    /// The underlying symbol table protected by a read-write lock
    table: Arc<RwLock<SymbolTable>>,
}

/// Atomic snapshot of a symbol table for lock-free reads
#[derive(Debug, Clone)]
pub struct SymbolTableSnapshot {
    /// All scopes in the snapshot (shared symbol references for memory efficiency)
    scope_symbols: Vec<HashMap<String, Arc<Symbol>>>,
    /// Current scope stack
    scope_stack: Vec<usize>,
    /// Module scope ID
    module_scope_id: usize,
    /// Scope metadata (types, names, etc.)
    scope_metadata: Vec<ScopeMetadata>,
}

/// Metadata for a scope snapshot (without the symbols)
#[derive(Debug, Clone)]
pub struct ScopeMetadata {
    scope_type: ScopeType,
    name: String,
    parent_id: Option<usize>,
    children: Vec<usize>,
    global_names: Vec<String>,
    nonlocal_names: Vec<String>,
}

impl SymbolTable {
    /// Create a new symbol table with a module scope
    pub fn new() -> Self {
        let mut scopes = Vec::new();
        let module_scope = Scope::new(ScopeType::Module, "<module>".to_string(), None);
        scopes.push(module_scope);

        Self {
            scopes,
            scope_stack: vec![0], // Start in module scope
            module_scope_id: 0,
        }
    }

    /// Get the current scope ID
    pub fn current_scope_id(&self) -> usize {
        *self
            .scope_stack
            .last()
            .expect("Scope stack should never be empty")
    }

    /// Get the current scope
    pub fn current_scope(&self) -> &Scope {
        &self.scopes[self.current_scope_id()]
    }

    /// Get the current scope (mutable)
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        let id = self.current_scope_id();
        &mut self.scopes[id]
    }

    /// Get a scope by ID
    pub fn get_scope(&self, id: usize) -> Option<&Scope> {
        self.scopes.get(id)
    }

    /// Get a scope by ID (mutable)
    pub fn get_scope_mut(&mut self, id: usize) -> Option<&mut Scope> {
        self.scopes.get_mut(id)
    }

    /// Enter a new scope
    pub fn push_scope(&mut self, scope_type: ScopeType, name: String) -> usize {
        let parent_id = self.current_scope_id();
        let scope_id = self.scopes.len();

        let scope = Scope::new(scope_type, name, Some(parent_id));
        self.scopes.push(scope);

        // Add this scope as a child of its parent
        self.scopes[parent_id].children.push(scope_id);

        self.scope_stack.push(scope_id);
        scope_id
    }

    /// Enter an existing child scope by name and type
    /// Returns true if the scope was found and entered
    pub fn enter_child_scope(&mut self, scope_type: ScopeType, name: &str) -> bool {
        let current_id = self.current_scope_id();
        let current_scope = &self.scopes[current_id];

        // Find matching child scope
        for &child_id in &current_scope.children {
            let child = &self.scopes[child_id];
            if child.scope_type == scope_type && child.name == name {
                self.scope_stack.push(child_id);
                return true;
            }
        }
        false
    }

    /// Exit the current scope
    pub fn pop_scope(&mut self) {
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    /// Define a symbol in the current scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), String> {
        self.current_scope_mut().define(symbol)
    }

    /// Look up a symbol following LEGB rules
    /// Returns (symbol, scope_id) if found
    pub fn lookup(&self, name: &str) -> Option<(Symbol, usize)> {
        let current_id = self.current_scope_id();
        let current_scope = &self.scopes[current_id];

        // Check if name is declared as global
        if current_scope.is_global(name) {
            // Look only in module scope
            if let Some(symbol) =
                self.scopes[self.module_scope_id].lookup_local(name, |s| s.clone())
            {
                return Some((symbol, self.module_scope_id));
            }
            return None;
        }

        // Check if name is declared as nonlocal
        if current_scope.is_nonlocal(name) {
            // Look in enclosing scopes (skip current)
            return self.lookup_in_enclosing(name, current_id);
        }

        // Normal LEGB lookup
        self.lookup_legb(name, current_id)
    }

    /// Perform LEGB lookup starting from a specific scope
    fn lookup_legb(&self, name: &str, start_scope_id: usize) -> Option<(Symbol, usize)> {
        let mut scope_id = start_scope_id;

        loop {
            let scope = &self.scopes[scope_id];

            // Local: Check current scope
            if let Some(symbol) = scope.lookup_local(name, |s| s.clone()) {
                return Some((symbol, scope_id));
            }

            // Move to enclosing scope
            match scope.parent_id {
                Some(parent_id) => scope_id = parent_id,
                None => break, // Reached module scope
            }
        }

        // Not found (built-ins would be checked by the caller if needed)
        None
    }

    /// Look up in enclosing scopes (for nonlocal)
    fn lookup_in_enclosing(&self, name: &str, current_scope_id: usize) -> Option<(Symbol, usize)> {
        let current_scope = &self.scopes[current_scope_id];

        if let Some(parent_id) = current_scope.parent_id {
            self.lookup_legb(name, parent_id)
        } else {
            None
        }
    }

    /// Record a usage of a name
    pub fn record_usage(&mut self, name: &str, span: TextRange) -> Result<(), String> {
        // Find the symbol using LEGB lookup
        if let Some((_, scope_id)) = self.lookup(name) {
            // Add usage to the symbol
            let result = self.scopes[scope_id].lookup_local_mut(name, |symbol| {
                symbol.add_usage(span);
            });
            if result.is_some() {
                return Ok(());
            }
        }

        Err(format!("Undefined name: '{}'", name))
    }

    /// Get all scopes
    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    /// Get the module scope
    pub fn module_scope(&self) -> &Scope {
        &self.scopes[self.module_scope_id]
    }

    /// Check for unused variables in all scopes
    pub fn find_unused_variables(&self) -> Vec<(Symbol, usize)> {
        let mut unused = Vec::new();

        for (scope_id, scope) in self.scopes.iter().enumerate() {
            // For each symbol in the scope, check if it's unused
            let symbols = scope.symbols.read().unwrap();
            for symbol in symbols.values() {
                // Skip function/class definitions and imports (often intentionally unused)
                if matches!(
                    symbol.kind,
                    BindingKind::Assignment | BindingKind::Parameter
                ) && !symbol.is_used
                    && !symbol.name.starts_with('_')
                {
                    unused.push((symbol.clone(), scope_id));
                }
            }
        }

        unused
    }

    /// Set the type for a symbol by name
    pub fn set_symbol_type(&mut self, name: &str, ty: Type) {
        if let Some((scope_id, symbol_name)) = self.find_symbol_scope(name) {
            self.scopes[scope_id].lookup_local_mut(&symbol_name, |symbol| {
                symbol.set_type(ty);
            });
        }
    }

    /// Get the type for a symbol by name
    pub fn get_symbol_type(&self, name: &str) -> Option<Type> {
        self.lookup(name)
            .and_then(|(sym, _)| sym.get_type().cloned())
    }

    /// Find which scope contains a symbol by name
    fn find_symbol_scope(&self, name: &str) -> Option<(usize, String)> {
        self.lookup(name)
            .map(|(_, scope_id)| (scope_id, name.to_string()))
    }

    /// Analyze closure captures - mark symbols that are captured by nested functions
    pub fn analyze_closures(&mut self) {
        // For each function scope, check if it references variables from enclosing scopes
        for scope_id in 0..self.scopes.len() {
            if let Some(scope) = self.scopes.get(scope_id)
                && scope.scope_type == ScopeType::Function
            {
                // Get list of symbols referenced in this scope
                let referenced_names: Vec<String> = {
                    let symbols = scope.symbols.read().unwrap();
                    symbols
                        .values()
                        .filter(|s| s.is_free_var)
                        .map(|s| s.name.clone())
                        .collect()
                };

                // Mark these symbols as captured in their defining scopes
                for name in &referenced_names {
                    self.mark_symbol_captured(name, scope_id);
                }
            }
        }
    }

    /// Mark a symbol as captured when accessed from a nested scope
    fn mark_symbol_captured(&mut self, name: &str, accessing_scope_id: usize) {
        // Walk up the scope chain from the accessing scope
        let mut current_id = accessing_scope_id;

        while let Some(scope) = self.scopes.get(current_id) {
            if let Some(parent_id) = scope.parent_id {
                // Check if the symbol is defined in the parent scope
                if let Some(scope) = self.scopes.get(parent_id) {
                    let found = scope.lookup_local_mut(name, |symbol| {
                        symbol.mark_captured();
                    });
                    if found.is_some() {
                        return;
                    }
                }
                current_id = parent_id;
            } else {
                break;
            }
        }
    }

    /// Mark a symbol usage and track if it's a free variable
    pub fn mark_usage_and_check_closure(&mut self, name: &str, span: TextRange) -> Option<()> {
        let current_scope_id = self.current_scope_id();

        // First, try to find the symbol
        let (_, defining_scope_id) = self.lookup(name)?;

        // If the defining scope is different from the current scope, it might be a free variable
        if defining_scope_id != current_scope_id {
            // Check if it's a free variable (from an enclosing function scope)
            let defining_scope = self.scopes.get(defining_scope_id)?;
            let current_scope = self.scopes.get(current_scope_id)?;

            if current_scope.scope_type == ScopeType::Function
                && defining_scope.scope_type == ScopeType::Function
            {
                // Mark the symbol as captured in the defining scope
                if let Some(scope) = self.scopes.get(defining_scope_id) {
                    scope.lookup_local_mut(name, |symbol| {
                        symbol.mark_captured();
                    });
                }

                // Create a free variable entry in the current scope if it doesn't exist
                let symbol_name = name.to_string();
                if let Some(current) = self.scopes.get_mut(current_scope_id) {
                    // Check if the symbol already exists as a free variable
                    let exists = current
                        .lookup_local(&symbol_name, |symbol| symbol.is_free_var)
                        .unwrap_or(false);

                    if !exists {
                        // Create a new free variable entry
                        let mut free_var = Symbol::new(
                            symbol_name.clone(),
                            BindingKind::Parameter, // Free variables are like implicit parameters
                            span,
                        );
                        free_var.mark_free_var();
                        free_var.set_scope_depth(current_scope_id);

                        // Add it to the current scope
                        let _ = current.define(free_var);
                    }
                }
            }
        }

        // Add usage to the symbol
        if let Some(scope) = self.scopes.get(defining_scope_id) {
            scope.lookup_local_mut(name, |symbol| {
                symbol.add_usage(span);
            });
        }

        Some(())
    }

    /// Create an atomic snapshot of the entire symbol table (consistent view)
    pub fn atomic_snapshot(&self) -> SymbolTableSnapshot {
        self.atomic_snapshot_with_metrics(None)
    }

    /// Create an atomic snapshot with optional metrics collection
    pub fn atomic_snapshot_with_metrics(
        &self,
        mut metrics_collector: Option<&mut MetricsCollector>,
    ) -> SymbolTableSnapshot {
        // Collect metrics if enabled
        let mut before_size = 0;
        let mut total_symbols = 0;

        // Calculate size before snapshot (rough estimate)
        if let Some(ref mut collector) = metrics_collector
            && collector.memory_metrics_enabled()
        {
            for scope in &self.scopes {
                if let Ok(symbols) = scope.symbols.read() {
                    for symbol in symbols.values() {
                        // Rough estimate: symbol struct size + string sizes
                        before_size += std::mem::size_of::<Symbol>() + symbol.name.capacity();
                    }
                    total_symbols += symbols.len();
                }
            }

            collector.record_symbol_count(total_symbols);
            collector.record_scope_count(self.scopes.len());
        }

        let mut scope_symbols = Vec::new();
        let mut scope_metadata = Vec::new();
        let mut arc_count = 0;

        // Create snapshots of all scopes atomically
        for scope in &self.scopes {
            let symbols_snapshot = scope.snapshot();

            // Count Arc references in the snapshot
            for symbol_arc in symbols_snapshot.values() {
                arc_count += Arc::strong_count(symbol_arc);
            }

            scope_symbols.push(symbols_snapshot);

            let metadata = ScopeMetadata {
                scope_type: scope.scope_type,
                name: scope.name.clone(),
                parent_id: scope.parent_id,
                children: scope.children.clone(),
                global_names: scope.global_names.clone(),
                nonlocal_names: scope.nonlocal_names.clone(),
            };
            scope_metadata.push(metadata);
        }

        let snapshot = SymbolTableSnapshot {
            scope_symbols,
            scope_stack: self.scope_stack.clone(),
            module_scope_id: self.module_scope_id,
            scope_metadata,
        };

        // Calculate size after snapshot and record metrics
        if let Some(collector) = metrics_collector
            && collector.memory_metrics_enabled()
        {
            // Rough estimate of snapshot size
            let mut after_size = std::mem::size_of::<SymbolTableSnapshot>();
            for scope_symbols in &snapshot.scope_symbols {
                after_size += std::mem::size_of::<HashMap<String, Arc<Symbol>>>()
                    + scope_symbols.len()
                        * (std::mem::size_of::<String>() + std::mem::size_of::<Arc<Symbol>>());
                // Add string capacities
                for key in scope_symbols.keys() {
                    after_size += key.capacity();
                }
            }

            collector.record_snapshot_creation(before_size, after_size, arc_count);

            // Update metrics with calculated values
            let mut metrics = collector.current_metrics().clone();
            metrics.symbol_table_size_before = before_size;
            metrics.symbol_table_size_after = after_size;
            metrics.arc_reference_count = arc_count;
            collector.update_memory_metrics(metrics);
        }

        snapshot
    }

    /// Get a summary of the symbol table for debugging
    pub fn summary(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("Symbol Table ({}scopes):\n", self.scopes.len()));

        for (id, scope) in self.scopes.iter().enumerate() {
            output.push_str(&format!(
                "\n  Scope #{} ({:?}): '{}'\n",
                id, scope.scope_type, scope.name
            ));
            output.push_str(&format!("    Parent: {:?}\n", scope.parent_id));

            let symbols = scope.symbols.read().unwrap();
            output.push_str(&format!("    Symbols: {}\n", symbols.len()));

            for (name, symbol) in symbols.iter() {
                output.push_str(&format!(
                    "      {}: {:?}, used={}, usages={}\n",
                    name,
                    symbol.kind,
                    symbol.is_used,
                    symbol.usages.len()
                ));
            }
        }

        output
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SyncSymbolTable {
    /// Create a new synchronized symbol table
    pub fn new() -> Self {
        Self {
            table: Arc::new(RwLock::new(SymbolTable::new())),
        }
    }

    /// Acquire a read lock and get a reference to the symbol table
    pub fn read<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&SymbolTable) -> R,
    {
        let guard = self.table.read().unwrap();
        f(&guard)
    }

    /// Acquire a write lock and get a mutable reference to the symbol table
    pub fn write<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut SymbolTable) -> R,
    {
        let mut guard = self.table.write().unwrap();
        f(&mut guard)
    }

    /// Create an atomic snapshot of the symbol table (lock-free read)
    pub fn atomic_snapshot(&self) -> SymbolTableSnapshot {
        self.read(|table| table.atomic_snapshot())
    }
}

impl Default for SyncSymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTableSnapshot {
    /// Look up a symbol in the snapshot (lock-free)
    pub fn lookup(&self, name: &str) -> Option<Arc<Symbol>> {
        let current_id = *self.scope_stack.last()?;
        self.lookup_legb(name, current_id)
    }

    /// Get a mutable copy of a symbol (copy-on-write for modifications)
    pub fn get_symbol_mut(&self, name: &str) -> Option<Symbol> {
        self.lookup(name).map(|arc_symbol| (*arc_symbol).clone())
    }

    /// Perform LEGB lookup in the snapshot
    fn lookup_legb(&self, name: &str, start_scope_id: usize) -> Option<Arc<Symbol>> {
        let mut scope_id = start_scope_id;

        loop {
            let metadata = self.scope_metadata.get(scope_id)?;
            let symbols = self.scope_symbols.get(scope_id)?;

            // Check if name is declared as global
            if metadata.global_names.contains(&name.to_string()) {
                // Look only in module scope
                let module_symbols = self.scope_symbols.get(self.module_scope_id)?;
                if let Some(symbol) = Scope::lookup_in_snapshot(module_symbols, name) {
                    return Some(Arc::clone(symbol));
                }
                return None;
            }

            // Check if name is declared as nonlocal
            if metadata.nonlocal_names.contains(&name.to_string()) {
                // Look in enclosing scopes (skip current)
                return self.lookup_in_enclosing(name, scope_id);
            }

            // Local: Check current scope
            if let Some(symbol) = Scope::lookup_in_snapshot(symbols, name) {
                return Some(Arc::clone(symbol));
            }

            // Move to enclosing scope
            match metadata.parent_id {
                Some(parent_id) => scope_id = parent_id,
                None => break, // Reached module scope
            }
        }

        None
    }

    /// Look up in enclosing scopes (for nonlocal)
    fn lookup_in_enclosing(&self, name: &str, current_scope_id: usize) -> Option<Arc<Symbol>> {
        let current_metadata = self.scope_metadata.get(current_scope_id)?;
        if let Some(parent_id) = current_metadata.parent_id {
            self.lookup_legb(name, parent_id)
        } else {
            None
        }
    }

    /// Get the scope symbols in this snapshot
    pub fn scope_symbols(&self) -> &[HashMap<String, Arc<Symbol>>] {
        &self.scope_symbols
    }

    /// Get the module scope symbols from this snapshot
    pub fn module_scope_symbols(&self) -> &HashMap<String, Arc<Symbol>> {
        &self.scope_symbols[self.module_scope_id]
    }

    /// Get the scope metadata for a given scope ID
    pub fn scope_metadata(&self, scope_id: usize) -> Option<&ScopeMetadata> {
        self.scope_metadata.get(scope_id)
    }

    /// Get the name of a scope by ID
    pub fn scope_name(&self, scope_id: usize) -> Option<&str> {
        self.scope_metadata.get(scope_id).map(|m| m.name.as_str())
    }

    /// Get the type of a scope by ID
    pub fn scope_type(&self, scope_id: usize) -> Option<ScopeType> {
        self.scope_metadata.get(scope_id).map(|m| m.scope_type)
    }

    /// Get the children of a scope by ID
    pub fn scope_children(&self, scope_id: usize) -> Option<&[usize]> {
        self.scope_metadata
            .get(scope_id)
            .map(|m| m.children.as_slice())
    }

    /// Check if a scope is a function scope
    pub fn is_function_scope(&self, scope_id: usize) -> bool {
        self.scope_type(scope_id) == Some(ScopeType::Function)
    }

    /// Check if a scope is a class scope
    pub fn is_class_scope(&self, scope_id: usize) -> bool {
        self.scope_type(scope_id) == Some(ScopeType::Class)
    }

    /// Find all function scopes in the snapshot
    pub fn function_scopes(&self) -> Vec<(usize, &str)> {
        self.scope_metadata
            .iter()
            .enumerate()
            .filter(|(_, metadata)| metadata.scope_type == ScopeType::Function)
            .map(|(id, metadata)| (id, metadata.name.as_str()))
            .collect()
    }

    /// Find all class scopes in the snapshot
    pub fn class_scopes(&self) -> Vec<(usize, &str)> {
        self.scope_metadata
            .iter()
            .enumerate()
            .filter(|(_, metadata)| metadata.scope_type == ScopeType::Class)
            .map(|(id, metadata)| (id, metadata.name.as_str()))
            .collect()
    }

    /// Get a summary of the snapshot for debugging
    pub fn summary(&self) -> String {
        let mut output = format!(
            "SymbolTableSnapshot ({} scopes):\n",
            self.scope_symbols.len()
        );

        for (id, metadata) in self.scope_metadata.iter().enumerate() {
            let symbols = &self.scope_symbols[id];
            output.push_str(&format!(
                "\n  Scope #{} ({:?}): '{}' ({} symbols)\n",
                id,
                metadata.scope_type,
                metadata.name,
                symbols.len()
            ));

            if let Some(parent_id) = metadata.parent_id {
                output.push_str(&format!("    Parent: #{}\n", parent_id));
            } else {
                output.push_str("    Parent: None (module scope)\n");
            }

            if !metadata.children.is_empty() {
                output.push_str(&format!("    Children: {:?}\n", metadata.children));
            }

            if !metadata.global_names.is_empty() {
                output.push_str(&format!("    Globals: {:?}\n", metadata.global_names));
            }

            if !metadata.nonlocal_names.is_empty() {
                output.push_str(&format!("    Nonlocals: {:?}\n", metadata.nonlocal_names));
            }
        }

        output
    }

    /// Traverse the scope hierarchy starting from a given scope
    pub fn traverse_scope_hierarchy(&self, start_scope_id: usize) -> Vec<usize> {
        let mut result = Vec::new();
        let mut to_visit = vec![start_scope_id];

        while let Some(scope_id) = to_visit.pop() {
            result.push(scope_id);

            // Add children to visit list (breadth-first traversal)
            if let Some(metadata) = self.scope_metadata.get(scope_id) {
                for &child_id in &metadata.children {
                    to_visit.insert(0, child_id); // Insert at beginning for breadth-first
                }
            }
        }

        result
    }

    /// Get all scopes that are descendants of a given scope
    pub fn scope_descendants(&self, scope_id: usize) -> Vec<usize> {
        let hierarchy = self.traverse_scope_hierarchy(scope_id);
        hierarchy.into_iter().skip(1).collect() // Skip the starting scope itself
    }

    /// Find scopes by type
    pub fn scopes_by_type(&self, scope_type: ScopeType) -> Vec<(usize, &str)> {
        self.scope_metadata
            .iter()
            .enumerate()
            .filter(|(_, metadata)| metadata.scope_type == scope_type)
            .map(|(id, metadata)| (id, metadata.name.as_str()))
            .collect()
    }
}
