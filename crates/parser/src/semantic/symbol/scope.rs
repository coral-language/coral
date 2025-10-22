// Lexical scope tracking

use crate::semantic::types::Type;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use text_size::TextRange;

/// The type of scope in the LEGB scope hierarchy (Local, Enclosing, Global, Builtin)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeType {
    /// Module/global scope
    Module,
    /// Function scope (including lambdas)
    Function,
    /// Class scope
    Class,
    /// Comprehension scope (list/dict/set comprehensions, generator expressions)
    Comprehension,
}

/// The kind of symbol binding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// Variable assignment (most common)
    Assignment,
    /// Function parameter
    Parameter,
    /// Function definition
    Function,
    /// Class definition
    Class,
    /// Import statement
    Import,
    /// Comprehension variable (for x in ...)
    ComprehensionTarget,
    /// Exception handler variable (except E as e:)
    ExceptionHandler,
    /// With statement target (with x as y:)
    WithTarget,
}

/// Information about a name binding in a scope
#[derive(Debug, Clone)]
pub struct Symbol {
    /// The name being bound
    pub name: String,
    /// The kind of binding
    pub kind: BindingKind,
    /// Where the binding was defined
    pub definition_span: TextRange,
    /// All locations where this symbol is used
    pub usages: Vec<TextRange>,
    /// Whether this symbol is declared as global
    pub is_global: bool,
    /// Whether this symbol is declared as nonlocal
    pub is_nonlocal: bool,
    /// Whether this symbol is used (to detect unused variables)
    pub is_used: bool,
    /// The inferred or annotated type of this symbol
    pub inferred_type: Option<Type>,
    /// Whether this symbol is captured by a nested function (closure)
    pub is_captured: bool,
    /// Whether this symbol is a free variable (from enclosing scope)
    pub is_free_var: bool,
    /// The scope depth where this symbol is defined (for closure analysis)
    pub scope_depth: usize,
}

impl Symbol {
    pub fn new(name: String, kind: BindingKind, definition_span: TextRange) -> Self {
        Self {
            name,
            kind,
            definition_span,
            usages: Vec::new(),
            is_global: false,
            is_nonlocal: false,
            is_used: false,
            inferred_type: None,
            is_captured: false,
            is_free_var: false,
            scope_depth: 0,
        }
    }

    /// Add a usage of this symbol
    pub fn add_usage(&mut self, span: TextRange) {
        self.usages.push(span);
        self.is_used = true;
    }

    /// Set the inferred type for this symbol
    pub fn set_type(&mut self, ty: Type) {
        self.inferred_type = Some(ty);
    }

    /// Get the inferred type for this symbol
    pub fn get_type(&self) -> Option<&Type> {
        self.inferred_type.as_ref()
    }

    /// Mark this symbol as captured by a nested function
    pub fn mark_captured(&mut self) {
        self.is_captured = true;
    }

    /// Mark this symbol as a free variable from an enclosing scope
    pub fn mark_free_var(&mut self) {
        self.is_free_var = true;
    }

    /// Set the scope depth for this symbol
    pub fn set_scope_depth(&mut self, depth: usize) {
        self.scope_depth = depth;
    }
}

/// A lexical scope containing symbol bindings
#[derive(Debug)]
pub struct Scope {
    /// The type of this scope
    pub scope_type: ScopeType,
    /// The name of this scope (function/class name, or "<module>" for module)
    pub name: String,
    /// Symbols defined in this scope (protected by read-write lock for thread safety)
    pub symbols: Arc<RwLock<HashMap<String, Symbol>>>,
    /// Parent scope ID (None for module scope)
    pub parent_id: Option<usize>,
    /// Child scope IDs
    pub children: Vec<usize>,
    /// Names declared as global in this scope
    pub global_names: Vec<String>,
    /// Names declared as nonlocal in this scope
    pub nonlocal_names: Vec<String>,
}

impl Scope {
    pub fn new(scope_type: ScopeType, name: String, parent_id: Option<usize>) -> Self {
        Self {
            scope_type,
            name,
            symbols: Arc::new(RwLock::new(HashMap::new())),
            parent_id,
            children: Vec::new(),
            global_names: Vec::new(),
            nonlocal_names: Vec::new(),
        }
    }

    /// Define a new symbol in this scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), String> {
        let name = symbol.name.clone();

        // Check for duplicate definition in same scope
        if self.symbols.read().unwrap().contains_key(&name) {
            return Err(format!("Name '{}' is already defined in this scope", name));
        }

        self.symbols.write().unwrap().insert(name, symbol);
        Ok(())
    }

    /// Look up a symbol in this scope (doesn't search parent scopes)
    pub fn lookup_local<F, R>(&self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&Symbol) -> R,
    {
        let symbols = self.symbols.read().unwrap();
        symbols.get(name).map(f)
    }

    /// Look up a symbol in this scope with mutable access
    pub fn lookup_local_mut<F, R>(&self, name: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut Symbol) -> R,
    {
        let mut symbols = self.symbols.write().unwrap();
        symbols.get_mut(name).map(f)
    }

    /// Create an atomic snapshot of this scope's symbols (lock-free read)
    pub fn snapshot(&self) -> HashMap<String, Symbol> {
        self.symbols.read().unwrap().clone()
    }

    /// Look up a symbol in a snapshot (lock-free)
    pub fn lookup_in_snapshot<'a>(
        snapshot: &'a HashMap<String, Symbol>,
        name: &str,
    ) -> Option<&'a Symbol> {
        snapshot.get(name)
    }

    /// Mark a name as global
    pub fn add_global(&mut self, name: String) {
        self.global_names.push(name);
    }

    /// Mark a name as nonlocal
    pub fn add_nonlocal(&mut self, name: String) {
        self.nonlocal_names.push(name);
    }

    /// Check if a name is declared as global in this scope
    pub fn is_global(&self, name: &str) -> bool {
        self.global_names.iter().any(|n| n == name)
    }

    /// Check if a name is declared as nonlocal in this scope
    pub fn is_nonlocal(&self, name: &str) -> bool {
        self.nonlocal_names.iter().any(|n| n == name)
    }
}
