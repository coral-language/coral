// Name resolution pass - builds symbol table and resolves names

use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::symbol::{BindingKind, ScopeType, Symbol, SymbolTable};
use text_size::TextRange;

/// Name resolution visitor that builds symbol table and checks for errors
pub struct NameResolver {
    symbol_table: SymbolTable,
    errors: Vec<Error>,
}

impl NameResolver {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    /// Get the symbol table (consuming the resolver)
    pub fn into_symbol_table(self) -> (SymbolTable, Vec<Error>) {
        (self.symbol_table, self.errors)
    }

    /// Get a reference to the symbol table
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    /// Get the errors collected during resolution
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Resolve names in a module
    pub fn resolve_module(&mut self, module: &Module) {
        for stmt in module.body {
            self.visit_stmt(stmt);
        }

        // After visiting all statements, analyze closures
        self.symbol_table.analyze_closures();
    }

    /// Visit a statement
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDef(func) => self.visit_func_def(func),
            Stmt::ClassDef(class) => self.visit_class_def(class),
            Stmt::Assign(assign) => self.visit_assign(assign),
            Stmt::AugAssign(aug) => self.visit_aug_assign(aug),
            Stmt::AnnAssign(ann) => self.visit_ann_assign(ann),
            Stmt::For(for_stmt) => self.visit_for(for_stmt),
            Stmt::While(while_stmt) => self.visit_while(while_stmt),
            Stmt::If(if_stmt) => self.visit_if(if_stmt),
            Stmt::With(with_stmt) => self.visit_with(with_stmt),
            Stmt::Try(try_stmt) => self.visit_try(try_stmt),
            Stmt::Match(match_stmt) => self.visit_match(match_stmt),
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {
                    self.visit_expr_val(value);
                }
            }
            Stmt::Expr(expr_stmt) => self.visit_expr_val(&expr_stmt.value),
            Stmt::Global(global) => {
                for name in global.names {
                    self.symbol_table
                        .current_scope_mut()
                        .add_global(name.to_string());
                }
            }
            Stmt::Nonlocal(nonlocal) => {
                for name in nonlocal.names {
                    self.symbol_table
                        .current_scope_mut()
                        .add_nonlocal(name.to_string());
                    // Validate that the name exists in an enclosing scope
                    // This is simplified - full validation would check at usage time
                }
            }
            Stmt::Import(import) => {
                for (name, alias) in import.names {
                    let binding_name = alias.unwrap_or(name);
                    let symbol =
                        Symbol::new(binding_name.to_string(), BindingKind::Import, import.span);
                    if self.symbol_table.define(symbol).is_err() {
                        // Duplicate import - just a warning
                    }
                }
            }
            Stmt::From(from) => {
                for (name, alias) in from.names {
                    let binding_name = alias.unwrap_or(name);
                    let symbol =
                        Symbol::new(binding_name.to_string(), BindingKind::Import, from.span);
                    if self.symbol_table.define(symbol).is_err() {
                        // Duplicate import
                    }
                }
            }
            _ => {
                // Other statements don't define names or have special handling
            }
        }
    }

    fn visit_func_def(&mut self, func: &FuncDefStmt) {
        // Define the function in the current scope
        let symbol = Symbol::new(func.name.to_string(), BindingKind::Function, func.span);
        if self.symbol_table.define(symbol).is_err() {
            // Duplicate function definition
        }

        // Visit default values in current (outer) scope (they're evaluated when function is defined)
        for default in func.args.defaults {
            self.visit_expr_val(default);
        }

        // Enter function scope
        self.symbol_table
            .push_scope(ScopeType::Function, func.name.to_string());

        // Add parameters to function scope
        for param in func.args.args {
            let symbol = Symbol::new(param.arg.to_string(), BindingKind::Parameter, func.span);
            let _ = self.symbol_table.define(symbol);
        }

        // Visit function body
        for stmt in func.body {
            self.visit_stmt(stmt);
        }

        // Exit function scope
        self.symbol_table.pop_scope();
    }

    fn visit_class_def(&mut self, class: &ClassDefStmt) {
        // Define the class in the current scope
        let symbol = Symbol::new(class.name.to_string(), BindingKind::Class, class.span);
        if self.symbol_table.define(symbol).is_err() {
            // Duplicate class definition
        }

        // Visit base classes in outer scope
        for base in class.bases {
            self.visit_expr(base);
        }

        // Enter class scope
        self.symbol_table
            .push_scope(ScopeType::Class, class.name.to_string());

        // Visit class body
        for stmt in class.body {
            self.visit_stmt(stmt);
        }

        // Exit class scope
        self.symbol_table.pop_scope();
    }

    fn visit_assign(&mut self, assign: &AssignStmt) {
        // Visit the value first (RHS)
        self.visit_expr_val(&assign.value);

        // Then define the targets (LHS)
        for target in assign.targets {
            self.define_expr_target_val(target, BindingKind::Assignment, assign.span);
        }
    }

    fn visit_aug_assign(&mut self, aug: &AugAssignStmt) {
        // Augmented assignment: target op= value
        // Target must exist and is used, then assigned to
        self.visit_expr_val(&aug.target);
        self.visit_expr_val(&aug.value);
    }

    fn visit_ann_assign(&mut self, ann: &AnnAssignStmt) {
        // Visit the annotation
        self.visit_expr_val(&ann.annotation);

        // Visit the value if present
        if let Some(ref value) = ann.value {
            self.visit_expr_val(value);
        }

        // Define the target
        self.define_expr_target_val(&ann.target, BindingKind::Assignment, ann.span);
    }

    fn visit_for(&mut self, for_stmt: &ForStmt) {
        // Visit the iterator first
        self.visit_expr_val(&for_stmt.iter);

        // Define the target variable
        self.define_expr_target_val(&for_stmt.target, BindingKind::Assignment, for_stmt.span);

        // Visit body
        for stmt in for_stmt.body {
            self.visit_stmt(stmt);
        }

        // Visit orelse
        for stmt in for_stmt.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_while(&mut self, while_stmt: &WhileStmt) {
        self.visit_expr_val(&while_stmt.test);
        for stmt in while_stmt.body {
            self.visit_stmt(stmt);
        }
        for stmt in while_stmt.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_if(&mut self, if_stmt: &IfStmt) {
        self.visit_expr_val(&if_stmt.test);
        for stmt in if_stmt.body {
            self.visit_stmt(stmt);
        }
        for stmt in if_stmt.orelse {
            self.visit_stmt(stmt);
        }
    }

    fn visit_with(&mut self, with_stmt: &WithStmt) {
        for item in with_stmt.items {
            self.visit_expr_val(&item.context_expr);
            if let Some(ref target) = item.optional_vars {
                self.define_expr_target_val(target, BindingKind::WithTarget, with_stmt.span);
            }
        }
        for stmt in with_stmt.body {
            self.visit_stmt(stmt);
        }
    }

    fn visit_try(&mut self, try_stmt: &TryStmt) {
        for stmt in try_stmt.body {
            self.visit_stmt(stmt);
        }
        for handler in try_stmt.handlers {
            if let Some(ref type_expr) = handler.typ {
                self.visit_expr_val(type_expr);
            }
            if let Some(name) = handler.name {
                let symbol = Symbol::new(
                    name.to_string(),
                    BindingKind::ExceptionHandler,
                    handler.span,
                );
                let _ = self.symbol_table.define(symbol);
            }
            for stmt in handler.body {
                self.visit_stmt(stmt);
            }
        }
        for stmt in try_stmt.orelse {
            self.visit_stmt(stmt);
        }
        for stmt in try_stmt.finalbody {
            self.visit_stmt(stmt);
        }
    }

    fn visit_match(&mut self, match_stmt: &super::super::super::ast::patterns::MatchStmt) {
        self.visit_expr_val(&match_stmt.subject);
        for case in match_stmt.cases {
            // Pattern matching can define new names
            // This is simplified - full implementation would handle all pattern types
            for stmt in case.body {
                self.visit_stmt(stmt);
            }
        }
    }

    /// Define a target expression value (handles Name, Tuple, List, etc.)
    fn define_expr_target_val(&mut self, expr: &Expr, kind: BindingKind, span: TextRange) {
        match expr {
            Expr::Name(name) => {
                let symbol = Symbol::new(name.id.to_string(), kind, span);
                if self.symbol_table.define(symbol).is_err() {
                    // Already defined - allows shadowing
                }
            }
            Expr::Tuple(tuple) => {
                for elt in tuple.elts {
                    self.define_expr_target_val(elt, kind, span);
                }
            }
            Expr::List(list) => {
                for elt in list.elts {
                    self.define_expr_target_val(elt, kind, span);
                }
            }
            Expr::Starred(starred) => {
                self.visit_expr(starred.value);
            }
            _ => {
                // Other expressions like attributes, subscripts are not name definitions
            }
        }
    }

    /// Visit an expression value (records name usages)
    fn visit_expr_val(&mut self, expr: &Expr) {
        match expr {
            Expr::Name(name) => {
                // Record usage of this name
                if self.symbol_table.record_usage(name.id, name.span).is_err() {
                    self.errors.push(*error(
                        ErrorKind::UndefinedName {
                            name: name.id.to_string(),
                        },
                        name.span,
                    ));
                }
            }
            Expr::BinOp(binop) => {
                self.visit_expr(binop.left);
                self.visit_expr(binop.right);
            }
            Expr::UnaryOp(unary) => {
                self.visit_expr(unary.operand);
            }
            Expr::Lambda(lambda) => {
                // Lambda creates a new scope
                self.symbol_table
                    .push_scope(ScopeType::Function, "<lambda>".to_string());

                // Add parameters
                for param in lambda.args.args {
                    let symbol =
                        Symbol::new(param.arg.to_string(), BindingKind::Parameter, lambda.span);
                    let _ = self.symbol_table.define(symbol);
                }

                // Visit body
                self.visit_expr(lambda.body);

                self.symbol_table.pop_scope();
            }
            Expr::IfExp(ifexp) => {
                self.visit_expr(ifexp.test);
                self.visit_expr(ifexp.body);
                self.visit_expr(ifexp.orelse);
            }
            Expr::Dict(dict) => {
                for key in dict.keys {
                    if let Some(k) = key.as_ref() {
                        self.visit_expr_val(k);
                    }
                }
                for value in dict.values {
                    self.visit_expr_val(value);
                }
            }
            Expr::Set(set) => {
                for elt in set.elts {
                    self.visit_expr_val(elt);
                }
            }
            Expr::ListComp(comp) => self.visit_list_comp(comp),
            Expr::SetComp(comp) => self.visit_set_comp(comp),
            Expr::DictComp(comp) => self.visit_dict_comp(comp),
            Expr::GeneratorExp(generator) => self.visit_generatorerator_exp(generator),
            Expr::Await(await_expr) => {
                self.visit_expr(await_expr.value);
            }
            Expr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    self.visit_expr_val(value);
                }
            }
            Expr::YieldFrom(yield_from) => {
                self.visit_expr_val(yield_from.value);
            }
            Expr::Compare(comp) => {
                self.visit_expr_val(comp.left);
                for comparator in comp.comparators {
                    self.visit_expr_val(comparator);
                }
            }
            Expr::Call(call) => {
                self.visit_expr_val(call.func);
                for arg in call.args {
                    self.visit_expr_val(arg);
                }
                for keyword in call.keywords {
                    self.visit_expr_val(&keyword.value);
                }
            }
            Expr::Attribute(attr) => {
                self.visit_expr(attr.value);
            }
            Expr::Subscript(sub) => {
                self.visit_expr(sub.value);
                self.visit_expr(sub.slice);
            }
            Expr::Starred(starred) => {
                self.visit_expr_val(starred.value);
            }
            Expr::List(list) => {
                for elt in list.elts {
                    self.visit_expr_val(elt);
                }
            }
            Expr::Tuple(tuple) => {
                for elt in tuple.elts {
                    self.visit_expr_val(elt);
                }
            }
            Expr::Slice(slice) => {
                if let Some(lower) = slice.lower {
                    self.visit_expr(lower);
                }
                if let Some(upper) = slice.upper {
                    self.visit_expr(upper);
                }
                if let Some(step) = slice.step {
                    self.visit_expr(step);
                }
            }
            Expr::BoolOp(boolop) => {
                for value in boolop.values {
                    self.visit_expr_val(value);
                }
            }
            // Literals don't reference names
            Expr::Constant(_)
            | Expr::JoinedStr(_)
            | Expr::FormattedValue(_)
            | Expr::Complex(_)
            | Expr::Bytes(_)
            | Expr::TString(_)
            | Expr::ModuleIntrospection(_) => {}
            Expr::NamedExpr(named) => {
                self.visit_expr(named.target);
                self.visit_expr(named.value);
            }
        }
    }

    /// Visit an expression reference (records name usages)
    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Name(name) => {
                // Record usage of this name
                if self.symbol_table.record_usage(name.id, name.span).is_err() {
                    self.errors.push(*error(
                        ErrorKind::UndefinedName {
                            name: name.id.to_string(),
                        },
                        name.span,
                    ));
                }
            }
            Expr::BinOp(binop) => {
                self.visit_expr(binop.left);
                self.visit_expr(binop.right);
            }
            Expr::UnaryOp(unary) => {
                self.visit_expr(unary.operand);
            }
            Expr::Lambda(lambda) => {
                // Lambda creates a new scope
                self.symbol_table
                    .push_scope(ScopeType::Function, "<lambda>".to_string());

                // Add parameters
                for param in lambda.args.args {
                    let symbol =
                        Symbol::new(param.arg.to_string(), BindingKind::Parameter, lambda.span);
                    let _ = self.symbol_table.define(symbol);
                }

                // Visit body
                self.visit_expr(lambda.body);

                self.symbol_table.pop_scope();
            }
            Expr::IfExp(ifexp) => {
                self.visit_expr(ifexp.test);
                self.visit_expr(ifexp.body);
                self.visit_expr(ifexp.orelse);
            }
            Expr::Dict(dict) => {
                for key in dict.keys.iter().flatten() {
                    self.visit_expr(key);
                }
                for value in dict.values {
                    self.visit_expr(value);
                }
            }
            Expr::Set(set) => {
                for elt in set.elts {
                    self.visit_expr(elt);
                }
            }
            Expr::ListComp(comp) => self.visit_list_comp(comp),
            Expr::SetComp(comp) => self.visit_set_comp(comp),
            Expr::DictComp(comp) => self.visit_dict_comp(comp),
            Expr::GeneratorExp(generator) => self.visit_generatorerator_exp(generator),
            Expr::Await(await_expr) => {
                self.visit_expr(await_expr.value);
            }
            Expr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    self.visit_expr(value);
                }
            }
            Expr::YieldFrom(yield_from) => {
                self.visit_expr(yield_from.value);
            }
            Expr::Compare(comp) => {
                self.visit_expr(comp.left);
                for comparator in comp.comparators {
                    self.visit_expr(comparator);
                }
            }
            Expr::Call(call) => {
                self.visit_expr(call.func);
                for arg in call.args {
                    self.visit_expr_val(arg);
                }
                for keyword in call.keywords {
                    self.visit_expr_val(&keyword.value);
                }
            }
            Expr::Attribute(attr) => {
                self.visit_expr(attr.value);
            }
            Expr::Subscript(sub) => {
                self.visit_expr(sub.value);
                self.visit_expr(sub.slice);
            }
            Expr::Starred(starred) => {
                self.visit_expr(starred.value);
            }
            Expr::List(list) => {
                for elt in list.elts {
                    self.visit_expr(elt);
                }
            }
            Expr::Tuple(tuple) => {
                for elt in tuple.elts {
                    self.visit_expr(elt);
                }
            }
            Expr::Slice(slice) => {
                if let Some(lower) = slice.lower {
                    self.visit_expr(lower);
                }
                if let Some(upper) = slice.upper {
                    self.visit_expr(upper);
                }
                if let Some(step) = slice.step {
                    self.visit_expr(step);
                }
            }
            // Literals don't reference names
            Expr::Constant(_) | Expr::JoinedStr(_) | Expr::FormattedValue(_) => {}
            _ => {}
        }
    }

    fn visit_list_comp(&mut self, comp: &ListCompExpr) {
        self.visit_comprehension(comp.generators, |resolver| {
            resolver.visit_expr(comp.elt);
        });
    }

    fn visit_set_comp(&mut self, comp: &SetCompExpr) {
        self.visit_comprehension(comp.generators, |resolver| {
            resolver.visit_expr(comp.elt);
        });
    }

    fn visit_dict_comp(&mut self, comp: &DictCompExpr) {
        self.visit_comprehension(comp.generators, |resolver| {
            resolver.visit_expr(comp.key);
            resolver.visit_expr(comp.value);
        });
    }

    fn visit_generatorerator_exp(&mut self, generatorerator: &GeneratorExpExpr) {
        self.visit_comprehension(generatorerator.generators, |resolver| {
            resolver.visit_expr(generatorerator.elt);
        });
    }

    fn visit_comprehension<F>(&mut self, generators: &[Comprehension], visit_elt: F)
    where
        F: FnOnce(&mut Self),
    {
        // Comprehensions create their own scope
        self.symbol_table
            .push_scope(ScopeType::Comprehension, "<comp>".to_string());

        for (i, comp) in generators.iter().enumerate() {
            // First generatorerator's iter is evaluated in enclosing scope
            if i == 0 {
                self.symbol_table.pop_scope();
                self.visit_expr_val(&comp.iter);
                self.symbol_table
                    .push_scope(ScopeType::Comprehension, "<comp>".to_string());
            } else {
                self.visit_expr_val(&comp.iter);
            }

            // Define the target in comprehension scope
            self.define_expr_target_val(
                &comp.target,
                BindingKind::ComprehensionTarget,
                comp.iter.span(),
            );

            // Visit conditions
            for if_clause in comp.ifs {
                self.visit_expr_val(if_clause);
            }
        }

        // Visit the element expression
        visit_elt(self);

        self.symbol_table.pop_scope();
    }
}

impl Default for NameResolver {
    fn default() -> Self {
        Self::new()
    }
}
