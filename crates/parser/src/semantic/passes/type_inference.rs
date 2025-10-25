//! Type inference pass
//!
//! This pass performs bidirectional type inference on the AST:
//! - **Bottom-up inference**: Infers types from expressions and propagates upward
//! - **Top-down inference**: Propagates expected types from context (e.g., call sites)
//!
//! ## Lambda Inference
//!
//! Lambda expressions support:
//! - Explicit type annotations on parameters
//! - Type inference from call sites (bidirectional)
//! - Return type inference from body
//! - Closure capture analysis
//!
//! ## Example
//!
//! ```coral
//! # Lambda with explicit annotation
//! f = lambda x: int: x * 2
//!
//! # Lambda inferred from call site
//! def apply(f: (int) -> int, x: int) -> int:
//!     return f(x)
//!
//! result = apply(lambda x: x + 1, 5)  # x inferred as int
//! ```

use crate::ast::*;
use crate::semantic::symbol::SymbolTable;
use crate::semantic::types::{Type, builtins::BUILTIN_ATTRIBUTE_REGISTRY};
use std::collections::HashMap;

/// Parse a constant string literal to infer its type
fn infer_constant_from_str(value: &str) -> Type {
    if value == "True" || value == "False" {
        return Type::Bool;
    }
    if value == "None" {
        return Type::None;
    }

    if value.parse::<i64>().is_ok() {
        return Type::Int;
    }

    if value.parse::<f64>().is_ok() {
        return Type::Float;
    }

    Type::Str
}

/// Parse a type annotation expression into a Type
pub fn parse_annotation(annotation: &Expr) -> Type {
    match annotation {
        Expr::Name(name) => {
            match name.id {
                "int" => Type::Int,
                "str" => Type::Str,
                "float" => Type::Float,
                "bool" => Type::Bool,
                "bytes" => Type::Bytes,
                "complex" => Type::Complex,
                "None" => Type::None,
                "Any" => Type::Any,

                "list" | "List" => Type::list(Type::Unknown),
                "tuple" | "Tuple" => Type::tuple(vec![]), // Empty tuple type represents any tuple
                "dict" | "Dict" => Type::dict(Type::Unknown, Type::Unknown),
                "set" | "Set" => Type::set(Type::Unknown),

                _ => Type::Instance(name.id.to_string()),
            }
        }
        Expr::Subscript(subscript) => {
            if let Expr::Name(name) = subscript.value {
                match name.id {
                    "list" | "List" => {
                        let elem_ty = parse_annotation(subscript.slice);
                        Type::list(elem_ty)
                    }
                    "dict" | "Dict" => {
                        if let Expr::Tuple(tuple) = subscript.slice
                            && tuple.elts.len() == 2
                        {
                            let key_ty = parse_annotation(&tuple.elts[0]);
                            let val_ty = parse_annotation(&tuple.elts[1]);
                            return Type::dict(key_ty, val_ty);
                        }
                        Type::dict(Type::Unknown, Type::Unknown)
                    }
                    "set" | "Set" => {
                        let elem_ty = parse_annotation(subscript.slice);
                        Type::set(elem_ty)
                    }
                    "tuple" | "Tuple" => {
                        if let Expr::Tuple(tuple) = subscript.slice {
                            let types: Vec<Type> =
                                tuple.elts.iter().map(parse_annotation).collect();
                            Type::tuple(types)
                        } else {
                            Type::tuple(vec![parse_annotation(subscript.slice)])
                        }
                    }
                    "Optional" => {
                        let inner = parse_annotation(subscript.slice);
                        Type::optional(inner)
                    }
                    "Union" => {
                        if let Expr::Tuple(tuple) = subscript.slice {
                            let types: Vec<Type> =
                                tuple.elts.iter().map(parse_annotation).collect();
                            Type::union(types)
                        } else {
                            Type::Unknown
                        }
                    }
                    _ => Type::Unknown,
                }
            } else {
                Type::Unknown
            }
        }
        Expr::Constant(constant) => {
            if constant.value == "None" {
                Type::None
            } else {
                Type::Unknown
            }
        }
        Expr::BinOp(binop) if binop.op == "|" => {
            let left = parse_annotation(binop.left);
            let right = parse_annotation(binop.right);
            Type::union(vec![left, right])
        }
        _ => Type::Unknown,
    }
}

/// Type inference context that tracks inferred types for expressions
#[derive(Clone, Debug)]
pub struct TypeInferenceContext {
    /// Map from expression spans (start, end) to inferred types
    expr_types: HashMap<(usize, usize), Type>,
    /// Symbol table for name resolution
    symbol_table: SymbolTable,
    /// Expected type context stack for bidirectional type checking
    expected_type_stack: Vec<Option<Type>>,
    /// Track yielded types for generator functions (function name -> yielded types)
    generator_yields: HashMap<String, Vec<Type>>,
    /// Module exports for cross-module type resolution (module_name -> export_name -> type)
    pub(crate) module_exports: HashMap<String, HashMap<String, Type>>,
    /// Class attributes for user-defined classes (class_name, attr_name) -> attr_type
    pub(crate) class_attributes: HashMap<(String, String), Type>,
    /// Method resolution order for each class (class_name -> [base classes in order])
    pub(crate) class_mro: HashMap<String, Vec<String>>,
}

impl TypeInferenceContext {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            expr_types: HashMap::new(),
            symbol_table,
            expected_type_stack: vec![None], // Start with no expectation
            generator_yields: HashMap::new(),
            module_exports: HashMap::new(),
            class_attributes: HashMap::new(),
            class_mro: HashMap::new(),
        }
    }

    /// Set module exports for cross-module type resolution
    pub fn set_module_exports(&mut self, exports: HashMap<String, HashMap<String, Type>>) {
        self.module_exports = exports;
    }

    /// Set class attributes for user-defined class attribute resolution
    pub fn set_class_attributes(&mut self, attributes: HashMap<(String, String), Type>) {
        self.class_attributes = attributes;
    }

    /// Set class MRO (method resolution order) for inheritance lookups
    pub fn set_class_mro(&mut self, mro: HashMap<String, Vec<String>>) {
        self.class_mro = mro;
    }

    /// Get the inferred type for an expression by span
    pub fn get_type_by_span(&self, start: usize, end: usize) -> Option<&Type> {
        self.expr_types.get(&(start, end))
    }

    /// Set the inferred type for an expression by span
    pub fn set_type_by_span(&mut self, start: usize, end: usize, ty: Type) {
        self.expr_types.insert((start, end), ty);
    }

    /// Get the symbol table
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }

    /// Get mutable access to the symbol table
    pub fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        &mut self.symbol_table
    }

    /// Push an expected type for nested expressions
    pub fn push_expected_type(&mut self, ty: Option<Type>) {
        self.expected_type_stack.push(ty);
    }

    /// Pop the expected type
    pub fn pop_expected_type(&mut self) {
        self.expected_type_stack.pop();
    }

    /// Get current expected type
    pub fn expected_type(&self) -> Option<&Type> {
        self.expected_type_stack.last().and_then(|opt| opt.as_ref())
    }

    /// Mark that the current function contains a yield expression
    pub fn mark_current_function_as_generator(&mut self, yield_ty: Type) {
        let current_scope = self.symbol_table.current_scope();
        if current_scope.scope_type == crate::semantic::symbol::ScopeType::Function {
            self.generator_yields
                .entry(current_scope.name.clone())
                .or_default()
                .push(yield_ty);
        }
    }

    /// Check if a function is a generator function
    pub fn is_generator_function(&self, name: &str) -> bool {
        self.generator_yields.contains_key(name)
    }

    /// Get the yield type for a generator function
    pub fn get_generator_yield_type(&self, name: &str) -> Option<Type> {
        self.generator_yields.get(name).map(|types| {
            if types.is_empty() {
                Type::None
            } else if types.iter().all(|t| t == &types[0]) {
                types[0].clone()
            } else {
                Type::Union(types.clone())
            }
        })
    }
}

/// Type inference visitor that infers types from AST
pub struct TypeInference<'a> {
    context: &'a mut TypeInferenceContext,
}

impl<'a> TypeInference<'a> {
    pub fn new(context: &'a mut TypeInferenceContext) -> Self {
        Self { context }
    }

    /// Resolve attribute type for a given base type and attribute name
    #[allow(clippy::only_used_in_recursion)]
    fn resolve_attribute_type(&mut self, base_ty: &Type, attr_name: &str) -> Type {
        match base_ty {
            Type::Instance(class_name) => {
                let key = (class_name.to_string(), attr_name.to_string());
                if let Some(attr_type) = self.context.class_attributes.get(&key) {
                    return attr_type.clone();
                }

                if let Some(mro) = self.context.class_mro.get(class_name.as_str()) {
                    for base_class in mro {
                        let base_key = (base_class.clone(), attr_name.to_string());
                        if let Some(attr_type) = self.context.class_attributes.get(&base_key) {
                            return attr_type.clone();
                        }
                    }
                }

                Type::Unknown
            }
            Type::Class(_class_name) => BUILTIN_ATTRIBUTE_REGISTRY
                .lookup_builtin_attribute(base_ty, attr_name)
                .unwrap_or(Type::Unknown),
            Type::Module(module_name) => self
                .context
                .module_exports
                .get(module_name.as_str())
                .and_then(|exports| exports.get(attr_name))
                .cloned()
                .unwrap_or(Type::Unknown),
            Type::Union(types) => {
                let mut result_types = Vec::new();
                for ty in types {
                    let attr_ty = self.resolve_attribute_type(ty, attr_name);
                    if !matches!(attr_ty, Type::Unknown) {
                        result_types.push(attr_ty);
                    }
                }
                if result_types.is_empty() {
                    Type::Unknown
                } else if result_types.len() == 1 {
                    result_types.into_iter().next().unwrap()
                } else {
                    Type::Union(result_types)
                }
            }
            Type::Optional(inner_ty) => self.resolve_attribute_type(inner_ty, attr_name),
            Type::List(elem_ty) => {
                let _ = elem_ty;
                BUILTIN_ATTRIBUTE_REGISTRY
                    .lookup_builtin_attribute(base_ty, attr_name)
                    .unwrap_or(Type::Unknown)
            }
            Type::Dict(key_ty, val_ty) => {
                let _ = (key_ty, val_ty);
                BUILTIN_ATTRIBUTE_REGISTRY
                    .lookup_builtin_attribute(base_ty, attr_name)
                    .unwrap_or(Type::Unknown)
            }
            Type::Set(elem_ty) => {
                let _ = elem_ty;
                BUILTIN_ATTRIBUTE_REGISTRY
                    .lookup_builtin_attribute(base_ty, attr_name)
                    .unwrap_or(Type::Unknown)
            }
            Type::Tuple(elem_types) => {
                let _ = elem_types;
                BUILTIN_ATTRIBUTE_REGISTRY
                    .lookup_builtin_attribute(base_ty, attr_name)
                    .unwrap_or(Type::Unknown)
            }

            _ => BUILTIN_ATTRIBUTE_REGISTRY
                .lookup_builtin_attribute(base_ty, attr_name)
                .unwrap_or(Type::Unknown),
        }
    }

    /// Infer types for a module
    pub fn infer_module(&mut self, module: &Module) {
        for stmt in module.body {
            self.infer_stmt(stmt);
        }
    }

    /// Infer types for a statement
    fn infer_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign(assign) => {
                let value_type = self.infer_expr_val(&assign.value);

                for target in assign.targets {
                    self.infer_target(target, &value_type);
                }
            }
            Stmt::AnnAssign(ann) => {
                let annotated_type = parse_annotation(&ann.annotation);

                if let Some(ref value) = ann.value {
                    let _value_type = self.infer_expr_val(value);

                    self.infer_target(&ann.target, &annotated_type);

                    if let Expr::Name(name) = &ann.target {
                        self.context
                            .symbol_table_mut()
                            .set_symbol_type(name.id, annotated_type);
                    }
                } else {
                    self.infer_target(&ann.target, &annotated_type);
                }
            }
            Stmt::AugAssign(aug) => {
                let _ = self.infer_expr_val(&aug.target);
                let _ = self.infer_expr_val(&aug.value);
            }
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {
                    let _ = self.infer_expr_val(value);
                }
            }
            Stmt::Expr(expr_stmt) => {
                let _ = self.infer_expr_val(&expr_stmt.value);
            }
            Stmt::FuncDef(func) => {
                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Function, func.name);

                if !entered {
                    return;
                }

                let declared_return_ty = if let Some(returns) = &func.returns {
                    parse_annotation(returns)
                } else {
                    Type::Unknown
                };

                let return_ty = if func.is_async {
                    Type::Coroutine(Box::new(declared_return_ty))
                } else {
                    declared_return_ty
                };

                let func_ty = self.build_function_type_from_args(&func.args, return_ty);
                self.context
                    .symbol_table_mut()
                    .set_symbol_type(func.name, func_ty);

                for stmt in func.body {
                    self.infer_stmt(stmt);
                }

                if self.context.is_generator_function(func.name) {
                    let yield_ty = self
                        .context
                        .get_generator_yield_type(func.name)
                        .unwrap_or(Type::Unknown);

                    let generator_ty = Type::generator(yield_ty);

                    let func_ty = self.build_function_type_from_args(&func.args, generator_ty);
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(func.name, func_ty);
                }

                self.context.symbol_table_mut().pop_scope();
            }
            Stmt::ClassDef(class) => {
                self.context
                    .symbol_table_mut()
                    .set_symbol_type(class.name, Type::Class(class.name.to_string()));

                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Class, class.name);

                if entered {
                    for stmt in class.body {
                        self.infer_stmt(stmt);
                    }

                    self.context.symbol_table_mut().pop_scope();
                }
            }
            Stmt::For(for_stmt) => {
                let iter_type = self.infer_expr_val(&for_stmt.iter);

                let elem_type = Self::infer_iterable_element_type(iter_type);

                self.infer_target(&for_stmt.target, &elem_type);

                for stmt in for_stmt.body {
                    self.infer_stmt(stmt);
                }
                for stmt in for_stmt.orelse {
                    self.infer_stmt(stmt);
                }
            }
            Stmt::While(while_stmt) => {
                let _ = self.infer_expr_val(&while_stmt.test);
                for stmt in while_stmt.body {
                    self.infer_stmt(stmt);
                }
                for stmt in while_stmt.orelse {
                    self.infer_stmt(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                let _ = self.infer_expr_val(&if_stmt.test);
                for stmt in if_stmt.body {
                    self.infer_stmt(stmt);
                }
                for stmt in if_stmt.orelse {
                    self.infer_stmt(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                for item in with_stmt.items {
                    let _ = self.infer_expr_val(&item.context_expr);
                }
                for stmt in with_stmt.body {
                    self.infer_stmt(stmt);
                }
            }
            Stmt::Try(try_stmt) => {
                for stmt in try_stmt.body {
                    self.infer_stmt(stmt);
                }
                for handler in try_stmt.handlers {
                    for stmt in handler.body {
                        self.infer_stmt(stmt);
                    }
                }
                for stmt in try_stmt.orelse {
                    self.infer_stmt(stmt);
                }
                for stmt in try_stmt.finalbody {
                    self.infer_stmt(stmt);
                }
            }
            Stmt::Match(match_stmt) => {
                let subject_ty = self.infer_expr_val(&match_stmt.subject);
                for case in match_stmt.cases {
                    self.infer_pattern_types(&case.pattern, &subject_ty);

                    if let Some(ref guard) = case.guard {
                        let _ = self.infer_expr(guard);
                    }

                    for stmt in case.body {
                        self.infer_stmt(stmt);
                    }
                }
            }
            Stmt::Yield(yield_stmt) => {
                if let Some(ref value) = yield_stmt.value {
                    match value {
                        Expr::YieldFrom(yield_from) => {
                            let iter_ty = self.infer_expr(yield_from.value);
                            let elem_ty = Self::infer_iterable_element_type(iter_ty);
                            self.context
                                .mark_current_function_as_generator(elem_ty.clone());
                        }
                        _ => {
                            let value_ty = self.infer_expr(value);
                            self.context
                                .mark_current_function_as_generator(value_ty.clone());
                        }
                    }
                } else {
                    self.context.mark_current_function_as_generator(Type::None);
                }
            }
            _ => {}
        }
    }

    /// Infer type for assignment target
    fn infer_target(&mut self, target: &Expr, ty: &Type) {
        match target {
            Expr::Name(name) => {
                self.context
                    .symbol_table_mut()
                    .set_symbol_type(name.id, ty.clone());
            }
            Expr::Tuple(tuple) => {
                if let Type::Tuple(types) = ty {
                    for (elem, elem_ty) in tuple.elts.iter().zip(types.iter()) {
                        self.infer_target(elem, elem_ty);
                    }
                } else {
                    let elem_ty = Self::infer_iterable_element_type(ty.clone());
                    for elem in tuple.elts {
                        self.infer_target(elem, &elem_ty);
                    }
                }
            }
            Expr::List(list) => {
                if let Type::List(elem_ty) = ty {
                    let starred_count = list
                        .elts
                        .iter()
                        .filter(|e| matches!(e, Expr::Starred(_)))
                        .count();
                    if starred_count == 0 {
                        for elem in list.elts {
                            self.infer_target(elem, elem_ty);
                        }
                    } else if starred_count == 1 {
                        for elem in list.elts {
                            if let Expr::Starred(starred) = elem {
                                self.infer_target(starred.value, &Type::List(elem_ty.clone()));
                            } else {
                                self.infer_target(elem, elem_ty);
                            }
                        }
                    }
                } else {
                    let elem_ty = Self::infer_iterable_element_type(ty.clone());
                    for elem in list.elts {
                        if let Expr::Starred(starred) = elem {
                            self.infer_target(
                                starred.value,
                                &Type::List(Box::new(elem_ty.clone())),
                            );
                        } else {
                            self.infer_target(elem, &elem_ty);
                        }
                    }
                }
            }
            Expr::Dict(dict) => {
                if let Type::Dict(_key_ty, val_ty) = ty {
                    for (_key, value) in dict.keys.iter().zip(dict.values.iter()) {
                        self.infer_target(value, val_ty);
                    }
                }
            }
            Expr::Starred(starred) => {
                if let Type::List(elem_ty) = ty {
                    self.infer_target(starred.value, &Type::List(elem_ty.clone()));
                } else {
                    let elem_ty = Self::infer_iterable_element_type(ty.clone());
                    self.infer_target(starred.value, &Type::List(Box::new(elem_ty)));
                }
            }
            _ => {}
        }
    }

    /// Extract element type from an iterable type
    fn infer_iterable_element_type(iter_ty: Type) -> Type {
        match iter_ty {
            Type::List(elem) => *elem,
            Type::Set(elem) => *elem,
            Type::Tuple(elems) => {
                if elems.is_empty() {
                    Type::Unknown
                } else if elems.iter().all(|t| t == &elems[0]) {
                    elems[0].clone()
                } else {
                    Type::Union(elems)
                }
            }
            Type::Dict(key, _) => *key,
            Type::Str => Type::Str,
            Type::Bytes => Type::Int,
            Type::Generator(elem) => *elem, // Support generator iteration
            Type::Any => Type::Any,         // Support gradual typing
            Type::Union(types) => {
                let elem_types: Vec<Type> = types
                    .into_iter()
                    .map(Self::infer_iterable_element_type)
                    .filter(|t| !matches!(t, Type::Unknown))
                    .collect();
                if elem_types.is_empty() {
                    Type::Unknown
                } else if elem_types.len() == 1 {
                    elem_types.into_iter().next().unwrap()
                } else {
                    Type::Union(elem_types)
                }
            }
            _ => Type::Unknown,
        }
    }

    /// Infer type from an expression (owned)
    fn infer_expr_val(&mut self, expr: &Expr) -> Type {
        self.infer_expr(expr)
    }

    fn infer_expr_with_expected(&mut self, expr: &Expr, expected: Option<Type>) -> Type {
        self.context.push_expected_type(expected);
        let ty = self.infer_expr(expr);
        self.context.pop_expected_type();
        ty
    }

    /// Infer type from an expression
    fn infer_expr(&mut self, expr: &Expr) -> Type {
        let ty = match expr {
            Expr::Constant(constant) => infer_constant_from_str(constant.value),
            Expr::Complex(_) => Type::Complex,
            Expr::Bytes(_) => Type::Bytes,
            Expr::Name(name) => self
                .context
                .symbol_table()
                .get_symbol_type(name.id)
                .unwrap_or(Type::Unknown),

            Expr::List(list) => {
                if list.elts.is_empty() {
                    Type::list(Type::Unknown)
                } else {
                    let elem_ty = self.infer_expr(&list.elts[0]);
                    Type::list(elem_ty)
                }
            }
            Expr::Tuple(tuple) => {
                let types: Vec<Type> = tuple.elts.iter().map(|e| self.infer_expr(e)).collect();
                Type::tuple(types)
            }
            Expr::Set(set) => {
                if set.elts.is_empty() {
                    Type::set(Type::Unknown)
                } else {
                    let elem_ty = self.infer_expr(&set.elts[0]);
                    Type::set(elem_ty)
                }
            }
            Expr::Dict(dict) => {
                if dict.keys.is_empty() {
                    Type::dict(Type::Unknown, Type::Unknown)
                } else {
                    let key_ty = if let Some(ref key) = dict.keys[0] {
                        self.infer_expr(key)
                    } else {
                        Type::Unknown
                    };
                    let value_ty = self.infer_expr(&dict.values[0]);
                    Type::dict(key_ty, value_ty)
                }
            }

            Expr::ListComp(comp) => {
                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Comprehension, "<comp>");

                if !entered {
                    let elem_ty = self.infer_expr(comp.elt);
                    return Type::list(elem_ty);
                }

                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);

                    let elem_ty = Self::infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);

                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let elem_ty = self.infer_expr(comp.elt);

                self.context.symbol_table_mut().pop_scope();

                Type::list(elem_ty)
            }
            Expr::SetComp(comp) => {
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = Self::infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let elem_ty = self.infer_expr(comp.elt);

                self.context.symbol_table_mut().pop_scope();

                Type::set(elem_ty)
            }
            Expr::DictComp(comp) => {
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = Self::infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let key_ty = self.infer_expr(comp.key);
                let value_ty = self.infer_expr(comp.value);

                self.context.symbol_table_mut().pop_scope();

                Type::dict(key_ty, value_ty)
            }
            Expr::GeneratorExp(comp) => {
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = Self::infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }

                let elem_ty = self.infer_expr(comp.elt);

                self.context.symbol_table_mut().pop_scope();

                Type::generator(elem_ty) // Return proper generator type
            }

            Expr::BinOp(binop) => self.infer_binop(binop),
            Expr::UnaryOp(unary) => self.infer_unaryop(unary),
            Expr::Compare(compare) => {
                let _left_ty = self.infer_expr(compare.left);
                for comparator in compare.comparators {
                    let _comp_ty = self.infer_expr(comparator);
                }

                Type::Bool
            }
            Expr::BoolOp(boolop) => {
                for value in boolop.values {
                    let _val_ty = self.infer_expr(value);
                }

                Type::Bool
            }

            Expr::Call(call) => {
                let func_ty = self.infer_expr(call.func);

                let param_types = match &func_ty {
                    Type::Function { params, .. } => Some(params.as_slice()),
                    _ => None,
                };

                for (i, arg) in call.args.iter().enumerate() {
                    let expected =
                        param_types.and_then(|params| params.get(i).map(|(_name, ty)| ty.clone()));
                    self.infer_expr_with_expected(arg, expected);
                }

                for keyword in call.keywords {
                    let _kwarg_ty = self.infer_expr(&keyword.value);
                }

                match func_ty {
                    Type::Function { returns, .. } => *returns,
                    Type::Class(class_name) => Type::Instance(class_name),
                    _ => Type::Unknown,
                }
            }

            Expr::Attribute(attr) => {
                let value_ty = self.infer_expr(attr.value);
                self.resolve_attribute_type(&value_ty, attr.attr)
            }

            Expr::Subscript(subscript) => {
                let value_ty = self.infer_expr(subscript.value);

                let _slice_ty = self.infer_expr(subscript.slice);

                match value_ty {
                    Type::List(elem_ty) => *elem_ty,
                    Type::Dict(_, val_ty) => *val_ty,
                    Type::Tuple(types) => {
                        if let Expr::Constant(constant) = subscript.slice
                            && let Ok(index) = constant.value.parse::<usize>()
                            && index < types.len()
                        {
                            return types[index].clone();
                        }

                        if let Expr::Constant(constant) = subscript.slice
                            && let Ok(index) = constant.value.parse::<isize>()
                            && index < 0
                        {
                            let positive_index = types.len() as isize + index;
                            if positive_index >= 0 && (positive_index as usize) < types.len() {
                                return types[positive_index as usize].clone();
                            }
                        }

                        if types.is_empty() {
                            Type::Unknown
                        } else if types.iter().all(|t| t == &types[0]) {
                            types[0].clone()
                        } else {
                            Type::Union(types)
                        }
                    }
                    Type::Str => Type::Str, // String indexing returns single-char string
                    Type::Bytes => Type::Int, // Bytes indexing returns int
                    _ => Type::Unknown,
                }
            }

            Expr::Lambda(lambda) => {
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Function,
                    "<lambda>".to_string(),
                );

                let expected_params =
                    if let Some(Type::Function { params, .. }) = self.context.expected_type() {
                        Some(params.clone())
                    } else {
                        None
                    };

                let mut param_types = Vec::new();

                for (i, arg) in lambda.args.args.iter().enumerate() {
                    let param_type = if let Some(annotation) = &arg.annotation {
                        parse_annotation(annotation)
                    } else if let Some((_name, expected_ty)) =
                        expected_params.as_ref().and_then(|p| p.get(i))
                    {
                        expected_ty.clone()
                    } else {
                        Type::Unknown
                    };

                    param_types.push(param_type.clone());
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(arg.arg, param_type);
                }

                for arg in lambda.args.posonlyargs {
                    let param_type = arg
                        .annotation
                        .as_ref()
                        .map(|ann| parse_annotation(ann))
                        .unwrap_or(Type::Unknown);
                    param_types.push(param_type.clone());
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(arg.arg, param_type);
                }

                for arg in lambda.args.kwonlyargs {
                    let param_type = arg
                        .annotation
                        .as_ref()
                        .map(|ann| parse_annotation(ann))
                        .unwrap_or(Type::Unknown);
                    param_types.push(param_type.clone());
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(arg.arg, param_type);
                }

                let return_ty = self.infer_expr(lambda.body);

                self.context.symbol_table_mut().analyze_closures();

                let mut captures = Vec::new();
                {
                    let current_scope = self.context.symbol_table().current_scope();
                    let symbols = current_scope.symbols.read().unwrap();
                    for (name, symbol) in symbols.iter() {
                        if symbol.is_free_var
                            && let Some(ty) = symbol.get_type()
                        {
                            captures.push((name.clone(), ty.clone()));
                        }
                    }
                }

                self.context.symbol_table_mut().pop_scope();

                Type::function_with_captures(param_types, return_ty, captures)
            }

            Expr::IfExp(ifexp) => {
                let _test_ty = self.infer_expr(ifexp.test);
                let body_ty = self.infer_expr(ifexp.body);
                let else_ty = self.infer_expr(ifexp.orelse);

                if body_ty == else_ty {
                    body_ty
                } else {
                    Type::union(vec![body_ty, else_ty])
                }
            }

            Expr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    let value_ty = self.infer_expr(value);

                    self.context
                        .mark_current_function_as_generator(value_ty.clone());
                    Type::generator(value_ty)
                } else {
                    self.context.mark_current_function_as_generator(Type::None);
                    Type::generator(Type::None)
                }
            }

            Expr::YieldFrom(yield_from) => {
                let iter_ty = self.infer_expr(yield_from.value);

                let elem_ty = Self::infer_iterable_element_type(iter_ty);
                self.context
                    .mark_current_function_as_generator(elem_ty.clone());
                Type::generator(elem_ty)
            }

            Expr::TString(_) => Type::Str,
            Expr::JoinedStr(_) => Type::Str,
            Expr::FormattedValue(_) => Type::Str,

            Expr::Starred(starred) => self.infer_expr(starred.value),

            Expr::Await(await_expr) => {
                let awaited_type = self.infer_expr(await_expr.value);

                match awaited_type {
                    Type::Coroutine(inner) => *inner,

                    Type::Instance(ref name)
                        if name == "Future" || name == "Task" || name == "Coroutine" =>
                    {
                        Type::Unknown
                    }

                    Type::Generic { base, params } if matches!(*base, Type::Instance(ref n) if n == "Future" || n == "Task" || n == "Coroutine") => {
                        params.first().cloned().unwrap_or(Type::Unknown)
                    }

                    Type::Unknown => Type::Unknown,

                    other => other,
                }
            }

            Expr::Slice(_) => Type::Unknown, // Slices don't have a direct type

            Expr::NamedExpr(named) => self.infer_expr(named.value),

            _ => Type::Unknown,
        };

        let span = expr.span();
        self.context
            .set_type_by_span(span.start().into(), span.end().into(), ty.clone());

        ty
    }

    /// Infer type from binary operation
    fn infer_binop(&mut self, binop: &BinOpExpr) -> Type {
        let left_ty = self.infer_expr(binop.left);
        let right_ty = self.infer_expr(binop.right);

        match binop.op {
            "+" => {
                if left_ty == Type::Str && right_ty == Type::Str {
                    return Type::Str;
                }

                Self::infer_numeric_binop(&left_ty, &right_ty, false)
            }
            "/" => Type::Float,
            "-" | "*" | "//" | "%" | "**" => Self::infer_numeric_binop(&left_ty, &right_ty, false),
            "|" | "^" | "&" | "<<" | ">>" => match (&left_ty, &right_ty) {
                (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => Type::Int,
                (Type::Bool, Type::Int) | (Type::Int, Type::Bool) => Type::Int,

                (Type::Any, _) | (_, Type::Any) => Type::Any,
                _ => Type::Unknown,
            },
            "@" => match (&left_ty, &right_ty) {
                (Type::Any, _) | (_, Type::Any) => Type::Any,

                (Type::List(_), Type::List(_)) => left_ty,
                _ => Type::Unknown,
            },
            _ => Type::Unknown,
        }
    }

    /// Helper to infer numeric binary operation types
    fn infer_numeric_binop(left_ty: &Type, right_ty: &Type, is_div: bool) -> Type {
        match (left_ty, right_ty) {
            (Type::Complex, _) | (_, Type::Complex) => Type::Complex,
            (Type::Float, _) | (_, Type::Float) => Type::Float,
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Int)
            | (Type::Int, Type::Bool)
            | (Type::Bool, Type::Bool) => {
                if is_div {
                    Type::Float // Division always returns float
                } else {
                    Type::Int
                }
            }

            (Type::Any, _) | (_, Type::Any) => Type::Any,

            (Type::Union(types), other) | (other, Type::Union(types)) => {
                let all_numeric = types.iter().all(|t| {
                    matches!(
                        t,
                        Type::Int | Type::Float | Type::Complex | Type::Bool | Type::Any
                    )
                });
                if all_numeric {
                    if types.iter().any(|t| matches!(t, Type::Complex)) {
                        Type::Complex
                    } else if types.iter().any(|t| matches!(t, Type::Float)) {
                        Type::Float
                    } else {
                        Type::Int
                    }
                } else if matches!(
                    other,
                    Type::Int | Type::Float | Type::Complex | Type::Bool | Type::Any
                ) {
                    Type::Union(types.clone())
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown,
        }
    }

    /// Infer type from unary operation
    fn infer_unaryop(&mut self, unary: &UnaryOpExpr) -> Type {
        let operand_ty = self.infer_expr(unary.operand);

        match unary.op {
            "not" => Type::Bool,
            "+" | "-" => match &operand_ty {
                Type::Int | Type::Float | Type::Complex | Type::Bool => operand_ty,
                Type::Any => Type::Any,
                _ => Type::Unknown,
            },
            "~" => match &operand_ty {
                Type::Int | Type::Bool => Type::Int,
                Type::Any => Type::Any,
                _ => Type::Unknown,
            },
            _ => Type::Unknown,
        }
    }

    /// Build a function type from argument list and return type
    fn build_function_type_from_args(
        &mut self,
        args: &crate::ast::Arguments,
        return_ty: Type,
    ) -> Type {
        let mut params: Vec<(Option<String>, Type)> = Vec::new();

        for arg in args.posonlyargs {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);

            params.push((Some(arg.arg.to_string()), ty.clone()));

            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        for arg in args.args {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);

            params.push((Some(arg.arg.to_string()), ty.clone()));

            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        for arg in args.kwonlyargs {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);

            params.push((Some(arg.arg.to_string()), ty.clone()));

            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        if let Some(vararg) = &args.vararg {
            let ty = vararg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);
            let tuple_type = Type::tuple(vec![ty]);
            self.context
                .symbol_table_mut()
                .set_symbol_type(vararg.arg, tuple_type);
        }

        if let Some(kwarg) = &args.kwarg {
            let ty = kwarg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);
            let dict_type = Type::dict(Type::Str, ty);
            self.context
                .symbol_table_mut()
                .set_symbol_type(kwarg.arg, dict_type);
        }

        Type::function_with_names(params, return_ty)
    }

    /// Infer types from match patterns
    fn infer_pattern_types(&mut self, pattern: &crate::ast::patterns::Pattern, subject_ty: &Type) {
        use crate::ast::patterns::Pattern;

        match pattern {
            Pattern::MatchAs(match_as) => {
                if let Some(name) = match_as.name {
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(name, subject_ty.clone());
                }

                if let Some(ref inner_pattern) = match_as.pattern {
                    self.infer_pattern_types(inner_pattern, subject_ty);
                }
            }
            Pattern::MatchValue(_) => {}
            Pattern::MatchOr(match_or) => {
                for alt_pattern in match_or.patterns {
                    self.infer_pattern_types(alt_pattern, subject_ty);
                }
            }
            Pattern::MatchSequence(match_seq) => {
                let elem_ty = match subject_ty {
                    Type::List(elem) => (**elem).clone(),
                    Type::Tuple(elems) => {
                        if elems.len() == match_seq.patterns.len() {
                            for (pat, ty) in match_seq.patterns.iter().zip(elems.iter()) {
                                self.infer_pattern_types(pat, ty);
                            }
                            return;
                        } else if elems.is_empty() {
                            Type::Unknown
                        } else {
                            Type::Union(elems.clone())
                        }
                    }
                    Type::Set(elem) => (**elem).clone(),
                    _ => Self::infer_iterable_element_type(subject_ty.clone()),
                };

                for pat in match_seq.patterns {
                    self.infer_pattern_types(pat, &elem_ty);
                }
            }
            Pattern::MatchMapping(match_map) => {
                let val_ty = match subject_ty {
                    Type::Dict(_, val) => (**val).clone(),
                    _ => Type::Unknown,
                };

                for pat in match_map.patterns {
                    self.infer_pattern_types(pat, &val_ty);
                }

                if let Some(rest_name) = match_map.rest {
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(rest_name, subject_ty.clone());
                }
            }
            Pattern::MatchClass(match_class) => {
                let _class_name = match &match_class.cls {
                    crate::ast::Expr::Name(name) => name.id,
                    _ => "",
                };

                for pat in match_class.patterns {
                    self.infer_pattern_types(pat, &Type::Unknown);
                }
                for pat in match_class.kwd_patterns {
                    self.infer_pattern_types(pat, &Type::Unknown);
                }
            }
            Pattern::MatchSingleton(_singleton) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::symbol::SymbolTable;
    use crate::{Arena, Lexer, Parser};

    fn infer_types(source: &str) -> TypeInferenceContext {
        let arena = Arena::new();
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, &arena);
        let module = parser.parse_module().expect("Parse failed");

        let symbol_table = SymbolTable::new();
        let mut context = TypeInferenceContext::new(symbol_table);
        let mut inference = TypeInference::new(&mut context);
        inference.infer_module(module);

        context
    }

    #[test]
    fn test_literal_inference() {
        let source = r#"
x = 42
y = 3.14
z = "hello"
b = True
n = None
"#;
        let context = infer_types(source);

        assert!(!context.expr_types.is_empty(), "Should have inferred types");

        assert!(
            context.expr_types.len() >= 5,
            "Should have inferred at least 5 literal types, got {}",
            context.expr_types.len()
        );

        let has_int = context.expr_types.values().any(|t| matches!(t, Type::Int));
        let has_float = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::Float));
        let has_str = context.expr_types.values().any(|t| matches!(t, Type::Str));
        let has_bool = context.expr_types.values().any(|t| matches!(t, Type::Bool));
        let has_none = context.expr_types.values().any(|t| matches!(t, Type::None));

        assert!(has_int, "Should have inferred Int type");
        assert!(has_float, "Should have inferred Float type");
        assert!(has_str, "Should have inferred Str type");
        assert!(has_bool, "Should have inferred Bool type");
        assert!(has_none, "Should have inferred None type");
    }

    #[test]
    fn test_collection_inference() {
        let source = r#"
lst = [1, 2, 3]
tup = (1, "a", True)
st = {1, 2, 3}
dct = {"a": 1, "b": 2}
"#;
        let context = infer_types(source);

        assert!(!context.expr_types.is_empty(), "Should have inferred types");

        let has_list = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::List(_)));
        let has_tuple = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::Tuple(_)));
        let has_set = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::Set(_)));
        let has_dict = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::Dict(_, _)));

        assert!(has_list, "Should have inferred List type");
        assert!(has_tuple, "Should have inferred Tuple type");
        assert!(has_set, "Should have inferred Set type");
        assert!(has_dict, "Should have inferred Dict type");
    }

    #[test]
    fn test_binop_inference() {
        let source = r#"
a = 1 + 2
b = 1.0 + 2
c = "hello" + "world"
"#;
        let context = infer_types(source);

        assert!(!context.expr_types.is_empty(), "Should have inferred types");

        let has_int = context.expr_types.values().any(|t| matches!(t, Type::Int));
        let has_float = context
            .expr_types
            .values()
            .any(|t| matches!(t, Type::Float));
        let has_str = context.expr_types.values().any(|t| matches!(t, Type::Str));

        assert!(has_int, "Should have inferred Int from 1 + 2");
        assert!(has_float, "Should have inferred Float from 1.0 + 2");
        assert!(
            has_str,
            "Should have inferred Str from string concatenation"
        );
    }

    #[test]
    fn test_generator_type() {
        let source = r#"
gen = (x * 2 for x in [1, 2, 3])
"#;
        let _context = infer_types(source);
    }

    #[test]
    fn test_nested_generator_comprehension() {
        let source = r#"
result = [x for gen in [(y for y in [1,2,3])] for x in gen]
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_call_site_propagation() {
        let source = r#"
def takes_int(x: int) -> int:
    return x + 1

result = takes_int(42)
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_lambda_basic_inference() {
        let source = r#"
f = lambda x: x + 1
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_lambda_with_annotation() {
        let source = r#"
g = lambda x: int: x * 2
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_lambda_from_call_site() {
        let source = r#"
def apply(f, x: int) -> int:
    return f(x)

result = apply(lambda x: x * 2, 5)
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_lambda_closure_capture() {
        let source = r#"
def make_adder(n: int):
    return lambda x: x + n

add_five = make_adder(5)
"#;
        let _ctx = infer_types(source);
    }

    #[test]
    fn test_builtin_attribute_resolution() {
        let source = r#"
s = "hello"
upper_s = s.upper()
len_s = s.len()

lst = [1, 2, 3]
appended = lst.append(4)

d = {"a": 1}
keys = d.keys()
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_attribute_type_inference() {
        let source = r#"
s = "hello"
result = s.upper()
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_list_attribute_inference() {
        let source = r#"
lst = [1, 2, 3]
count = lst.count(1)
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_dict_attribute_inference() {
        let source = r#"
d = {"a": 1, "b": 2}
keys = d.keys()
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_simple_class_definition() {
        let source = r#"
class Person:
    name: str = "Alice"
    age: int = 30
"#;
        let ctx = infer_types(source);

        let _ = ctx; // Just ensure it parses without panicking
    }

    #[test]
    fn test_class_with_constructor() {
        let source = r#"
class Person:
    def constructor(self, name: str, age: int):
        self.name = name
        self.age = age
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_class_with_methods() {
        let source = r#"
class Calculator:
    def add(self, a: int, b: int) -> int:
        return a + b

    def multiply(self, a: int, b: int) -> int:
        return a * b
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_class_attributes_vs_instance_attributes() {
        let source = r#"
class Counter:
    count = 0  # Class attribute

    def constructor(self):
        self.value = 0  # Instance attribute
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_static_method_decorator() {
        let source = r#"
class Math:
    @staticmethod
    def add(a: int, b: int) -> int:
        return a + b
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_class_method_decorator() {
        let source = r#"
class Factory:
    @classmethod
    def create(cls):
        return cls()
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_property_decorator() {
        let source = r#"
class Circle:
    def constructor(self, radius: float):
        self._radius = radius

    @property
    def radius(self) -> float:
        return self._radius
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_property_with_setter() {
        let source = r#"
class Circle:
    def constructor(self, radius: float):
        self._radius = radius

    @property
    def radius(self) -> float:
        return self._radius

    @radius.setter
    def radius(self, value: float):
        self._radius = value
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_inheritance() {
        let source = r#"
class Animal:
    def speak(self):
        pass

class Dog(Animal):
    def bark(self):
        pass
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_multiple_inheritance() {
        let source = r#"
class A:
    def method_a(self):
        pass

class B:
    def method_b(self):
        pass

class C(A, B):
    def method_c(self):
        pass
"#;
        let ctx = infer_types(source);

        let _ = ctx;
    }

    #[test]
    fn test_builtin_attribute_on_str() {
        let source = r#"
s = "hello"
upper_result = s.upper()
length = len(s)
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_chained_attributes() {
        let source = r#"
class Address:
    def constructor(self):
        self.city = "NYC"

class Person:
    def constructor(self):
        self.address = Address()

person = Person()
city = person.address.city
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_builtin_list_methods() {
        let source = r#"
lst = [1, 2, 3]
lst.append(4)
lst.pop()
lst.clear()
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }

    #[test]
    fn test_builtin_dict_methods() {
        let source = r#"
d = {"a": 1, "b": 2}
keys = d.keys()
values = d.values()
items = d.items()
d.clear()
"#;
        let ctx = infer_types(source);

        assert!(!ctx.expr_types.is_empty());
    }
}
