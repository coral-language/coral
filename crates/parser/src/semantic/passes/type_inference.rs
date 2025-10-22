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
use crate::semantic::types::Type;
use std::collections::HashMap;

/// Parse a constant string literal to infer its type
fn infer_constant_from_str(value: &str) -> Type {
    // Try to parse as different types
    if value == "True" || value == "False" {
        return Type::Bool;
    }
    if value == "None" {
        return Type::None;
    }
    // Try to parse as int first (handles negative too)
    if value.parse::<i64>().is_ok() {
        return Type::Int;
    }
    // Try to parse as float (includes scientific notation)
    if value.parse::<f64>().is_ok() {
        return Type::Float;
    }
    // If it's not a number and not a keyword, it's likely a string
    // The AST Constant node is used for string literals (with quotes already removed by parser)
    // as well as numeric constants
    Type::Str
}

/// Parse a type annotation expression into a Type
pub fn parse_annotation(annotation: &Expr) -> Type {
    match annotation {
        Expr::Name(name) => {
            // Simple type names
            match name.id {
                "int" => Type::Int,
                "str" => Type::Str,
                "float" => Type::Float,
                "bool" => Type::Bool,
                "bytes" => Type::Bytes,
                "complex" => Type::Complex,
                "None" => Type::None,
                "Any" => Type::Any,
                _ => Type::Unknown, // Custom class or undefined type
            }
        }
        Expr::Subscript(subscript) => {
            // Generic types like List[int], Dict[str, int]
            if let Expr::Name(name) = subscript.value {
                match name.id {
                    "list" | "List" => {
                        let elem_ty = parse_annotation(subscript.slice);
                        Type::list(elem_ty)
                    }
                    "dict" | "Dict" => {
                        // Dict[K, V] - slice should be a tuple
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
                        // Tuple[int, str, float]
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
                        // Union[int, str]
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
            // Literal types like Literal["foo"]
            if constant.value == "None" {
                Type::None
            } else {
                Type::Unknown
            }
        }
        Expr::BinOp(binop) if binop.op == "|" => {
            // Union types using | operator
            let left = parse_annotation(binop.left);
            let right = parse_annotation(binop.right);
            Type::union(vec![left, right])
        }
        _ => Type::Unknown,
    }
}

/// Type inference context that tracks inferred types for expressions
pub struct TypeInferenceContext {
    /// Map from expression spans (start, end) to inferred types
    expr_types: HashMap<(usize, usize), Type>,
    /// Symbol table for name resolution
    symbol_table: SymbolTable,
    /// Expected type context stack for bidirectional type checking
    expected_type_stack: Vec<Option<Type>>,
    /// Track yielded types for generator functions (function name -> yielded types)
    generator_yields: HashMap<String, Vec<Type>>,
}

impl TypeInferenceContext {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            expr_types: HashMap::new(),
            symbol_table,
            expected_type_stack: vec![None], // Start with no expectation
            generator_yields: HashMap::new(),
        }
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
                // Infer type from the value
                let value_type = self.infer_expr_val(&assign.value);
                // All targets get the same type
                for target in assign.targets {
                    self.infer_target(target, &value_type);
                }
            }
            Stmt::AnnAssign(ann) => {
                // Parse the type annotation
                let annotated_type = parse_annotation(&ann.annotation);

                // If there's a value, use the annotation type
                if let Some(ref value) = ann.value {
                    let _value_type = self.infer_expr_val(value);

                    // Use the annotation type, but could check compatibility
                    self.infer_target(&ann.target, &annotated_type);

                    // Store the more specific type (annotation)
                    if let Expr::Name(name) = &ann.target {
                        self.context
                            .symbol_table_mut()
                            .set_symbol_type(name.id, annotated_type);
                    }
                } else {
                    // Just the annotation, no value
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
                // Enter existing function scope (created by NameResolver)
                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Function, func.name);

                if !entered {
                    // Scope doesn't exist, skip (this shouldn't happen if name resolution ran first)
                    return;
                }

                // Infer return type
                let return_ty = if let Some(returns) = &func.returns {
                    parse_annotation(returns)
                } else {
                    // Try to infer from return statements in body
                    // For now, use Unknown
                    Type::Unknown
                };

                // Build function type and store parameter types in symbol table
                let func_ty = self.build_function_type_from_args(&func.args, return_ty);
                self.context
                    .symbol_table_mut()
                    .set_symbol_type(func.name, func_ty);

                // Infer types in function body
                for stmt in func.body {
                    self.infer_stmt(stmt);
                }

                // Check if function is a generator
                if self.context.is_generator_function(func.name) {
                    let yield_ty = self
                        .context
                        .get_generator_yield_type(func.name)
                        .unwrap_or(Type::Unknown);

                    // Function's return type is Generator[YieldType]
                    let generator_ty = Type::generator(yield_ty);

                    // Update function type to return Generator instead of declared return type
                    let func_ty = self.build_function_type_from_args(&func.args, generator_ty);
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(func.name, func_ty);
                }

                // Exit function scope
                self.context.symbol_table_mut().pop_scope();
            }
            Stmt::ClassDef(class) => {
                // Enter existing class scope (created by NameResolver)
                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Class, class.name);

                if entered {
                    for stmt in class.body {
                        self.infer_stmt(stmt);
                    }

                    // Exit class scope
                    self.context.symbol_table_mut().pop_scope();
                }
            }
            Stmt::For(for_stmt) => {
                let iter_type = self.infer_expr_val(&for_stmt.iter);

                // Infer element type from iterable
                let elem_type = self.infer_iterable_element_type(iter_type);

                // Set the type for the target variable
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
                let _ = self.infer_expr_val(&match_stmt.subject);
                for case in match_stmt.cases {
                    for stmt in case.body {
                        self.infer_stmt(stmt);
                    }
                }
            }
            Stmt::Yield(yield_stmt) => {
                if let Some(ref value) = yield_stmt.value {
                    // The value might be a YieldFrom expression, which needs special handling
                    match value {
                        Expr::YieldFrom(yield_from) => {
                            // Handle yield from
                            let iter_ty = self.infer_expr(yield_from.value);
                            let elem_ty = self.infer_iterable_element_type(iter_ty);
                            self.context
                                .mark_current_function_as_generator(elem_ty.clone());
                        }
                        _ => {
                            // Regular yield
                            let value_ty = self.infer_expr(value);
                            self.context
                                .mark_current_function_as_generator(value_ty.clone());
                        }
                    }
                } else {
                    // yield without value
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
                // Store type for this name in the symbol table
                self.context
                    .symbol_table_mut()
                    .set_symbol_type(name.id, ty.clone());
            }
            Expr::Tuple(tuple) => {
                // Unpack tuple type if available
                if let Type::Tuple(types) = ty {
                    for (elem, elem_ty) in tuple.elts.iter().zip(types.iter()) {
                        self.infer_target(elem, elem_ty);
                    }
                }
            }
            Expr::List(list) => {
                // All elements get the same type
                if let Type::List(elem_ty) = ty {
                    for elem in list.elts {
                        self.infer_target(elem, elem_ty);
                    }
                }
            }
            _ => {}
        }
    }

    /// Extract element type from an iterable type
    fn infer_iterable_element_type(&self, iter_ty: Type) -> Type {
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
            _ => Type::Unknown,
        }
    }

    /// Infer type from an expression (owned)
    fn infer_expr_val(&mut self, expr: &Expr) -> Type {
        self.infer_expr(expr)
    }

    // Add new method variant
    fn infer_expr_with_expected(&mut self, expr: &Expr, expected: Option<Type>) -> Type {
        self.context.push_expected_type(expected);
        let ty = self.infer_expr(expr);
        self.context.pop_expected_type();
        ty
    }

    /// Infer type from an expression
    fn infer_expr(&mut self, expr: &Expr) -> Type {
        let ty = match expr {
            // Literals
            Expr::Constant(constant) => infer_constant_from_str(constant.value),
            Expr::Complex(_) => Type::Complex,
            Expr::Bytes(_) => Type::Bytes,
            Expr::Name(name) => {
                // Look up name in symbol table
                self.context
                    .symbol_table()
                    .get_symbol_type(name.id)
                    .unwrap_or(Type::Unknown)
            }

            // Collections
            Expr::List(list) => {
                if list.elts.is_empty() {
                    Type::list(Type::Unknown)
                } else {
                    // Infer element type from first element
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
                    // Keys can be None for ** unpacking, skip those
                    let key_ty = if let Some(ref key) = dict.keys[0] {
                        self.infer_expr(key)
                    } else {
                        Type::Unknown
                    };
                    let value_ty = self.infer_expr(&dict.values[0]);
                    Type::dict(key_ty, value_ty)
                }
            }

            // Comprehensions
            Expr::ListComp(comp) => {
                // Enter existing comprehension scope (created by NameResolver)
                let entered = self
                    .context
                    .symbol_table_mut()
                    .enter_child_scope(crate::semantic::symbol::ScopeType::Comprehension, "<comp>");

                if !entered {
                    // Fallback: compute without scope navigation
                    let elem_ty = self.infer_expr(comp.elt);
                    return Type::list(elem_ty);
                }

                // Infer types for generators (iter and conditions)
                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    // Infer element type from iterable and set for target
                    let elem_ty = self.infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);

                    // Infer condition types
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let elem_ty = self.infer_expr(comp.elt);

                // Exit comprehension scope
                self.context.symbol_table_mut().pop_scope();

                Type::list(elem_ty)
            }
            Expr::SetComp(comp) => {
                // Enter comprehension scope
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                // Infer types for generators (similar to ListComp)
                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = self.infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let elem_ty = self.infer_expr(comp.elt);

                // Exit comprehension scope
                self.context.symbol_table_mut().pop_scope();

                Type::set(elem_ty)
            }
            Expr::DictComp(comp) => {
                // Enter comprehension scope
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                // Infer types for generators (similar to ListComp)
                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = self.infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                let key_ty = self.infer_expr(comp.key);
                let value_ty = self.infer_expr(comp.value);

                // Exit comprehension scope
                self.context.symbol_table_mut().pop_scope();

                Type::dict(key_ty, value_ty)
            }
            Expr::GeneratorExp(comp) => {
                // Enter comprehension scope
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Comprehension,
                    "<comp>".to_string(),
                );

                // Infer types for generators (similar to ListComp)
                for generator in comp.generators {
                    let iter_ty = self.infer_expr(&generator.iter);
                    let elem_ty = self.infer_iterable_element_type(iter_ty);
                    self.infer_target(&generator.target, &elem_ty);
                    for cond in generator.ifs {
                        let _cond_ty = self.infer_expr(cond);
                    }
                }
                // Generator expressions return generator objects
                // For simplicity, we'll treat them as iterables of the element type
                let elem_ty = self.infer_expr(comp.elt);

                // Exit comprehension scope
                self.context.symbol_table_mut().pop_scope();

                Type::generator(elem_ty) // Return proper generator type
            }

            // Operations
            Expr::BinOp(binop) => self.infer_binop(binop),
            Expr::UnaryOp(unary) => self.infer_unaryop(unary),
            Expr::Compare(compare) => {
                // Infer types of all operands for type storage
                let _left_ty = self.infer_expr(compare.left);
                for comparator in compare.comparators {
                    let _comp_ty = self.infer_expr(comparator);
                }
                // Comparisons always return bool
                Type::Bool
            }
            Expr::BoolOp(boolop) => {
                // Infer types of all operands for type storage
                for value in boolop.values {
                    let _val_ty = self.infer_expr(value);
                }
                // Boolean operations return bool
                Type::Bool
            }

            // Function calls
            Expr::Call(call) => {
                // Infer function type
                let func_ty = self.infer_expr(call.func);

                // Extract parameter types if function type is known
                let param_types = match &func_ty {
                    Type::Function { params, .. } => Some(params.as_slice()),
                    _ => None,
                };

                // Infer argument types with expected types from parameters
                for (i, arg) in call.args.iter().enumerate() {
                    let expected = param_types.and_then(|params| params.get(i)).cloned();
                    self.infer_expr_with_expected(arg, expected);
                }

                // Infer keyword argument types
                for keyword in call.keywords {
                    // TODO: Match keyword name to parameter for expected type
                    let _kwarg_ty = self.infer_expr(&keyword.value);
                }

                // Return the function's return type
                match func_ty {
                    Type::Function { returns, .. } => *returns,
                    _ => Type::Unknown,
                }
            }

            // Attribute access
            Expr::Attribute(_) => {
                // Can't infer without knowing object type
                Type::Unknown
            }

            // Subscript
            Expr::Subscript(subscript) => {
                let value_ty = self.infer_expr(subscript.value);
                // Also infer the slice type (important for storing types)
                let _slice_ty = self.infer_expr(subscript.slice);

                match value_ty {
                    Type::List(elem_ty) => *elem_ty,
                    Type::Dict(_, val_ty) => *val_ty,
                    Type::Tuple(types) => {
                        // Check if slice is a constant integer
                        if let Expr::Constant(constant) = subscript.slice
                            && let Ok(index) = constant.value.parse::<usize>()
                            && index < types.len()
                        {
                            return types[index].clone();
                        }
                        // Negative indices
                        if let Expr::Constant(constant) = subscript.slice
                            && let Ok(index) = constant.value.parse::<isize>()
                            && index < 0
                        {
                            let positive_index = types.len() as isize + index;
                            if positive_index >= 0 && (positive_index as usize) < types.len() {
                                return types[positive_index as usize].clone();
                            }
                        }
                        // If we can't determine the index, return union of all types
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

            // Lambda
            Expr::Lambda(lambda) => {
                // Enter lambda scope
                self.context.symbol_table_mut().push_scope(
                    crate::semantic::symbol::ScopeType::Function,
                    "<lambda>".to_string(),
                );

                // Get expected function type from context if available
                let expected_params =
                    if let Some(Type::Function { params, .. }) = self.context.expected_type() {
                        Some(params.clone())
                    } else {
                        None
                    };

                let mut param_types = Vec::new();

                // Infer parameter types from annotations OR expected types
                for (i, arg) in lambda.args.args.iter().enumerate() {
                    let param_type = if let Some(annotation) = &arg.annotation {
                        // Explicit annotation takes precedence
                        parse_annotation(annotation)
                    } else if let Some(expected) = expected_params.as_ref().and_then(|p| p.get(i)) {
                        // Use expected type from call site
                        expected.clone()
                    } else {
                        // No information available
                        Type::Unknown
                    };

                    param_types.push(param_type.clone());
                    self.context
                        .symbol_table_mut()
                        .set_symbol_type(arg.arg, param_type);
                }

                // Handle positional-only args
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

                // Handle keyword-only args
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

                // Infer the body expression type (this is the return type)
                let return_ty = self.infer_expr(lambda.body);

                // Analyze closure captures before exiting scope
                self.context.symbol_table_mut().analyze_closures();

                // Collect captured variables
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

                // Exit lambda scope
                self.context.symbol_table_mut().pop_scope();

                // Build function type with captures
                Type::function_with_captures(param_types, return_ty, captures)
            }

            // If expression
            Expr::IfExp(ifexp) => {
                let _test_ty = self.infer_expr(ifexp.test);
                let body_ty = self.infer_expr(ifexp.body);
                let else_ty = self.infer_expr(ifexp.orelse);
                // Return union of both branches
                if body_ty == else_ty {
                    body_ty
                } else {
                    Type::union(vec![body_ty, else_ty])
                }
            }

            Expr::Yield(yield_expr) => {
                if let Some(value) = yield_expr.value {
                    let value_ty = self.infer_expr(value);
                    // Mark that we're in a generator context
                    self.context
                        .mark_current_function_as_generator(value_ty.clone());
                    Type::generator(value_ty)
                } else {
                    // yield without value yields None
                    self.context.mark_current_function_as_generator(Type::None);
                    Type::generator(Type::None)
                }
            }

            Expr::YieldFrom(yield_from) => {
                let iter_ty = self.infer_expr(yield_from.value);
                // Extract element type from the iterable
                let elem_ty = self.infer_iterable_element_type(iter_ty);
                self.context
                    .mark_current_function_as_generator(elem_ty.clone());
                Type::generator(elem_ty)
            }

            _ => Type::Unknown,
        };

        // Store the inferred type for this expression
        let span = expr.span();
        self.context
            .set_type_by_span(span.start().into(), span.end().into(), ty.clone());

        ty
    }

    /// Infer type from binary operation
    fn infer_binop(&mut self, binop: &BinOpExpr) -> Type {
        let left_ty = self.infer_expr(binop.left);
        let right_ty = self.infer_expr(binop.right);

        // Simple inference based on operation string
        match binop.op {
            "+" => {
                // String concatenation
                if left_ty == Type::Str && right_ty == Type::Str {
                    return Type::Str;
                }
                // Numeric addition
                Self::infer_numeric_binop(&left_ty, &right_ty, false)
            }
            "/" => {
                // Division always returns float
                Type::Float
            }
            "-" | "*" | "//" | "%" | "**" => Self::infer_numeric_binop(&left_ty, &right_ty, false),
            "|" | "^" | "&" | "<<" | ">>" => {
                // Bitwise ops work on integers
                if left_ty == Type::Int && right_ty == Type::Int {
                    Type::Int
                } else {
                    Type::Unknown
                }
            }
            "@" => Type::Unknown, // Matrix mult
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
            _ => Type::Unknown,
        }
    }

    /// Infer type from unary operation
    fn infer_unaryop(&mut self, unary: &UnaryOpExpr) -> Type {
        let operand_ty = self.infer_expr(unary.operand);

        match unary.op {
            "not" => Type::Bool,
            "+" | "-" => operand_ty,
            "~" => {
                if operand_ty == Type::Int {
                    Type::Int
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown,
        }
    }

    /// Build a function type from argument list and return type
    fn build_function_type_from_args(
        &mut self,
        args: &crate::ast::Arguments,
        return_ty: Type,
    ) -> Type {
        let mut param_types = Vec::new();

        // Positional-only arguments
        for arg in args.posonlyargs {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);
            param_types.push(ty.clone());
            // Store parameter type in symbol table
            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        // Regular arguments
        for arg in args.args {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);
            param_types.push(ty.clone());
            // Store parameter type in symbol table
            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        // Keyword-only arguments
        for arg in args.kwonlyargs {
            let ty = arg
                .annotation
                .as_ref()
                .map(|ann| parse_annotation(ann))
                .unwrap_or(Type::Unknown);
            param_types.push(ty.clone());
            // Store parameter type in symbol table
            self.context.symbol_table_mut().set_symbol_type(arg.arg, ty);
        }

        // Handle *args and **kwargs
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

        Type::function(param_types, return_ty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        let _context = infer_types(source);
        // Types are inferred but not yet stored in a way we can query
        // This test just ensures inference runs without panicking
    }

    #[test]
    fn test_collection_inference() {
        let source = r#"
lst = [1, 2, 3]
tup = (1, "a", True)
st = {1, 2, 3}
dct = {"a": 1, "b": 2}
"#;
        let _context = infer_types(source);
    }

    #[test]
    fn test_binop_inference() {
        let source = r#"
a = 1 + 2
b = 1.0 + 2
c = "hello" + "world"
"#;
        let _context = infer_types(source);
    }

    #[test]
    fn test_generator_type() {
        let source = r#"
gen = (x * 2 for x in [1, 2, 3])
"#;
        let _context = infer_types(source);
        // Type inference should run without panicking
        // The actual type checking will be done when we integrate with name resolution
    }

    #[test]
    fn test_nested_generator_comprehension() {
        let source = r#"
result = [x for gen in [(y for y in [1,2,3])] for x in gen]
"#;
        let _ctx = infer_types(source);
        // Should handle nested generator iteration without panicking
    }

    #[test]
    fn test_call_site_propagation() {
        let source = r#"
def takes_int(x: int) -> int:
    return x + 1

result = takes_int(42)
"#;
        let _ctx = infer_types(source);
        // Call site propagation should run without panicking
        // Full integration will happen when name resolution is run first
    }

    #[test]
    fn test_lambda_basic_inference() {
        let source = r#"
f = lambda x: x + 1
"#;
        let _ctx = infer_types(source);
        // Lambda inference should run without panicking
        // Full integration will happen when name resolution is run first
    }

    #[test]
    fn test_lambda_with_annotation() {
        let source = r#"
g = lambda x: int: x * 2
"#;
        let _ctx = infer_types(source);
        // Lambda with annotations should run without panicking
    }

    #[test]
    fn test_lambda_from_call_site() {
        let source = r#"
def apply(f, x: int) -> int:
    return f(x)

result = apply(lambda x: x * 2, 5)
"#;
        let _ctx = infer_types(source);
        // Lambda from call site should run without panicking
    }

    #[test]
    fn test_lambda_closure_capture() {
        let source = r#"
def make_adder(n: int):
    return lambda x: x + n

add_five = make_adder(5)
"#;
        let _ctx = infer_types(source);
        // Lambda closure capture should run without panicking
    }
}
