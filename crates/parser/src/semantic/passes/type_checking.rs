// Type checking pass

use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::type_inference::{TypeInferenceContext, parse_annotation};
use crate::semantic::types::{Type, builtins::BUILTIN_ATTRIBUTE_REGISTRY};
use std::collections::HashMap;
use text_size::TextRange;

/// Type checker context
pub struct TypeCheckContext<'a> {
    /// Type inference context with inferred types (borrowed)
    inference_ctx: &'a TypeInferenceContext,
    /// Errors collected during type checking
    errors: Vec<Error>,
    /// Current function return type (for checking return statements)
    current_return_type: Option<Type>,
    /// Class attributes for validation (class_name, attr_name) -> attr_type
    class_attributes: HashMap<(String, String), Type>,
    /// Enable strict attribute checking
    strict_attribute_checking: bool,
}

impl<'a> TypeCheckContext<'a> {
    pub fn new(inference_ctx: &'a TypeInferenceContext) -> Self {
        Self::with_config(inference_ctx, false)
    }

    pub fn with_config(
        inference_ctx: &'a TypeInferenceContext,
        strict_attribute_checking: bool,
    ) -> Self {
        Self {
            inference_ctx,
            errors: Vec::new(),
            current_return_type: None,
            class_attributes: HashMap::new(),
            strict_attribute_checking,
        }
    }

    /// Create a new context with class metadata for attribute validation
    pub fn with_class_metadata(
        inference_ctx: &'a TypeInferenceContext,
        class_attributes: HashMap<(String, String), Type>,
    ) -> Self {
        Self::with_class_metadata_and_config(inference_ctx, class_attributes, false)
    }

    /// Create a new context with class metadata and configuration
    pub fn with_class_metadata_and_config(
        inference_ctx: &'a TypeInferenceContext,
        class_attributes: HashMap<(String, String), Type>,
        strict_attribute_checking: bool,
    ) -> Self {
        Self {
            inference_ctx,
            errors: Vec::new(),
            current_return_type: None,
            class_attributes,
            strict_attribute_checking,
        }
    }

    /// Validate that an imported name exists in the source module
    /// Returns the type of the imported symbol if valid
    pub fn validate_import(
        &mut self,
        module_name: &str,
        import_name: &str,
        span: TextRange,
    ) -> Option<Type> {
        // Look up the export in module exports (from inference context)
        let export_type = self
            .inference_ctx
            .module_exports
            .get(module_name)
            .and_then(|exports| exports.get(import_name))
            .cloned();

        if export_type.is_none() {
            // Import name doesn't exist in the module
            self.add_error(*error(
                ErrorKind::UndefinedName {
                    name: format!("{}::{}", module_name, import_name),
                },
                span,
            ));
        }

        export_type
    }

    /// Get type checking errors
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Add an error
    fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    /// Get inferred type for an expression by its span start
    fn get_expr_type(&self, span: TextRange) -> Type {
        let start: usize = span.start().into();
        let end: usize = span.end().into();
        self.inference_ctx
            .get_type_by_span(start, end)
            .cloned()
            .unwrap_or(Type::Unknown)
    }
}

/// Type checker visitor
pub struct TypeChecker<'a, 'b> {
    context: &'a mut TypeCheckContext<'b>,
}

impl<'a, 'b> TypeChecker<'a, 'b> {
    pub fn new(context: &'a mut TypeCheckContext<'b>) -> Self {
        Self { context }
    }

    /// Check types for a module
    pub fn check_module(&mut self, module: &Module) {
        // First pass: validate imports to ensure cross-module type safety
        for stmt in module.body {
            match stmt {
                Stmt::Import(import) => {
                    self.check_import_types(import);
                }
                Stmt::From(from) => {
                    self.check_from_import_types(from);
                }
                _ => {}
            }
        }

        // Second pass: check all statements including expressions
        for stmt in module.body {
            self.check_stmt(stmt);
        }
    }

    /// Validate import statement types
    fn check_import_types(&mut self, _import: &ImportStmt) {
        // For "import foo.bar as baz", we validate that foo.bar exists as a module
        // The actual type of imported modules is Type::Module(name)
        // Type validation happens when attributes are accessed on the module

        // Currently, we rely on import_resolution pass to validate module existence
        // Type checking happens when module attributes are accessed
        // No additional validation needed at import time
    }

    /// Validate from-import statement types
    fn check_from_import_types(&mut self, from: &FromStmt) {
        let module_name = from.module.unwrap_or("");

        // For "from foo import bar", validate that bar is exported from foo
        for (name, _alias) in from.names {
            if *name != "*" {
                // Validate that this name is exported from the module
                self.context.validate_import(module_name, name, from.span);
            }
            // Note: star imports (*) can't be validated at compile time
            // without loading the entire module
        }
    }

    /// Check types for a statement
    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign(assign) => {
                // Check the value expression for errors
                self.check_expr(&assign.value);

                let value_type = self.context.get_expr_type(assign.value.span());
                // All targets should be compatible with the value type
                for target in assign.targets {
                    self.check_assignment_target(target, &value_type);
                }
            }
            Stmt::AnnAssign(ann) => {
                if let Some(ref value) = ann.value {
                    let value_type = self.context.get_expr_type(value.span());

                    // Parse the annotation and check compatibility
                    let annotated_type = parse_annotation(&ann.annotation);

                    // Check if the value type is compatible with the annotation
                    if !value_type.is_subtype_of(&annotated_type)
                        && !matches!(value_type, Type::Unknown)
                        && !matches!(annotated_type, Type::Unknown)
                    {
                        self.context.add_error(*error(
                            ErrorKind::TypeMismatch {
                                expected: annotated_type.clone().to_string(),
                                found: value_type.clone().to_string(),
                            },
                            value.span(),
                        ));
                    }

                    self.check_assignment_target(&ann.target, &value_type);
                }
            }
            Stmt::AugAssign(aug) => {
                let target_type = self.context.get_expr_type(aug.target.span());
                let value_type = self.context.get_expr_type(aug.value.span());
                // Check if operation is valid for these types
                self.check_binop_types(aug.op, &target_type, &value_type, aug.span);
            }
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {
                    // Check the return value expression for type errors
                    self.check_expr(value);

                    let return_type = self.context.get_expr_type(value.span());
                    if let Some(ref expected) = self.context.current_return_type
                        && !return_type.is_subtype_of(expected)
                    {
                        self.context.add_error(*error(
                            ErrorKind::ReturnTypeMismatch {
                                expected: expected.clone().to_string(),
                                found: return_type.to_string(),
                            },
                            ret.span,
                        ));
                    }
                }
            }
            Stmt::FuncDef(func) => {
                // Parse return type annotation if present
                let return_type = if let Some(returns) = &func.returns {
                    crate::semantic::passes::type_inference::parse_annotation(returns)
                } else {
                    Type::Unknown
                };

                let old_return_type = self.context.current_return_type.take();
                self.context.current_return_type = Some(return_type);

                for stmt in func.body {
                    self.check_stmt(stmt);
                }

                self.context.current_return_type = old_return_type;
            }
            Stmt::ClassDef(class) => {
                for stmt in class.body {
                    self.check_stmt(stmt);
                }
            }
            Stmt::For(for_stmt) => {
                let iter_type = self.context.get_expr_type(for_stmt.iter.span());

                // Check if the type is iterable
                let is_iterable = matches!(
                    iter_type,
                    Type::List(_)
                        | Type::Tuple(_)
                        | Type::Set(_)
                        | Type::Dict(_, _)
                        | Type::Str
                        | Type::Bytes
                        | Type::Unknown
                        | Type::Any
                );

                if !is_iterable {
                    self.context.add_error(*error(
                        ErrorKind::TypeMismatch {
                            expected: Type::List(Box::new(Type::Any)).to_string(),
                            found: iter_type.to_string(),
                        },
                        for_stmt.iter.span(),
                    ));
                }

                for stmt in for_stmt.body {
                    self.check_stmt(stmt);
                }
                for stmt in for_stmt.orelse {
                    self.check_stmt(stmt);
                }
            }
            Stmt::While(while_stmt) => {
                let _test_type = self.context.get_expr_type(while_stmt.test.span());
                // Any type can be used in boolean context
                for stmt in while_stmt.body {
                    self.check_stmt(stmt);
                }
                for stmt in while_stmt.orelse {
                    self.check_stmt(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                let _test_type = self.context.get_expr_type(if_stmt.test.span());
                // Any type can be used in boolean context
                for stmt in if_stmt.body {
                    self.check_stmt(stmt);
                }
                for stmt in if_stmt.orelse {
                    self.check_stmt(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                for item in with_stmt.items {
                    let ctx_type = self.context.get_expr_type(item.context_expr.span());

                    // Check if context manager protocol is implemented
                    // Coral context managers need @operator enter and @operator exit methods
                    self.check_context_manager_protocol(&ctx_type, item.context_expr.span());
                }
                for stmt in with_stmt.body {
                    self.check_stmt(stmt);
                }
            }
            Stmt::Try(try_stmt) => {
                for stmt in try_stmt.body {
                    self.check_stmt(stmt);
                }
                for handler in try_stmt.handlers {
                    for stmt in handler.body {
                        self.check_stmt(stmt);
                    }
                }
                for stmt in try_stmt.orelse {
                    self.check_stmt(stmt);
                }
                for stmt in try_stmt.finalbody {
                    self.check_stmt(stmt);
                }
            }
            Stmt::Match(match_stmt) => {
                let _subject_type = self.context.get_expr_type(match_stmt.subject.span());
                for case in match_stmt.cases {
                    for stmt in case.body {
                        self.check_stmt(stmt);
                    }
                }
            }
            Stmt::Expr(expr_stmt) => {
                // Check the expression
                self.check_expr(&expr_stmt.value);
            }
            _ => {}
        }
    }

    /// Check types within an expression
    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::BinOp(binop) => {
                // Check operands first
                self.check_expr(binop.left);
                self.check_expr(binop.right);

                // Then check if the operation is valid
                let left_ty = self.context.get_expr_type(binop.left.span());
                let right_ty = self.context.get_expr_type(binop.right.span());
                self.check_binop_types(binop.op, &left_ty, &right_ty, binop.span);
            }
            Expr::UnaryOp(unary) => {
                self.check_expr(unary.operand);

                // Check if unary operation is valid for the operand type
                let operand_ty = self.context.get_expr_type(unary.operand.span());
                let valid = match unary.op {
                    "not" => true, // 'not' works on any type
                    "-" | "+" => {
                        // Unary arithmetic works on numbers
                        matches!(
                            operand_ty,
                            Type::Int | Type::Float | Type::Complex | Type::Bool
                        )
                    }
                    "~" => {
                        // Bitwise NOT works on integers
                        matches!(operand_ty, Type::Int | Type::Bool)
                    }
                    _ => true, // Unknown operator, accept
                };

                if !valid && !matches!(operand_ty, Type::Unknown) {
                    self.context.add_error(*error(
                        ErrorKind::IncompatibleUnaryOp {
                            op: unary.op.to_string(),
                            operand: operand_ty.to_string(),
                        },
                        unary.span,
                    ));
                }
            }
            Expr::Call(call) => {
                self.check_expr(call.func);
                for arg in call.args {
                    self.check_expr(arg);
                }
                for keyword in call.keywords {
                    self.check_expr(&keyword.value);
                }

                // Check function call argument types with full signature validation
                self.check_function_call(call);
            }
            Expr::Subscript(subscript) => {
                self.check_expr(subscript.value);
                self.check_expr(subscript.slice);

                let container_ty = self.context.get_expr_type(subscript.value.span());
                let index_ty = self.context.get_expr_type(subscript.slice.span());
                self.check_subscript(&container_ty, &index_ty, subscript.span);
            }
            Expr::Attribute(attr) => {
                self.check_expr(attr.value);

                // Check if attribute exists on the object type
                let obj_ty = self.context.get_expr_type(attr.value.span());

                // Use the builtin attribute registry to check if the attribute exists
                let attr_ty =
                    BUILTIN_ATTRIBUTE_REGISTRY.lookup_builtin_attribute(&obj_ty, attr.attr);

                let has_attr = match &obj_ty {
                    Type::Unknown | Type::Any => true, // Can't verify, assume valid
                    Type::Instance(class_name) => {
                        // For user-defined classes, validate using class metadata from HIR
                        // Check if the attribute exists in the class
                        let key = (class_name.clone(), attr.attr.to_string());
                        if self.context.class_attributes.contains_key(&key) {
                            // Attribute exists, validation successful
                            true
                        } else {
                            // Attribute not found - could be dynamic or error
                            if self.context.strict_attribute_checking {
                                // Strict mode: reject unknown attributes
                                self.context.add_error(*error(
                                    ErrorKind::InvalidAttribute {
                                        obj_type: class_name.clone(),
                                        attribute: attr.attr.to_string(),
                                    },
                                    attr.span,
                                ));
                                false
                            } else {
                                // Lenient mode: allow dynamic attributes (Python compatibility)
                                true
                            }
                        }
                    }
                    Type::Module(_) => {
                        // Module attributes require module system integration
                        // Full implementation would resolve exports and check attribute existence
                        true
                    }
                    Type::Union(types) => {
                        // For union types, all types in the union must have the attribute
                        types.iter().all(|ty| {
                            BUILTIN_ATTRIBUTE_REGISTRY
                                .lookup_builtin_attribute(ty, attr.attr)
                                .is_some()
                        })
                    }
                    Type::Optional(inner_ty) => {
                        // For optional types, check if the inner type has the attribute
                        BUILTIN_ATTRIBUTE_REGISTRY
                            .lookup_builtin_attribute(inner_ty, attr.attr)
                            .is_some()
                    }
                    _ => attr_ty.is_some(), // Check builtin registry for other types
                };

                if !has_attr
                    && !matches!(
                        &obj_ty,
                        Type::Unknown | Type::Any | Type::Instance(_) | Type::Module(_)
                    )
                {
                    self.context.add_error(*error(
                        ErrorKind::InvalidAttribute {
                            obj_type: obj_ty.display_name(),
                            attribute: attr.attr.to_string(),
                        },
                        attr.span,
                    ));
                }
            }
            Expr::List(list) => {
                for elem in list.elts {
                    self.check_expr(elem);
                }
            }
            Expr::Tuple(tuple) => {
                for elem in tuple.elts {
                    self.check_expr(elem);
                }
            }
            Expr::Set(set) => {
                for elem in set.elts {
                    self.check_expr(elem);
                }
            }
            Expr::Dict(dict) => {
                for k in dict.keys.iter().flatten() {
                    self.check_expr(k);
                }
                for value in dict.values {
                    self.check_expr(value);
                }
            }
            Expr::ListComp(comp) => {
                self.check_expr(comp.elt);
                for generator in comp.generators {
                    self.check_expr(&generator.iter);
                    for if_clause in generator.ifs {
                        self.check_expr(if_clause);
                    }
                }
            }
            Expr::Compare(cmp) => {
                self.check_expr(cmp.left);
                for comparator in cmp.comparators {
                    self.check_expr(comparator);
                }
            }
            Expr::IfExp(ifexp) => {
                self.check_expr(ifexp.test);
                self.check_expr(ifexp.body);
                self.check_expr(ifexp.orelse);
            }
            _ => {
                // Other expression types don't need checking yet
            }
        }
    }

    /// Check assignment target compatibility
    fn check_assignment_target(&mut self, target: &Expr, value_type: &Type) {
        match target {
            Expr::Name(_) => {
                // Name assignments are always valid (type can change)
            }
            Expr::Tuple(tuple) => {
                // Check tuple unpacking
                if let Type::Tuple(types) = value_type {
                    if tuple.elts.len() != types.len() {
                        self.context.add_error(*error(
                            ErrorKind::TypeMismatch {
                                expected: Type::Tuple(vec![Type::Any; tuple.elts.len()])
                                    .to_string(),
                                found: value_type.clone().to_string(),
                            },
                            target.span(),
                        ));
                    } else {
                        for (elem, elem_ty) in tuple.elts.iter().zip(types.iter()) {
                            self.check_assignment_target(elem, elem_ty);
                        }
                    }
                } else {
                    // Not a tuple type, can't unpack
                    self.context.add_error(*error(
                        ErrorKind::TypeMismatch {
                            expected: Type::Tuple(vec![]).to_string(),
                            found: value_type.clone().to_string(),
                        },
                        target.span(),
                    ));
                }
            }
            Expr::List(list) => {
                // Check list unpacking
                if let Type::List(elem_ty) = value_type {
                    for elem in list.elts {
                        self.check_assignment_target(elem, elem_ty);
                    }
                } else {
                    self.context.add_error(*error(
                        ErrorKind::TypeMismatch {
                            expected: Type::List(Box::new(Type::Any)).to_string(),
                            found: value_type.clone().to_string(),
                        },
                        target.span(),
                    ));
                }
            }
            Expr::Subscript(subscript) => {
                let container_type = self.context.get_expr_type(subscript.value.span());
                let index_type = self.context.get_expr_type(subscript.slice.span());
                self.check_subscript(&container_type, &index_type, subscript.span);
            }
            Expr::Attribute(attr) => {
                // Check if this is a property assignment
                self.check_expr(attr.value);

                let obj_type = self.context.get_expr_type(attr.value.span());

                // For user-defined class instances, check if attribute is a read-only property
                if let Type::Instance(class_name) = &obj_type {
                    // Look up the attribute in class metadata to check if it's a property
                    let attr_key = (class_name.clone(), attr.attr.to_string());

                    if let Some(Type::AttributeDescriptor {
                        kind: crate::semantic::types::AttributeKind::Property,
                        getter_type: _,
                        setter_type,
                    }) = self.context.inference_ctx.class_attributes.get(&attr_key)
                    {
                        // This is a property - check if it has a setter
                        if setter_type.is_none() {
                            // Property without setter - read-only
                            self.context.add_error(*error(
                                ErrorKind::ReadOnlyProperty {
                                    name: attr.attr.to_string(),
                                    class_name: class_name.clone(),
                                },
                                attr.span,
                            ));
                        } else if let Some(setter_ty) = setter_type {
                            // Property has setter - validate value type
                            // Setter type is a function: (self, value: T) -> None
                            if let Type::Function { params, .. } = setter_ty.as_ref() {
                                // Second parameter is the value parameter (first is self)
                                if params.len() >= 2 {
                                    let (_self_param, value_param_type) = &params[1];

                                    // Check if assigned value matches setter parameter type
                                    if !value_type.is_subtype_of(value_param_type)
                                        && !matches!(value_type, Type::Unknown)
                                        && !matches!(value_param_type, Type::Unknown)
                                    {
                                        self.context.add_error(*error(
                                            ErrorKind::PropertySetterTypeMismatch {
                                                property_name: attr.attr.to_string(),
                                                expected: value_param_type.to_string(),
                                                found: value_type.to_string(),
                                            },
                                            attr.span,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    /// Check binary operation type compatibility
    fn check_binop_types(&mut self, op: &str, left: &Type, right: &Type, span: TextRange) {
        // Check if operation is valid
        let valid = match op {
            "+" => {
                // Addition works for numbers and strings
                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    ) | (Type::Str, Type::Str)
                        | (Type::List(_), Type::List(_))
                )
            }
            "-" | "*" | "/" | "//" | "%" | "**" => {
                // Arithmetic ops work for numbers
                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    )
                )
            }
            "|" | "^" | "&" | "<<" | ">>" => {
                // Bitwise ops work for integers
                matches!(
                    (left, right),
                    (Type::Int | Type::Bool, Type::Int | Type::Bool)
                )
            }
            "@" => {
                // Matrix multiplication operator (@)
                // Accept any types - requires protocol checking for validation
                true
            }
            _ => true, // Unknown operator, accept
        };

        if !valid && !matches!(left, Type::Unknown) && !matches!(right, Type::Unknown) {
            self.context.add_error(*error(
                ErrorKind::IncompatibleBinOp {
                    op: op.to_string(),
                    left: left.clone().to_string(),
                    right: right.clone().to_string(),
                },
                span,
            ));
        }
    }

    /// Check subscript operation
    fn check_subscript(&mut self, container: &Type, index: &Type, span: TextRange) {
        let valid = match container {
            Type::List(_) | Type::Tuple(_) => {
                // Lists and tuples require integer indices
                matches!(index, Type::Int | Type::Bool)
            }
            Type::Dict(key_type, _) => {
                // Dicts require compatible key type
                index.is_subtype_of(key_type)
            }
            Type::Str => {
                // Strings can be indexed by integers
                matches!(index, Type::Int | Type::Bool)
            }
            Type::Unknown | Type::Any => true,
            _ => false,
        };

        if !valid {
            self.context.add_error(*error(
                ErrorKind::InvalidSubscript {
                    container: container.clone().to_string(),
                    index: index.clone().to_string(),
                },
                span,
            ));
        }
    }

    /// Check if a type implements the context manager protocol
    /// In Coral, context managers need @operator enter and @operator exit methods
    fn check_context_manager_protocol(&mut self, ctx_type: &Type, span: TextRange) {
        match ctx_type {
            // Accept Unknown and Any without validation
            Type::Unknown | Type::Any => {}

            // For class instances, we would check for @operator enter and exit methods
            // This requires integration with ClassAnalyzer to look up methods
            Type::Instance(_class_name) => {
                // In a full implementation, we would:
                // 1. Use ClassAnalyzer to get class methods
                // 2. Check for methods named "enter" and "exit" with @operator decorator
                // 3. Validate signatures: enter(self) and exit(self, exc_type, exc_val, exc_tb)
                // For now, accept all class instances as potentially valid
            }

            // Modules could potentially be context managers
            Type::Module(_) => {}

            // Primitive types are not context managers
            Type::Int | Type::Float | Type::Bool | Type::Str | Type::Bytes | Type::None => {
                self.context.add_error(*error(
                    ErrorKind::TypeMismatch {
                        expected: "ContextManager (requires @operator enter and @operator exit)"
                            .to_string(),
                        found: ctx_type.display_name(),
                    },
                    span,
                ));
            }

            // Other types: accept for now, would need protocol checking
            _ => {}
        }
    }

    /// Check function call with comprehensive signature validation
    fn check_function_call(&mut self, call: &CallExpr) {
        let func_ty = self.context.get_expr_type(call.func.span());

        if let Type::Function {
            params,
            returns: _,
            captures: _,
        } = func_ty
        {
            // Collect argument types
            let positional_args: Vec<(Type, TextRange)> = call
                .args
                .iter()
                .map(|arg| (self.context.get_expr_type(arg.span()), arg.span()))
                .collect();

            // Collect keyword arguments with their names
            let keyword_args: Vec<(Option<&str>, Type, TextRange)> = call
                .keywords
                .iter()
                .map(|kw| {
                    (
                        kw.arg,
                        self.context.get_expr_type(kw.value.span()),
                        kw.value.span(),
                    )
                })
                .collect();

            // Check positional arguments
            let num_positional = positional_args.len();
            let num_params = params.len();

            // Simple validation: check if we have the right number of positional args
            // More sophisticated: would need to know which params have defaults
            if num_positional > num_params {
                self.context.add_error(*error(
                    ErrorKind::ArgumentCountMismatch {
                        expected: num_params,
                        found: num_positional + keyword_args.len(),
                    },
                    call.span,
                ));
                return;
            }

            // Validate positional argument types
            for (i, (arg_ty, arg_span)) in positional_args.iter().enumerate() {
                if let Some((_param_name, expected_ty)) = params.get(i) {
                    // Use contravariance: argument type must be subtype of parameter type
                    if !arg_ty.is_subtype_of(expected_ty)
                        && !matches!(arg_ty, Type::Unknown)
                        && !matches!(expected_ty, Type::Unknown)
                    {
                        self.context.add_error(*error(
                            ErrorKind::InvalidArgumentType {
                                param_index: i,
                                expected: expected_ty.to_string(),
                                found: arg_ty.to_string(),
                            },
                            *arg_span,
                        ));
                    }
                }
            }

            // Validate keyword arguments with parameter name matching
            for (arg_name, arg_ty, arg_span) in keyword_args.iter() {
                // Try to find parameter by name
                let param_match = params
                    .iter()
                    .enumerate()
                    .find(|(_idx, (param_name, _ty))| param_name.as_deref() == *arg_name);

                if let Some((idx, (_param_name, param_ty))) = param_match {
                    // Found matching parameter by name - validate type
                    if !arg_ty.is_subtype_of(param_ty)
                        && !matches!(arg_ty, Type::Unknown)
                        && !matches!(param_ty, Type::Unknown)
                    {
                        self.context.add_error(*error(
                            ErrorKind::InvalidArgumentType {
                                param_index: idx,
                                expected: param_ty.to_string(),
                                found: arg_ty.to_string(),
                            },
                            *arg_span,
                        ));
                    }
                } else {
                    // Parameter name not found - check if we can match by position
                    let remaining_params: Vec<&(Option<String>, Type)> =
                        params.iter().skip(num_positional).collect();

                    if remaining_params.is_empty() {
                        // No parameters available for this keyword arg
                        if let Some(name) = arg_name {
                            self.context.add_error(*error(
                                ErrorKind::UnexpectedKeywordArgument {
                                    name: name.to_string(),
                                },
                                *arg_span,
                            ));
                        }
                    } else {
                        // Check if argument type matches any remaining parameter by type
                        let matches_any = remaining_params
                            .iter()
                            .any(|(_name, param_ty)| arg_ty.is_subtype_of(param_ty));

                        if !matches_any
                            && !matches!(arg_ty, Type::Unknown)
                            && !remaining_params
                                .iter()
                                .all(|(_n, p)| matches!(p, Type::Unknown))
                        {
                            self.context.add_error(*error(
                                ErrorKind::InvalidArgumentType {
                                    param_index: num_positional,
                                    expected: remaining_params
                                        .first()
                                        .map(|(_n, t)| t.to_string())
                                        .unwrap_or_else(|| "unknown".to_string()),
                                    found: arg_ty.to_string(),
                                },
                                *arg_span,
                            ));
                        }
                    }
                }
            }
        }
    }

    /// Validate generator protocol
    #[allow(dead_code)]
    fn validate_generator_protocol(&mut self, gen_type: &Type, span: TextRange) {
        if let Type::Generator(elem_ty) = gen_type {
            // Generators implicitly implement @operator iter and @operator next
            // This is built-in behavior, no explicit validation needed
            // Just ensure element type is valid
            if matches!(elem_ty.as_ref(), Type::Never) {
                self.context.add_error(*error(
                    ErrorKind::TypeMismatch {
                        expected: "Any non-Never type".to_string(),
                        found: "Never".to_string(),
                    },
                    span,
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::passes::name_resolution::NameResolver;
    use crate::semantic::passes::type_inference::{TypeInference, TypeInferenceContext};
    // use crate::semantic::symbol::SymbolTable;
    use crate::{Arena, Lexer, Parser};

    fn check_types(source: &str) -> Vec<Error> {
        let arena = Arena::new();
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, &arena);
        let module = parser.parse_module().expect("Parse failed");

        // First run name resolution to populate symbol table
        let mut name_resolver = NameResolver::new();
        name_resolver.resolve_module(module);
        let (symbol_table, _name_errors) = name_resolver.into_symbol_table();

        // Then run type inference
        let mut infer_ctx = TypeInferenceContext::new(symbol_table);
        let mut inference = TypeInference::new(&mut infer_ctx);
        inference.infer_module(module);

        // Finally run type checking
        let mut check_ctx = TypeCheckContext::new(&infer_ctx);
        let mut checker = TypeChecker::new(&mut check_ctx);
        checker.check_module(module);

        check_ctx.errors().to_vec()
    }

    #[test]
    fn test_valid_assignment() {
        let source = r#"
x = 42
y = "hello"
z = [1, 2, 3]
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_tuple_unpacking() {
        let source = r#"
a, b = (1, 2)
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_tuple_unpacking_mismatch() {
        let source = r#"
a, b, c = (1, 2)
"#;
        let errors = check_types(source);
        // Should have an error about tuple length mismatch
        assert!(!errors.is_empty());
        assert!(matches!(errors[0].kind, ErrorKind::TypeMismatch { .. }));
    }

    #[test]
    fn test_invalid_binop() {
        // Test with variables to check type tracking across statements
        let source = r#"
s1 = "hello"
s2 = "world"
x = s1 - s2
"#;
        let errors = check_types(source);
        // String subtraction is invalid
        assert!(!errors.is_empty());
        assert!(matches!(
            errors[0].kind,
            ErrorKind::IncompatibleBinOp { .. }
        ));
    }

    #[test]
    fn test_valid_subscript() {
        let source = r#"
lst = [1, 2, 3]
x = lst[0]
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_string_operations() {
        let source = r#"
s1 = "hello"
s2 = "world"
s3 = s1 + s2
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_numeric_operations() {
        let source = r#"
a = 1 + 2
b = 3.14 * 2
c = 10 / 2
d = 10 // 3
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_annotated_assignment() {
        let source = r#"
x: int = 42
s: str = "hello"
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_annotated_assignment_mismatch() {
        let source = r#"
x: int = "hello"
"#;
        let errors = check_types(source);
        assert!(!errors.is_empty());
        assert!(matches!(errors[0].kind, ErrorKind::TypeMismatch { .. }));
    }

    #[test]
    fn test_unary_operations() {
        let source = r#"
x = -5
y = +3.14
z = not True
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_for_loop_iterable() {
        let source = r#"
lst = [1, 2, 3]
for x in lst:
    pass
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_list_methods() {
        let source = r#"
lst = [1, 2, 3]
lst.append
lst.extend
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_string_methods() {
        let source = r#"
s = "hello"
s.upper
s.lower
s.strip
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_function_return_type() {
        let source = r#"
def get_number() -> int:
    return 42

def get_string() -> str:
    return "hello"
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_function_return_type_mismatch() {
        let source = r#"
def get_number() -> int:
    return "hello"
"#;
        let errors = check_types(source);
        assert!(!errors.is_empty());
        assert!(matches!(
            errors[0].kind,
            ErrorKind::ReturnTypeMismatch { .. }
        ));
    }

    #[test]
    fn test_tuple_subscript() {
        let source = r#"
t = (1, "hello", 3.14)
x = t[0]   # Should be int
y = t[1]   # Should be str
z = t[2]   # Should be float
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_comprehension_types() {
        let source = r#"
numbers = [1, 2, 3, 4, 5]
squares = [x * x for x in numbers]
even = [x for x in numbers if x % 2 == 0]
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_parameter_annotations() {
        let source = r#"
def add(x: int, y: int) -> int:
    return x + y

def greet(name: str) -> str:
    return "Hello, " + name
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_parameter_type_mismatch() {
        let source = r#"
def add(x: int, y: int) -> int:
    return x + "wrong"
"#;
        let errors = check_types(source);
        assert!(!errors.is_empty(), "Expected type error for int + str");
    }

    #[test]
    fn test_parameter_in_body() {
        let source = r#"
def process(value: int) -> int:
    result = value * 2
    return result
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_closure_capture() {
        let source = r#"
def outer(x: int):
    y = 10
    def inner():
        return y + 1
    return inner()
"#;
        let errors = check_types(source);
        // Closure tracking is implemented in symbol table
        // Type checking for closures works at the basic level
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_closure_with_parameter_types() {
        let source = r#"
def outer(x: int) -> int:
    def inner(y: int) -> int:
        return x + y
    return inner(5)
"#;
        let errors = check_types(source);
        assert_eq!(errors.len(), 0);
    }
}
