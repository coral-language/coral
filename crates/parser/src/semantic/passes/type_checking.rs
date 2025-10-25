

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

        if module_name.is_empty() {
            return None;
        }


        let export_type = self
            .inference_ctx
            .module_exports
            .get(module_name)
            .and_then(|exports| exports.get(import_name))
            .cloned();






        if self.inference_ctx.module_exports.contains_key(module_name) && export_type.is_none() {

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

    /// Get a class attribute by name
    pub fn get_class_attribute(&self, class_name: &str, attr_name: &str) -> Option<Type> {
        self.class_attributes.get(&(class_name.to_string(), attr_name.to_string())).cloned()
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


        for stmt in module.body {
            self.check_stmt(stmt);
        }
    }

    /// Validate import statement types
    fn check_import_types(&mut self, _import: &ImportStmt) {







    }

    /// Validate from-import statement types
    fn check_from_import_types(&mut self, from: &FromStmt) {
        let module_name = from.module.unwrap_or("");


        for (name, _alias) in from.names {
            if *name != "*" {

                self.context.validate_import(module_name, name, from.span);
            }


        }
    }

    /// Check types for a statement
    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign(assign) => {

                self.check_expr(&assign.value);

                let value_type = self.context.get_expr_type(assign.value.span());

                for target in assign.targets {
                    self.check_assignment_target(target, &value_type);
                }
            }
            Stmt::AnnAssign(ann) => {
                if let Some(ref value) = ann.value {
                    let value_type = self.context.get_expr_type(value.span());


                    let annotated_type = parse_annotation(&ann.annotation);


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

                self.check_binop_types(aug.op, &target_type, &value_type, aug.span);
            }
            Stmt::Return(ret) => {
                if let Some(ref value) = ret.value {

                    self.check_expr(value);

                    let return_type = self.context.get_expr_type(value.span());
                    if let Some(ref expected) = self.context.current_return_type
                        && !self.type_satisfies(&return_type, expected)
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

                for stmt in while_stmt.body {
                    self.check_stmt(stmt);
                }
                for stmt in while_stmt.orelse {
                    self.check_stmt(stmt);
                }
            }
            Stmt::If(if_stmt) => {
                let _test_type = self.context.get_expr_type(if_stmt.test.span());

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

                self.check_expr(&expr_stmt.value);
            }
            _ => {}
        }
    }

    /// Check types within an expression
    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::BinOp(binop) => {

                self.check_expr(binop.left);
                self.check_expr(binop.right);


                let left_ty = self.context.get_expr_type(binop.left.span());
                let right_ty = self.context.get_expr_type(binop.right.span());
                self.check_binop_types(binop.op, &left_ty, &right_ty, binop.span);
            }
            Expr::UnaryOp(unary) => {
                self.check_expr(unary.operand);


                let operand_ty = self.context.get_expr_type(unary.operand.span());
                let valid = match unary.op {
                    "not" => true, // 'not' works on any type
                    "-" | "+" => {

                        matches!(
                            operand_ty,
                            Type::Int | Type::Float | Type::Complex | Type::Bool
                        )
                    }
                    "~" => {

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


                let obj_ty = self.context.get_expr_type(attr.value.span());


                let attr_ty =
                    BUILTIN_ATTRIBUTE_REGISTRY.lookup_builtin_attribute(&obj_ty, attr.attr);

                let has_attr = match &obj_ty {
                    Type::Unknown | Type::Any => true, // Can't verify, assume valid
                    Type::Instance(class_name) => {


                        let key = (class_name.clone(), attr.attr.to_string());

                        if self.context.class_attributes.contains_key(&key) {

                            true
                        } else {

                            if self.context.strict_attribute_checking {

                                self.context.add_error(*error(
                                    ErrorKind::InvalidAttribute {
                                        obj_type: class_name.clone(),
                                        attribute: attr.attr.to_string(),
                                    },
                                    attr.span,
                                ));
                                false
                            } else {

                                true
                            }
                        }
                    }
                    Type::Module(_) => {


                        true
                    }
                    Type::Union(types) => {

                        types.iter().all(|ty| {
                            BUILTIN_ATTRIBUTE_REGISTRY
                                .lookup_builtin_attribute(ty, attr.attr)
                                .is_some()
                        })
                    }
                    Type::Optional(inner_ty) => {

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

            }
        }
    }

    /// Check assignment target compatibility
    fn check_assignment_target(&mut self, target: &Expr, value_type: &Type) {
        match target {
            Expr::Name(name) => {


                if let Some(declared_type) = self
                    .context
                    .inference_ctx
                    .symbol_table()
                    .get_symbol_type(name.id)
                {

                    if !value_type.is_subtype_of(&declared_type)
                        && !matches!(value_type, Type::Unknown)
                        && !matches!(declared_type, Type::Unknown)
                    {
                        self.context.add_error(*error(
                            ErrorKind::TypeMismatch {
                                expected: declared_type.to_string(),
                                found: value_type.to_string(),
                            },
                            target.span(),
                        ));
                    }
                }

            }
            Expr::Tuple(tuple) => {

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

                self.check_expr(attr.value);

                let obj_type = self.context.get_expr_type(attr.value.span());


                if let Type::Instance(class_name) = &obj_type {

                    let attr_key = (class_name.clone(), attr.attr.to_string());

                    if let Some(Type::AttributeDescriptor {
                        kind: crate::semantic::types::AttributeKind::Property,
                        getter_type: _,
                        setter_type,
                    }) = self.context.inference_ctx.class_attributes.get(&attr_key)
                    {

                        if setter_type.is_none() {

                            self.context.add_error(*error(
                                ErrorKind::ReadOnlyProperty {
                                    name: attr.attr.to_string(),
                                    class_name: class_name.clone(),
                                },
                                attr.span,
                            ));
                        } else if let Some(setter_ty) = setter_type {


                            if let Type::Function { params, .. } = setter_ty.as_ref() {

                                if params.len() >= 2 {
                                    let (_self_param, value_param_type) = &params[1];


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

        let valid = match op {
            "+" => {

                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    ) | (Type::Str, Type::Str)
                        | (Type::List(_), Type::List(_))
                ) || self.has_operator_overload(left, "add")
            }
            "-" => {

                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    )
                ) || self.has_operator_overload(left, "sub")
            }
            "*" => {

                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    )
                ) || self.has_operator_overload(left, "mul")
            }
            "/" | "//" | "%" | "**" => {

                matches!(
                    (left, right),
                    (
                        Type::Int | Type::Float | Type::Complex | Type::Bool,
                        Type::Int | Type::Float | Type::Complex | Type::Bool
                    )
                )
            }
            "|" | "^" | "&" | "<<" | ">>" => {

                matches!(
                    (left, right),
                    (Type::Int | Type::Bool, Type::Int | Type::Bool)
                )
            }
            "@" => {


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

    /// Check if a type has an operator overload method
    fn has_operator_overload(&self, ty: &Type, method_name: &str) -> bool {
        match ty {
            Type::Instance(class_name) => {

                self.context
                    .class_attributes
                    .contains_key(&(class_name.clone(), method_name.to_string()))
            }
            _ => false,
        }
    }

    /// Check subscript operation
    fn check_subscript(&mut self, container: &Type, index: &Type, span: TextRange) {
        let valid = match container {
            Type::List(_) | Type::Tuple(_) => {

                matches!(index, Type::Int | Type::Bool)
            }
            Type::Dict(key_type, _) => {

                index.is_subtype_of(key_type)
            }
            Type::Str => {

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
            Type::Unknown | Type::Any => {

            }

            Type::Instance(class_name) => {

                let has_enter = self
                    .context
                    .get_class_attribute(class_name, "enter")
                    .is_some();
                let has_exit = self
                    .context
                    .get_class_attribute(class_name, "exit")
                    .is_some();

                if !has_enter || !has_exit {
                    let missing = match (has_enter, has_exit) {
                        (false, true) => "@operator enter",
                        (true, false) => "@operator exit",
                        _ => "@operator enter and @operator exit",
                    };

                    self.context.add_error(*error(
                        ErrorKind::TypeMismatch {
                            expected: format!(
                                "ContextManager protocol (requires {} method)",
                                missing
                            ),
                            found: format!("class '{}'", class_name),
                        },
                        span,
                    ));
                }
            }

            Type::Module(_) => {

                self.context.add_error(*error(
                    ErrorKind::TypeMismatch {
                        expected: "ContextManager (requires @operator enter and @operator exit)"
                            .to_string(),
                        found: ctx_type.display_name(),
                    },
                    span,
                ));
            }

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

            _ => {


                self.context.add_error(*error(
                    ErrorKind::TypeMismatch {
                        expected: "ContextManager (requires @operator enter and @operator exit)"
                            .to_string(),
                        found: ctx_type.display_name(),
                    },
                    span,
                ));
            }
        }
    }

    /// Check if a type satisfies another type (including protocol compliance)
    fn type_satisfies(&self, actual: &Type, expected: &Type) -> bool {

        if actual.is_subtype_of(expected) {
            return true;
        }


        if let (Type::Instance(actual_class), Type::Instance(protocol_name)) = (actual, expected) {
            return self.check_protocol_compliance(actual_class, protocol_name);
        }

        false
    }

    /// Check if a class implements a protocol (structurally)
    fn check_protocol_compliance(&self, class_name: &str, protocol_name: &str) -> bool {


        let protocol_methods: Vec<String> = self
            .context
            .class_attributes
            .iter()
            .filter(|((c, _), _)| c == protocol_name)
            .map(|((_, method), _)| method.clone())
            .collect();


        if protocol_methods.is_empty() {
            return false;
        }


        for required_method in protocol_methods {
            let class_key = (class_name.to_string(), required_method);
            if !self.context.class_attributes.contains_key(&class_key) {
                return false;
            }
        }

        true
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

            let positional_args: Vec<(Type, TextRange)> = call
                .args
                .iter()
                .map(|arg| (self.context.get_expr_type(arg.span()), arg.span()))
                .collect();


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


            let num_positional = positional_args.len();
            let num_params = params.len();



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


            for (i, (arg_ty, arg_span)) in positional_args.iter().enumerate() {
                if let Some((_param_name, expected_ty)) = params.get(i) {


                    if !self.type_satisfies(arg_ty, expected_ty)
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


            for (arg_name, arg_ty, arg_span) in keyword_args.iter() {

                let param_match = params
                    .iter()
                    .enumerate()
                    .find(|(_idx, (param_name, _ty))| param_name.as_deref() == *arg_name);

                if let Some((idx, (_param_name, param_ty))) = param_match {

                    if !self.type_satisfies(arg_ty, param_ty)
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
                } else if let Some(name) = arg_name {

                    self.context.add_error(*error(
                        ErrorKind::UnexpectedKeywordArgument {
                            name: name.to_string(),
                        },
                        *arg_span,
                    ));
                }
            }
        }
    }

    /// Validate generator protocol
    #[allow(dead_code)]
    fn validate_generator_protocol(&mut self, gen_type: &Type, span: TextRange) {
        if let Type::Generator(elem_ty) = gen_type {



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

    use crate::{Arena, Lexer, Parser};

    fn check_types(source: &str) -> Vec<Error> {
        let arena = Arena::new();
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer, &arena);
        let module = parser.parse_module().expect("Parse failed");


        let mut name_resolver = NameResolver::new();
        name_resolver.resolve_module(module);
        let (symbol_table, _name_errors) = name_resolver.into_symbol_table();


        let mut infer_ctx = TypeInferenceContext::new(symbol_table);
        let mut inference = TypeInference::new(&mut infer_ctx);
        inference.infer_module(module);


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

        assert!(!errors.is_empty());
        assert!(matches!(errors[0].kind, ErrorKind::TypeMismatch { .. }));
    }

    #[test]
    fn test_invalid_binop() {

        let source = r#"
s1 = "hello"
s2 = "world"
x = s1 - s2
"#;
        let errors = check_types(source);

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
