// Protocol (structural typing) checking
//
// This pass validates Protocol implementations (PEP 544):
// 1. Checks if classes properly implement Protocol interfaces
// 2. Validates structural subtyping (duck typing)
// 3. Ensures protocol methods have correct signatures
// 4. Checks runtime_checkable protocols

use crate::ast::expr::Expr;
use crate::ast::nodes::{Arguments, ClassDefStmt, FuncDefStmt, Module, Stmt};
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::type_inference::TypeInferenceContext;
use crate::semantic::types::Type;
use std::collections::HashMap;
use text_size::TextRange;

/// Protocol checker
pub struct ProtocolChecker<'a> {
    context: &'a TypeInferenceContext,
    errors: Vec<Error>,
    protocols: HashMap<String, ProtocolDef<'a>>,
    classes: HashMap<String, ClassInfo<'a>>,
}

/// Protocol definition with required methods and attributes
#[derive(Debug, Clone)]
struct ProtocolDef<'a> {
    _name: String,
    methods: HashMap<String, MethodSignature<'a>>,
    attributes: HashMap<String, Option<&'a Expr<'a>>>, // attribute name -> type annotation
    _is_runtime_checkable: bool,
}

/// Method signature for protocol checking
#[derive(Debug, Clone)]
struct MethodSignature<'a> {
    args: &'a Arguments<'a>,
    returns: Option<&'a Expr<'a>>,
    span: TextRange,
}

#[derive(Debug, Clone)]
struct ClassInfo<'a> {
    methods: HashMap<String, MethodSignature<'a>>,
    attributes: HashMap<String, Option<&'a Expr<'a>>>, // attribute name -> type annotation
    bases: Vec<String>,
    span: TextRange,
}

impl<'a> ProtocolChecker<'a> {
    pub fn new(context: &'a TypeInferenceContext) -> Self {
        Self {
            context,
            errors: Vec::new(),
            protocols: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    pub fn check_module(&mut self, module: &Module<'a>) -> Vec<Error> {
        // First pass: collect all protocols and classes
        for stmt in module.body {
            self.collect_definitions(stmt);
        }

        // Second pass: validate protocol implementations
        self.validate_implementations();

        std::mem::take(&mut self.errors)
    }

    fn collect_definitions(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::ClassDef(class) => {
                if self.is_protocol_class(class) {
                    self.collect_protocol(class);
                } else {
                    self.collect_class(class);
                }
            }
            Stmt::FuncDef(func) => {
                // Check nested functions
                for s in func.body {
                    self.collect_definitions(s);
                }
            }
            Stmt::If(if_stmt) => {
                for s in if_stmt.body {
                    self.collect_definitions(s);
                }
                for s in if_stmt.orelse {
                    self.collect_definitions(s);
                }
            }
            Stmt::While(while_stmt) => {
                for s in while_stmt.body {
                    self.collect_definitions(s);
                }
                for s in while_stmt.orelse {
                    self.collect_definitions(s);
                }
            }
            Stmt::For(for_stmt) => {
                for s in for_stmt.body {
                    self.collect_definitions(s);
                }
                for s in for_stmt.orelse {
                    self.collect_definitions(s);
                }
            }
            Stmt::Try(try_stmt) => {
                for s in try_stmt.body {
                    self.collect_definitions(s);
                }
                for handler in try_stmt.handlers {
                    for s in handler.body {
                        self.collect_definitions(s);
                    }
                }
                for s in try_stmt.orelse {
                    self.collect_definitions(s);
                }
                for s in try_stmt.finalbody {
                    self.collect_definitions(s);
                }
            }
            Stmt::With(with_stmt) => {
                for s in with_stmt.body {
                    self.collect_definitions(s);
                }
            }
            _ => {}
        }
    }

    fn is_protocol_class(&self, class: &ClassDefStmt<'a>) -> bool {
        // Check if class inherits from Protocol
        for base in class.bases {
            if let Expr::Name(name) = base {
                if name.id == "Protocol" {
                    return true;
                }
            } else if let Expr::Attribute(attr) = base {
                // typing.Protocol
                if attr.attr == "Protocol"
                    && let Expr::Name(module) = attr.value
                    && module.id == "typing"
                {
                    return true;
                }
            }
        }
        false
    }

    fn collect_protocol(&mut self, class: &ClassDefStmt<'a>) {
        let mut methods = HashMap::new();
        let mut attributes = HashMap::new();
        let is_runtime_checkable = self.has_runtime_checkable_decorator(class);

        // Collect methods and attributes from protocol
        for stmt in class.body {
            match stmt {
                Stmt::FuncDef(func) => {
                    // Check if method has implementation (body has more than just pass/...)
                    let has_implementation = self.has_method_implementation(func);

                    if has_implementation && !self.is_special_method(func.name) {
                        self.errors.push(*error(
                            ErrorKind::ProtocolWithImplementation {
                                protocol_name: class.name.to_string(),
                                method_name: func.name.to_string(),
                            },
                            func.span,
                        ));
                    }

                    methods.insert(
                        func.name.to_string(),
                        MethodSignature {
                            args: &func.args,
                            returns: func.returns.as_ref().map(|r| r.as_ref()),
                            span: func.span,
                        },
                    );
                }
                Stmt::AnnAssign(ann_assign) => {
                    // Collect required attributes from annotated assignments
                    if let Expr::Name(name) = &ann_assign.target {
                        attributes.insert(name.id.to_string(), Some(&ann_assign.annotation));
                    }
                }
                _ => {}
            }
        }

        self.protocols.insert(
            class.name.to_string(),
            ProtocolDef {
                _name: class.name.to_string(),
                methods,
                attributes,
                _is_runtime_checkable: is_runtime_checkable,
            },
        );
    }

    fn collect_class(&mut self, class: &ClassDefStmt<'a>) {
        let mut methods = HashMap::new();
        let mut attributes = HashMap::new();
        let mut bases = Vec::new();

        // Extract base class names
        for base in class.bases {
            if let Expr::Name(name) = base {
                bases.push(name.id.to_string());
            }
        }

        // Collect methods and attributes
        for stmt in class.body {
            match stmt {
                Stmt::FuncDef(func) => {
                    methods.insert(
                        func.name.to_string(),
                        MethodSignature {
                            args: &func.args,
                            returns: func.returns.as_ref().map(|r| r.as_ref()),
                            span: func.span,
                        },
                    );
                }
                Stmt::AnnAssign(ann_assign) => {
                    // Collect class attributes
                    if let Expr::Name(name) = &ann_assign.target {
                        attributes.insert(name.id.to_string(), Some(&ann_assign.annotation));
                    }
                }
                _ => {}
            }
        }

        self.classes.insert(
            class.name.to_string(),
            ClassInfo {
                methods,
                attributes,
                bases,
                span: class.span,
            },
        );
    }

    fn validate_implementations(&mut self) {
        type ValidationData<'a> = (
            String,
            TextRange,
            String,
            Vec<(String, MethodSignature<'a>)>,
            Vec<(String, Option<&'a Expr<'a>>)>,
        );

        // Collect all validation data upfront to avoid borrow checker issues
        let mut validations: Vec<ValidationData> = Vec::new();

        // Check explicit protocol implementations (classes with 'implements Protocol')
        for (class_name, class_info) in &self.classes {
            for base_name in &class_info.bases {
                if let Some(protocol) = self.protocols.get(base_name) {
                    // Collect protocol methods that need validation
                    let mut protocol_methods = Vec::new();
                    for (method_name, method_sig) in &protocol.methods {
                        protocol_methods.push((method_name.clone(), method_sig.clone()));
                    }

                    // Collect protocol attributes that need validation
                    let mut protocol_attributes = Vec::new();
                    for (attr_name, attr_type) in &protocol.attributes {
                        protocol_attributes.push((attr_name.clone(), *attr_type));
                    }

                    validations.push((
                        class_name.clone(),
                        class_info.span,
                        base_name.clone(),
                        protocol_methods,
                        protocol_attributes,
                    ));
                }
            }
        }

        // Now perform validations with collected data
        for (class_name, class_span, protocol_name, protocol_methods, protocol_attributes) in
            validations
        {
            let class_info = self.classes.get(&class_name).unwrap();

            // Validate methods
            for (method_name, protocol_sig) in protocol_methods {
                match class_info.methods.get(&method_name) {
                    None => {
                        self.errors.push(*error(
                            ErrorKind::MissingProtocolMethod {
                                class_name: class_name.clone(),
                                protocol_name: protocol_name.clone(),
                                method_name: method_name.clone(),
                            },
                            class_span,
                        ));
                    }
                    Some(class_sig) => {
                        // Check if signatures match
                        if !self.signatures_match(&protocol_sig, class_sig) {
                            self.errors.push(*error(
                                ErrorKind::MethodSignatureMismatch {
                                    class_name: class_name.clone(),
                                    protocol_name: protocol_name.clone(),
                                    method_name: method_name.clone(),
                                    expected: self.signature_to_string(&protocol_sig),
                                    found: self.signature_to_string(class_sig),
                                },
                                class_sig.span,
                            ));
                        }
                    }
                }
            }

            // Validate attributes
            for (attr_name, protocol_attr_type) in protocol_attributes {
                match class_info.attributes.get(&attr_name) {
                    None => {
                        self.errors.push(*error(
                            ErrorKind::MissingProtocolAttribute {
                                class_name: class_name.clone(),
                                protocol_name: protocol_name.clone(),
                                attribute_name: attr_name.clone(),
                            },
                            class_span,
                        ));
                    }
                    Some(class_attr_type) => {
                        // Check type compatibility if both have type annotations
                        if let (Some(protocol_type_expr), Some(class_type_expr)) =
                            (protocol_attr_type, class_attr_type)
                        {
                            let protocol_type = self.expr_to_type(protocol_type_expr);
                            let class_type = self.expr_to_type(class_type_expr);

                            // Class attribute type must be compatible with protocol attribute type
                            // (class_type should be a subtype of protocol_type)
                            if !class_type.is_subtype_of(&protocol_type) {
                                self.errors.push(*error(
                                    ErrorKind::IncompatibleProtocolAttribute {
                                        class_name: class_name.clone(),
                                        protocol_name: protocol_name.clone(),
                                        attribute_name: attr_name.clone(),
                                        expected: protocol_type.display_name(),
                                        found: class_type.display_name(),
                                    },
                                    class_span,
                                ));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Check if a class structurally satisfies a protocol (duck typing)
    /// Returns true if the class has all required methods and attributes
    /// without requiring explicit 'implements Protocol'
    pub fn check_structural_compatibility(
        &self,
        class_name: &str,
        protocol_name: &str,
    ) -> Result<(), Vec<String>> {
        let class_info = self
            .classes
            .get(class_name)
            .ok_or_else(|| vec![format!("Class '{}' not found", class_name)])?;

        let protocol = self
            .protocols
            .get(protocol_name)
            .ok_or_else(|| vec![format!("Protocol '{}' not found", protocol_name)])?;

        let mut missing_items = Vec::new();

        // Check all required methods
        for (method_name, protocol_sig) in &protocol.methods {
            match class_info.methods.get(method_name) {
                None => {
                    missing_items.push(format!("method '{}'", method_name));
                }
                Some(class_sig) => {
                    if !self.signatures_match(protocol_sig, class_sig) {
                        missing_items
                            .push(format!("method '{}' with matching signature", method_name));
                    }
                }
            }
        }

        // Check all required attributes
        for attr_name in protocol.attributes.keys() {
            if !class_info.attributes.contains_key(attr_name) {
                missing_items.push(format!("attribute '{}'", attr_name));
            }
        }

        if missing_items.is_empty() {
            Ok(())
        } else {
            Err(missing_items)
        }
    }

    fn signatures_match(
        &self,
        protocol_sig: &MethodSignature,
        class_sig: &MethodSignature,
    ) -> bool {
        // Check argument count (excluding self)
        let protocol_args = protocol_sig.args.args.len();
        let class_args = class_sig.args.args.len();

        if protocol_args != class_args {
            return false;
        }

        // Check argument type compatibility (contravariance)
        // For methods, argument types should be contravariant:
        // protocol requires T -> implementation can accept U where T <: U (more general)
        for (protocol_arg, class_arg) in protocol_sig
            .args
            .args
            .iter()
            .zip(class_sig.args.args.iter())
        {
            if let (Some(protocol_annotation), Some(class_annotation)) = (
                protocol_arg.annotation.as_ref(),
                class_arg.annotation.as_ref(),
            ) {
                let protocol_type = self.expr_to_type(protocol_annotation);
                let class_type = self.expr_to_type(class_annotation);

                // For contravariance: class argument should accept at least what protocol accepts
                // This means protocol_type should be subtype of class_type
                // (class can accept more general types)
                if !protocol_type.is_subtype_of(&class_type) {
                    return false;
                }
            }
        }

        // Check return type compatibility (covariance)
        // Return types should be covariant:
        // protocol returns T -> implementation can return U where U <: T (more specific)
        if let (Some(protocol_return), Some(class_return)) =
            (protocol_sig.returns, class_sig.returns)
        {
            let protocol_type = self.expr_to_type(protocol_return);
            let class_type = self.expr_to_type(class_return);

            // For covariance: class return should be subtype of protocol return
            // (class returns more specific type)
            if !class_type.is_subtype_of(&protocol_type) {
                return false;
            }
        }

        // Validate default arguments compatibility
        // Default arguments should match or class can have more defaults than protocol
        let protocol_defaults = protocol_sig.args.defaults;
        let class_defaults = class_sig.args.defaults;

        // Class must provide at least as many defaults as protocol requires
        if class_defaults.len() < protocol_defaults.len() {
            return false;
        }

        // Check that overlapping defaults are compatible types
        let protocol_required_params = protocol_sig.args.args.len() - protocol_defaults.len();
        let class_required_params = class_sig.args.args.len() - class_defaults.len();

        // Class cannot require more parameters than protocol
        if class_required_params > protocol_required_params {
            return false;
        }

        // Handle *args matching (varargs)
        match (&protocol_sig.args.vararg, &class_sig.args.vararg) {
            (Some(protocol_vararg), Some(class_vararg)) => {
                // Both have varargs - check type compatibility
                if let (Some(protocol_annotation), Some(class_annotation)) = (
                    protocol_vararg.annotation.as_ref(),
                    class_vararg.annotation.as_ref(),
                ) {
                    let protocol_type = self.expr_to_type(protocol_annotation);
                    let class_type = self.expr_to_type(class_annotation);

                    // Varargs are contravariant (like parameters)
                    if !protocol_type.is_subtype_of(&class_type) {
                        return false;
                    }
                }
            }
            (Some(_), None) => {
                // Protocol requires varargs but class doesn't provide it
                return false;
            }
            (None, Some(_)) => {
                // Class provides varargs but protocol doesn't require it - OK (more flexible)
            }
            (None, None) => {
                // Neither has varargs - OK
            }
        }

        // Handle **kwargs matching (keyword arguments)
        match (&protocol_sig.args.kwarg, &class_sig.args.kwarg) {
            (Some(protocol_kwarg), Some(class_kwarg)) => {
                // Both have kwargs - check type compatibility
                if let (Some(protocol_annotation), Some(class_annotation)) = (
                    protocol_kwarg.annotation.as_ref(),
                    class_kwarg.annotation.as_ref(),
                ) {
                    let protocol_type = self.expr_to_type(protocol_annotation);
                    let class_type = self.expr_to_type(class_annotation);

                    // Kwargs are contravariant (like parameters)
                    if !protocol_type.is_subtype_of(&class_type) {
                        return false;
                    }
                }
            }
            (Some(_), None) => {
                // Protocol requires kwargs but class doesn't provide it
                return false;
            }
            (None, Some(_)) => {
                // Class provides kwargs but protocol doesn't require it - OK (more flexible)
            }
            (None, None) => {
                // Neither has kwargs - OK
            }
        }

        // Check keyword-only arguments
        let protocol_kwonly = protocol_sig.args.kwonlyargs;
        let class_kwonly = class_sig.args.kwonlyargs;

        // All protocol keyword-only args must be present in class
        for protocol_kwonly_arg in protocol_kwonly {
            let found = class_kwonly.iter().any(|class_arg| {
                if class_arg.arg == protocol_kwonly_arg.arg {
                    // Check type compatibility if both have annotations
                    if let (Some(protocol_ann), Some(class_ann)) =
                        (&protocol_kwonly_arg.annotation, &class_arg.annotation)
                    {
                        let protocol_type = self.expr_to_type(protocol_ann);
                        let class_type = self.expr_to_type(class_ann);
                        // Contravariant like regular parameters
                        protocol_type.is_subtype_of(&class_type)
                    } else {
                        true
                    }
                } else {
                    false
                }
            });

            if !found {
                return false;
            }
        }

        true
    }

    /// Convert a type annotation expression to a Type
    fn expr_to_type(&self, expr: &Expr) -> Type {
        // Try to get inferred type from context first
        let span = expr.span();
        if let Some(ty) = self
            .context
            .get_type_by_span(span.start().into(), span.end().into())
        {
            return ty.clone();
        }

        // Fallback: parse common type expressions
        match expr {
            Expr::Name(name) => match name.id {
                "int" => Type::Int,
                "float" => Type::Float,
                "str" => Type::Str,
                "bool" => Type::Bool,
                "None" => Type::None,
                "Any" => Type::Any,
                name => Type::Class(name.to_string()),
            },
            Expr::Subscript(subscript) => {
                // Handle generic types like List[int], Dict[str, int]
                if let Expr::Name(base_name) = subscript.value {
                    let element_type = self.expr_to_type(subscript.slice);
                    match base_name.id {
                        "list" | "List" => Type::List(Box::new(element_type)),
                        "set" | "Set" => Type::Set(Box::new(element_type)),
                        "tuple" | "Tuple" => Type::Tuple(vec![element_type]),
                        _ => Type::Generic {
                            base: Box::new(Type::Class(base_name.id.to_string())),
                            params: vec![element_type],
                        },
                    }
                } else {
                    Type::Unknown
                }
            }
            Expr::Tuple(tuple) => {
                // Handle Tuple[int, str, ...] or Union[int, str]
                let types: Vec<Type> = tuple.elts.iter().map(|e| self.expr_to_type(e)).collect();
                if types.is_empty() {
                    Type::Unknown
                } else {
                    Type::Tuple(types)
                }
            }
            Expr::BinOp(binop) => {
                // Handle Union types: int | str
                if binop.op == "|" {
                    let left = self.expr_to_type(binop.left);
                    let right = self.expr_to_type(binop.right);
                    Type::Union(vec![left, right])
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown,
        }
    }

    fn signature_to_string(&self, sig: &MethodSignature) -> String {
        let arg_count = sig.args.args.len();
        let has_return = sig.returns.is_some();

        if has_return {
            format!("({} args) -> type", arg_count)
        } else {
            format!("({} args)", arg_count)
        }
    }

    fn has_runtime_checkable_decorator(&self, class: &ClassDefStmt<'a>) -> bool {
        for decorator in class.decorators {
            if let Expr::Name(name) = decorator {
                if name.id == "runtime_checkable" {
                    return true;
                }
            } else if let Expr::Attribute(attr) = decorator
                && attr.attr == "runtime_checkable"
            {
                return true;
            }
        }
        false
    }

    fn has_method_implementation(&self, func: &FuncDefStmt<'a>) -> bool {
        // Check if function body has actual implementation beyond pass/...
        if func.body.is_empty() {
            return false;
        }

        // If body only has Pass or Expr(...), it's not considered an implementation
        if func.body.len() == 1 {
            match &func.body[0] {
                Stmt::Pass(_) => return false,
                Stmt::Expr(expr_stmt) => {
                    // Check if it's just ... (ellipsis)
                    if let Expr::Constant(c) = &expr_stmt.value
                        && c.value == "..."
                    {
                        return false;
                    }
                }
                _ => {}
            }
        }

        true
    }

    fn is_special_method(&self, name: &str) -> bool {
        use crate::ast::protocols::Protocols;
        Protocols::is_special_method(name)
    }
}
