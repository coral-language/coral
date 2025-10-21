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
use std::collections::HashMap;
use text_size::TextRange;

/// Protocol checker
pub struct ProtocolChecker<'a> {
    _context: &'a TypeInferenceContext,
    errors: Vec<Error>,
    protocols: HashMap<String, ProtocolDef<'a>>,
    classes: HashMap<String, ClassInfo<'a>>,
}

/// Protocol definition with required methods
#[derive(Debug, Clone)]
struct ProtocolDef<'a> {
    _name: String,
    methods: HashMap<String, MethodSignature<'a>>,
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
    bases: Vec<String>,
    span: TextRange,
}

impl<'a> ProtocolChecker<'a> {
    pub fn new(context: &'a TypeInferenceContext) -> Self {
        Self {
            _context: context,
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
        let is_runtime_checkable = self.has_runtime_checkable_decorator(class);

        // Collect methods from protocol
        for stmt in class.body {
            if let Stmt::FuncDef(func) = stmt {
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
        }

        self.protocols.insert(
            class.name.to_string(),
            ProtocolDef {
                _name: class.name.to_string(),
                methods,
                _is_runtime_checkable: is_runtime_checkable,
            },
        );
    }

    fn collect_class(&mut self, class: &ClassDefStmt<'a>) {
        let mut methods = HashMap::new();
        let mut bases = Vec::new();

        // Extract base class names
        for base in class.bases {
            if let Expr::Name(name) = base {
                bases.push(name.id.to_string());
            }
        }

        // Collect methods
        for stmt in class.body {
            if let Stmt::FuncDef(func) = stmt {
                methods.insert(
                    func.name.to_string(),
                    MethodSignature {
                        args: &func.args,
                        returns: func.returns.as_ref().map(|r| r.as_ref()),
                        span: func.span,
                    },
                );
            }
        }

        self.classes.insert(
            class.name.to_string(),
            ClassInfo {
                methods,
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
        );

        // Collect all validation data upfront to avoid borrow checker issues
        let mut validations: Vec<ValidationData> = Vec::new();

        for (class_name, class_info) in &self.classes {
            for base_name in &class_info.bases {
                if let Some(protocol) = self.protocols.get(base_name) {
                    // Collect protocol methods that need validation
                    let mut protocol_methods = Vec::new();
                    for (method_name, method_sig) in &protocol.methods {
                        if !self.is_special_method(method_name) {
                            protocol_methods.push((method_name.clone(), method_sig.clone()));
                        }
                    }

                    validations.push((
                        class_name.clone(),
                        class_info.span,
                        base_name.clone(),
                        protocol_methods,
                    ));
                }
            }
        }

        // Now perform validations with collected data
        for (class_name, class_span, protocol_name, protocol_methods) in validations {
            let class_info = self.classes.get(&class_name).unwrap();

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

        // For now, we do a simple check
        // A full implementation would check:
        // - Argument types are compatible
        // - Return types are compatible (contravariant for args, covariant for return)
        // - Default arguments are compatible
        true
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
        name.starts_with("__") && name.ends_with("__")
    }
}
