// Type checking pass

use crate::ast::*;
use crate::error::{UnifiedError as Error, UnifiedErrorKind as ErrorKind, error};
use crate::semantic::passes::type_inference::{TypeInferenceContext, parse_annotation};
use crate::semantic::types::{Type, builtins::BUILTIN_ATTRIBUTE_REGISTRY};
use text_size::TextRange;

/// Type checker context
pub struct TypeCheckContext {
    /// Type inference context with inferred types
    inference_ctx: TypeInferenceContext,
    /// Errors collected during type checking
    errors: Vec<Error>,
    /// Current function return type (for checking return statements)
    current_return_type: Option<Type>,
}

impl TypeCheckContext {
    pub fn new(inference_ctx: TypeInferenceContext) -> Self {
        Self {
            inference_ctx,
            errors: Vec::new(),
            current_return_type: None,
        }
    }

    /// Get type checking errors
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    /// Consume the context and return the inference context
    pub fn into_inference_context(self) -> TypeInferenceContext {
        self.inference_ctx
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
pub struct TypeChecker<'a> {
    context: &'a mut TypeCheckContext,
}

impl<'a> TypeChecker<'a> {
    pub fn new(context: &'a mut TypeCheckContext) -> Self {
        Self { context }
    }

    /// Check types for a module
    pub fn check_module(&mut self, module: &Module) {
        for stmt in module.body {
            self.check_stmt(stmt);
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
                    // A context manager needs __enter__ and __exit__ methods
                    // For now, we'll accept Unknown, Any, and known context manager types
                    let is_context_manager = matches!(
                        ctx_type,
                        Type::Unknown | Type::Any | Type::Instance(_) | Type::Module(_)
                    );

                    // We could also check for specific types we know are context managers
                    // like file objects, locks, etc., but that requires more type infrastructure

                    if !is_context_manager {
                        // For primitive types, we know they're not context managers
                        self.context.add_error(*error(
                            ErrorKind::TypeMismatch {
                                expected: Type::Any.to_string(), // Placeholder for "context manager"
                                found: ctx_type.to_string(),
                            },
                            item.context_expr.span(),
                        ));
                    }
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

                // Check function call argument types if we know the function type
                let func_ty = self.context.get_expr_type(call.func.span());
                if let Type::Function {
                    params,
                    returns: _,
                    captures: _,
                } = func_ty
                {
                    // Check argument count
                    if call.args.len() != params.len() {
                        self.context.add_error(*error(
                            ErrorKind::ArgumentCountMismatch {
                                expected: params.len(),
                                found: call.args.len(),
                            },
                            call.span,
                        ));
                    } else {
                        // Check each argument type
                        for (i, (arg, expected_ty)) in
                            call.args.iter().zip(params.iter()).enumerate()
                        {
                            let arg_ty = self.context.get_expr_type(arg.span());
                            if !arg_ty.is_subtype_of(expected_ty) {
                                self.context.add_error(*error(
                                    ErrorKind::InvalidArgumentType {
                                        param_index: i,
                                        expected: expected_ty.to_string(),
                                        found: arg_ty.to_string(),
                                    },
                                    arg.span(),
                                ));
                            }
                        }
                    }
                }
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
                    Type::Instance(_class_name) => {
                        // For user-defined classes, we skip validation
                        // In a full implementation, we would use ClassAnalyzer here
                        true
                    }
                    Type::Module(_) => true, // Modules can have any attribute
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
                // Attribute assignments are generally valid (setting object attributes)
                // We could check if the attribute is settable, but that requires
                // more type system infrastructure (property descriptors, __setattr__, etc.)
                self.check_expr(attr.value);
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
                // Matrix multiplication - accept for now
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

    /// Validate generator protocol
    #[allow(dead_code)]
    fn validate_generator_protocol(&mut self, gen_type: &Type, span: TextRange) {
        if let Type::Generator(elem_ty) = gen_type {
            // Generators implicitly have __iter__ and __next__
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
        let mut check_ctx = TypeCheckContext::new(infer_ctx);
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
