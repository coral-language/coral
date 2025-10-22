//! AST to HIR lowering

use super::class_analysis::ClassAnalyzer;
use super::typed_expr::*;
use super::typed_item::{TypedExport, TypedImport, TypedModule};
use super::typed_pattern::*;
use super::typed_stmt::*;
use crate::arena::Arena;
use crate::arena::interner::Interner;
use crate::arena::symbol::Symbol;
use crate::ast::*;
use crate::semantic::symbol::SymbolTable;
use crate::semantic::types::Type;
use text_size::TextRange;

/// HIR lowering errors
#[derive(Debug, Clone)]
pub enum HirLoweringError {
    /// Undefined name
    UndefinedName { name: String, span: TextRange },
    /// Type inference failed
    TypeInferenceFailed { expr: String, span: TextRange },
    /// Desugaring failed
    DesugaringFailed {
        description: String,
        span: TextRange,
    },
    /// Class analysis failed
    ClassAnalysisFailed { class: String, span: TextRange },
}

impl std::fmt::Display for HirLoweringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirLoweringError::UndefinedName { name, .. } => {
                write!(f, "Undefined name: '{}'", name)
            }
            HirLoweringError::TypeInferenceFailed { expr, .. } => {
                write!(f, "Type inference failed for: {}", expr)
            }
            HirLoweringError::DesugaringFailed { description, .. } => {
                write!(f, "Desugaring failed: {}", description)
            }
            HirLoweringError::ClassAnalysisFailed { class, .. } => {
                write!(f, "Class analysis failed for: {}", class)
            }
        }
    }
}

impl std::error::Error for HirLoweringError {}

/// HIR lowerer that transforms AST to HIR
pub struct HirLowerer<'a> {
    /// Arena for allocating HIR nodes
    arena: &'a Arena,
    /// String interner for symbol resolution
    interner: &'a mut Interner,
    /// Symbol table for name resolution
    symbol_table: SymbolTable,
    /// Class analyzer for MRO computation
    class_analyzer: ClassAnalyzer<'a>,
    /// Errors collected during lowering
    errors: Vec<HirLoweringError>,
}

impl<'a> HirLowerer<'a> {
    /// Create a new HIR lowerer
    pub fn new(arena: &'a Arena, interner: &'a mut Interner) -> Self {
        let symbol_table = SymbolTable::new();
        let class_analyzer = ClassAnalyzer::new();

        Self {
            arena,
            interner,
            symbol_table,
            class_analyzer,
            errors: Vec::new(),
        }
    }

    /// Intern a string and return its symbol
    fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    /// Lower a module to HIR
    pub fn lower_module(
        &mut self,
        module: &Module<'a>,
    ) -> Result<TypedModule<'a>, Vec<HirLoweringError>> {
        self.errors.clear();

        // First pass: collect all class definitions for MRO computation
        self.collect_classes(module);

        // Set the interner on the class analyzer for decorator detection
        // We need to be careful with the borrowing here - first get the interner reference
        let interner_ptr = self.interner as *const _;
        let interner_ref = unsafe { &*interner_ptr };
        self.class_analyzer.set_interner(interner_ref);

        // Compute MRO for all classes
        if let Err(_e) = self.class_analyzer.analyze() {
            self.errors.push(HirLoweringError::ClassAnalysisFailed {
                class: "unknown".to_string(),
                span: module.span,
            });
        }

        // Lower all statements
        let body = self.lower_statements(module.body);
        let imports = self.extract_imports(module.body);
        let exports = self.extract_exports(module.body);

        let typed_module = TypedModule {
            name: self.intern("__main__"), // Module symbol
            body,
            imports,
            exports,
            span: module.span,
            docstring: module.docstring,
        };

        if self.errors.is_empty() {
            Ok(typed_module)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    /// Get the class analyzer after lowering
    pub fn into_class_analyzer(self) -> ClassAnalyzer<'a> {
        self.class_analyzer
    }

    /// Lower a list of statements
    fn lower_statements(&mut self, statements: &[Stmt<'a>]) -> &'a [TypedStmt<'a>] {
        let mut typed_statements = Vec::new();

        for stmt in statements {
            if let Some(typed_stmt) = self.lower_statement(stmt) {
                typed_statements.push(typed_stmt);
            }
        }

        self.arena.alloc_slice_vec(typed_statements)
    }

    /// Lower a single statement
    fn lower_statement(&mut self, stmt: &Stmt<'a>) -> Option<TypedStmt<'a>> {
        match stmt {
            Stmt::Expr(expr_stmt) => {
                let value = self.lower_expression(&expr_stmt.value)?;
                Some(TypedStmt::Expr(TypedExprStmt {
                    value,
                    span: expr_stmt.span,
                }))
            }
            Stmt::Assign(assign) => {
                let targets = self.lower_expressions(assign.targets)?;
                let value = self.lower_expression(&assign.value)?;
                Some(TypedStmt::Assign(TypedAssignStmt {
                    targets,
                    value,
                    span: assign.span,
                }))
            }
            Stmt::AnnAssign(ann_assign) => {
                let target = self.lower_expression(&ann_assign.target)?;
                let annotation = self.lower_expression(&ann_assign.annotation)?;
                let value = ann_assign
                    .value
                    .as_ref()
                    .map(|v| self.lower_expression(v))?;
                Some(TypedStmt::AnnAssign(TypedAnnAssignStmt {
                    target,
                    annotation,
                    value,
                    span: ann_assign.span,
                }))
            }
            Stmt::AugAssign(aug_assign) => {
                let target = self.lower_expression(&aug_assign.target)?;
                let value = self.lower_expression(&aug_assign.value)?;
                Some(TypedStmt::AugAssign(TypedAugAssignStmt {
                    target,
                    op: aug_assign.op,
                    value,
                    span: aug_assign.span,
                }))
            }
            Stmt::Return(ret) => {
                let value = ret.value.as_ref().map(|v| self.lower_expression(v))?;
                Some(TypedStmt::Return(TypedReturnStmt {
                    value,
                    span: ret.span,
                }))
            }
            Stmt::If(if_stmt) => {
                let test = self.lower_expression(&if_stmt.test)?;
                let body = self.lower_statements(if_stmt.body);
                let orelse = self.lower_statements(if_stmt.orelse);
                Some(TypedStmt::If(TypedIfStmt {
                    test,
                    body,
                    orelse,
                    span: if_stmt.span,
                }))
            }
            Stmt::While(while_stmt) => {
                let test = self.lower_expression(&while_stmt.test)?;
                let body = self.lower_statements(while_stmt.body);
                let orelse = self.lower_statements(while_stmt.orelse);
                Some(TypedStmt::While(TypedWhileStmt {
                    test,
                    body,
                    orelse,
                    span: while_stmt.span,
                }))
            }
            Stmt::For(for_stmt) => {
                let target = self.lower_expression(&for_stmt.target)?;
                let iter = self.lower_expression(&for_stmt.iter)?;
                let iter_ref = self.arena.alloc(iter);
                let iter_call = self.create_iter_call(iter_ref)?;
                let next_call = self.create_next_call(iter_ref)?;
                let body = self.lower_statements(for_stmt.body);
                let orelse = self.lower_statements(for_stmt.orelse);
                Some(TypedStmt::For(Box::new(TypedForStmt {
                    target,
                    iter: iter_ref.clone(),
                    iter_call,
                    next_call,
                    body,
                    orelse,
                    is_async: for_stmt.is_async,
                    span: for_stmt.span,
                })))
            }
            Stmt::FuncDef(func) => self.lower_function_definition(func),
            Stmt::ClassDef(class) => self.lower_class_definition(class),
            Stmt::Pass(span) => Some(TypedStmt::Pass(*span)),
            Stmt::Break(span) => Some(TypedStmt::Break(*span)),
            Stmt::Continue(span) => Some(TypedStmt::Continue(*span)),
            Stmt::Import(import) => self.lower_import_statement(import),
            Stmt::From(from) => self.lower_from_statement(from),
            Stmt::Export(export) => self.lower_export_statement(export),
            Stmt::Raise(raise) => {
                let exc = raise.exc.as_ref().map(|e| self.lower_expression(e))?;
                let cause = raise.cause.as_ref().map(|e| self.lower_expression(e))?;
                Some(TypedStmt::Raise(TypedRaiseStmt {
                    exc,
                    cause,
                    span: raise.span,
                }))
            }
            Stmt::Try(try_stmt) => {
                let body = self.lower_statements(try_stmt.body);
                let handlers = self.lower_except_handlers(try_stmt.handlers)?;
                let orelse = self.lower_statements(try_stmt.orelse);
                let finalbody = self.lower_statements(try_stmt.finalbody);
                Some(TypedStmt::Try(TypedTryStmt {
                    body,
                    handlers,
                    orelse,
                    finalbody,
                    span: try_stmt.span,
                }))
            }
            Stmt::With(with_stmt) => {
                let items = self.lower_with_items(with_stmt.items)?;
                let body = self.lower_statements(with_stmt.body);
                Some(TypedStmt::With(TypedWithStmt {
                    items,
                    body,
                    is_async: with_stmt.is_async,
                    span: with_stmt.span,
                }))
            }
            Stmt::Assert(assert_stmt) => {
                let test = self.lower_expression(&assert_stmt.test)?;
                let msg = assert_stmt.msg.as_ref().map(|m| self.lower_expression(m))?;
                Some(TypedStmt::Assert(TypedAssertStmt {
                    test,
                    msg,
                    span: assert_stmt.span,
                }))
            }
            Stmt::Delete(delete_stmt) => {
                let targets = self.lower_expressions(delete_stmt.targets)?;
                Some(TypedStmt::Delete(TypedDeleteStmt {
                    targets,
                    span: delete_stmt.span,
                }))
            }
            Stmt::Global(global) => {
                let mut names = Vec::new();
                for n in global.names {
                    names.push(self.intern(n));
                }
                let names = self.arena.alloc_slice(&names);
                Some(TypedStmt::Global(TypedGlobalStmt {
                    names,
                    span: global.span,
                }))
            }
            Stmt::Nonlocal(nonlocal) => {
                let mut names = Vec::new();
                for n in nonlocal.names {
                    names.push(self.intern(n));
                }
                let names = self.arena.alloc_slice(&names);
                Some(TypedStmt::Nonlocal(TypedNonlocalStmt {
                    names,
                    span: nonlocal.span,
                }))
            }
            Stmt::Match(match_stmt) => {
                let subject = self.lower_expression(&match_stmt.subject)?;
                let cases = self.lower_match_cases(match_stmt.cases)?;
                Some(TypedStmt::Match(TypedMatchStmt {
                    subject,
                    cases,
                    span: match_stmt.span,
                }))
            }
            Stmt::Yield(yield_stmt) => {
                let value = yield_stmt
                    .value
                    .as_ref()
                    .map(|v| self.lower_expression(v))?;
                let value_ref = value.map(|v| self.arena.alloc(v));
                Some(TypedStmt::Yield(TypedYieldStmt {
                    value: value_ref,
                    span: yield_stmt.span,
                }))
            }
            Stmt::TypeAlias(type_alias) => {
                let value = self.lower_expression(&type_alias.value)?;
                let ty = self.infer_type(&value);

                // Lower type parameters
                let type_params = self
                    .lower_type_parameters(type_alias.type_params)
                    .unwrap_or(&[]);

                Some(TypedStmt::TypeAlias(TypedTypeAliasStmt {
                    name: self.intern(type_alias.name),
                    type_params,
                    value,
                    ty,
                    span: type_alias.span,
                }))
            }
        }
    }

    /// Lower a list of expressions
    fn lower_expressions(&mut self, expressions: &[Expr<'a>]) -> Option<&'a [TypedExpr<'a>]> {
        let mut typed_expressions = Vec::new();

        for expr in expressions {
            if let Some(typed_expr) = self.lower_expression(expr) {
                typed_expressions.push(typed_expr);
            } else {
                return None;
            }
        }

        Some(self.arena.alloc_slice_vec(typed_expressions))
    }

    /// Lower a single expression
    fn lower_expression(&mut self, expr: &Expr<'a>) -> Option<TypedExpr<'a>> {
        match expr {
            Expr::Constant(constant) => {
                let ty = self.infer_literal_type(constant.value);
                Some(TypedExpr::Constant(TypedConstantExpr {
                    value: constant.value,
                    ty,
                    span: constant.span,
                }))
            }
            Expr::Complex(complex) => {
                let ty = Type::Complex;
                Some(TypedExpr::Complex(TypedComplexExpr {
                    value: complex.value,
                    ty,
                    span: complex.span,
                }))
            }
            Expr::Bytes(bytes) => {
                let ty = Type::Bytes;
                Some(TypedExpr::Bytes(TypedBytesExpr {
                    value: bytes.value,
                    ty,
                    span: bytes.span,
                }))
            }
            Expr::Name(name) => self.lower_name_expression(name),
            Expr::BinOp(bin_op) => {
                let left = self.lower_expression(bin_op.left)?;
                let right = self.lower_expression(bin_op.right)?;
                let ty = self.infer_binary_operation_type(&left, bin_op.op, &right);
                let left_ref = self.arena.alloc(left);
                let right_ref = self.arena.alloc(right);
                Some(TypedExpr::BinOp(TypedBinOpExpr {
                    left: left_ref,
                    op: bin_op.op,
                    right: right_ref,
                    ty,
                    span: bin_op.span,
                }))
            }
            Expr::UnaryOp(unary_op) => {
                let operand = self.lower_expression(unary_op.operand)?;
                let ty = self.infer_unary_operation_type(unary_op.op, &operand);
                let operand_ref = self.arena.alloc(operand);
                Some(TypedExpr::UnaryOp(TypedUnaryOpExpr {
                    op: unary_op.op,
                    operand: operand_ref,
                    ty,
                    span: unary_op.span,
                }))
            }
            Expr::Compare(compare) => {
                let left = self.lower_expression(compare.left)?;
                let comparators = self.lower_expressions(compare.comparators)?;
                let ty = Type::Bool; // Comparisons always return bool
                let left_ref = self.arena.alloc(left);
                Some(TypedExpr::Compare(TypedCompareExpr {
                    left: left_ref,
                    ops: compare.ops,
                    comparators,
                    ty,
                    span: compare.span,
                }))
            }
            Expr::Call(call) => {
                let func = self.lower_expression(call.func)?;
                let args = self.lower_expressions(call.args)?;
                let keywords = self.lower_keywords(call.keywords)?;
                let ty = self.infer_call_type(&func, args, keywords);
                let func_ref = self.arena.alloc(func);
                Some(TypedExpr::Call(TypedCallExpr {
                    func: func_ref,
                    args,
                    keywords,
                    ty,
                    span: call.span,
                }))
            }
            Expr::Attribute(attr) => {
                let value = self.lower_expression(attr.value)?;
                let attr_symbol = self.intern(attr.attr);
                let ty = self.infer_attribute_type(&value, &attr_symbol);
                let value_ref = self.arena.alloc(value);
                Some(TypedExpr::Attribute(TypedAttributeExpr {
                    value: value_ref,
                    attr: attr_symbol,
                    ty,
                    span: attr.span,
                }))
            }
            Expr::Subscript(subscript) => {
                let value = self.lower_expression(subscript.value)?;
                let slice = self.lower_expression(subscript.slice)?;
                let ty = self.infer_subscript_type(&value, &slice);
                let value_ref = self.arena.alloc(value);
                let slice_ref = self.arena.alloc(slice);
                Some(TypedExpr::Subscript(TypedSubscriptExpr {
                    value: value_ref,
                    slice: slice_ref,
                    ty,
                    span: subscript.span,
                }))
            }
            Expr::Slice(slice) => {
                let lower = slice.lower.as_ref().map(|l| self.lower_expression(l))?;
                let upper = slice.upper.as_ref().map(|u| self.lower_expression(u))?;
                let step = slice.step.as_ref().map(|s| self.lower_expression(s))?;
                let ty = Type::Slice;
                let lower_ref = lower.map(|l| self.arena.alloc(l));
                let upper_ref = upper.map(|u| self.arena.alloc(u));
                let step_ref = step.map(|s| self.arena.alloc(s));
                Some(TypedExpr::Slice(TypedSliceExpr {
                    lower: lower_ref,
                    upper: upper_ref,
                    step: step_ref,
                    ty,
                    span: slice.span,
                }))
            }
            Expr::List(list) => {
                let elts = self.lower_expressions(list.elts)?;
                let ty = self.infer_list_type(elts);
                Some(TypedExpr::List(TypedListExpr {
                    elts,
                    ty,
                    span: list.span,
                }))
            }
            Expr::Tuple(tuple) => {
                let elts = self.lower_expressions(tuple.elts)?;
                let ty = self.infer_tuple_type(elts);
                Some(TypedExpr::Tuple(TypedTupleExpr {
                    elts,
                    ty,
                    span: tuple.span,
                }))
            }
            Expr::Set(set) => {
                let elts = self.lower_expressions(set.elts)?;
                let ty = self.infer_set_type(elts);
                Some(TypedExpr::Set(TypedSetExpr {
                    elts,
                    ty,
                    span: set.span,
                }))
            }
            Expr::Dict(dict) => {
                let keys = self.lower_dict_keys(dict.keys)?;
                let values = self.lower_expressions(dict.values)?;
                let ty = self.infer_dict_type(keys, values);
                Some(TypedExpr::Dict(TypedDictExpr {
                    keys,
                    values,
                    ty,
                    span: dict.span,
                }))
            }
            Expr::Lambda(lambda) => {
                let args = self.lower_arguments(&lambda.args)?;
                let body = self.lower_expression(lambda.body)?;
                let ty = self.infer_lambda_type(&args, &body);
                let body_ref = self.arena.alloc(body);
                Some(TypedExpr::Lambda(TypedLambdaExpr {
                    args,
                    body: body_ref,
                    ty,
                    span: lambda.span,
                }))
            }
            Expr::IfExp(if_expr) => {
                let test = self.lower_expression(if_expr.test)?;
                let body = self.lower_expression(if_expr.body)?;
                let orelse = self.lower_expression(if_expr.orelse)?;
                let ty = self.infer_conditional_type(&body, &orelse);
                let test_ref = self.arena.alloc(test);
                let body_ref = self.arena.alloc(body);
                let orelse_ref = self.arena.alloc(orelse);
                Some(TypedExpr::IfExp(TypedIfExpExpr {
                    test: test_ref,
                    body: body_ref,
                    orelse: orelse_ref,
                    ty,
                    span: if_expr.span,
                }))
            }
            Expr::BoolOp(bool_op) => {
                let values = self.lower_expressions(bool_op.values)?;
                let ty = Type::Bool; // Boolean operations always return bool
                Some(TypedExpr::BoolOp(TypedBoolOpExpr {
                    op: bool_op.op,
                    values,
                    ty,
                    span: bool_op.span,
                }))
            }
            Expr::Await(await_expr) => {
                let value = self.lower_expression(await_expr.value)?;
                let ty = self.infer_await_type(&value);
                let value_ref = self.arena.alloc(value);
                Some(TypedExpr::Await(TypedAwaitExpr {
                    value: value_ref,
                    ty,
                    span: await_expr.span,
                }))
            }
            Expr::NamedExpr(named_expr) => {
                let target = self.lower_expression(named_expr.target)?;
                let value = self.lower_expression(named_expr.value)?;
                let ty = value.ty().clone();
                let target_ref = self.arena.alloc(target);
                let value_ref = self.arena.alloc(value);
                Some(TypedExpr::NamedExpr(TypedNamedExpr {
                    target: target_ref,
                    value: value_ref,
                    ty,
                    span: named_expr.span,
                }))
            }
            Expr::JoinedStr(joined_str) => {
                let values = self.lower_expressions(joined_str.values)?;
                let ty = Type::Str;
                Some(TypedExpr::JoinedStr(TypedJoinedStrExpr {
                    values,
                    ty,
                    span: joined_str.span,
                }))
            }
            Expr::FormattedValue(formatted_value) => {
                let value = self.lower_expression(formatted_value.value)?;
                let format_spec = formatted_value
                    .format_spec
                    .as_ref()
                    .map(|fs| self.lower_joined_str(fs))?;
                let ty = Type::Str;
                let value_ref = self.arena.alloc(value);
                let format_spec_ref = format_spec.map(|fs| self.arena.alloc(fs));
                Some(TypedExpr::FormattedValue(TypedFormattedValueExpr {
                    value: value_ref,
                    conversion: formatted_value.conversion,
                    format_spec: format_spec_ref,
                    ty,
                    span: formatted_value.span,
                }))
            }
            Expr::TString(t_string) => {
                let values = self.lower_expressions(t_string.values)?;
                let ty = Type::TemplateString;
                Some(TypedExpr::TString(TypedTStringExpr {
                    values,
                    ty,
                    span: t_string.span,
                }))
            }
            Expr::Starred(starred) => {
                let value = self.lower_expression(starred.value)?;
                let ty = value.ty().clone();
                let value_ref = self.arena.alloc(value);
                Some(TypedExpr::Starred(TypedStarredExpr {
                    value: value_ref,
                    ty,
                    span: starred.span,
                }))
            }
            Expr::Yield(yield_expr) => {
                let value = yield_expr
                    .value
                    .as_ref()
                    .map(|v| self.lower_expression(v))?;
                let ty = Type::generator(
                    value
                        .as_ref()
                        .map(|v| v.ty().clone())
                        .unwrap_or(Type::Unknown),
                );
                let value_ref = value.map(|v| self.arena.alloc(v));
                Some(TypedExpr::Yield(TypedYieldExpr {
                    value: value_ref,
                    ty,
                    span: yield_expr.span,
                }))
            }
            Expr::YieldFrom(yield_from) => {
                let value = self.lower_expression(yield_from.value)?;
                let ty = Type::generator(value.ty().clone());
                let value_ref = self.arena.alloc(value);
                Some(TypedExpr::YieldFrom(TypedYieldFromExpr {
                    value: value_ref,
                    ty,
                    span: yield_from.span,
                }))
            }
            Expr::ModuleIntrospection(introspection) => {
                let ty = Type::Module("__main__".to_string());
                Some(TypedExpr::ModuleIntrospection(
                    TypedModuleIntrospectionExpr {
                        function: introspection.function,
                        ty,
                        span: introspection.span,
                    },
                ))
            }
            // Implement comprehension desugaring
            Expr::ListComp(list_comp) => self.lower_list_comprehension(list_comp),
            Expr::DictComp(dict_comp) => self.lower_dict_comprehension(dict_comp),
            Expr::SetComp(set_comp) => self.lower_set_comprehension(set_comp),
            Expr::GeneratorExp(gen_exp) => self.lower_generator_expression(gen_exp),
        }
    }

    /// Lower a name expression with symbol resolution
    fn lower_name_expression(&mut self, name: &NameExpr<'a>) -> Option<TypedExpr<'a>> {
        // Look up the symbol in the symbol table
        if let Some((symbol, _)) = self.symbol_table.lookup(name.id) {
            let ty = symbol.get_type().cloned().unwrap_or(Type::Unknown);
            // Create a symbol ID based on the symbol's name hash for consistency
            let symbol_id = crate::arena::symbol::Symbol::new(name.id.len() as u32);
            Some(TypedExpr::Name(TypedNameExpr {
                symbol: symbol_id,
                ty,
                span: name.span,
            }))
        } else {
            self.errors.push(HirLoweringError::UndefinedName {
                name: name.id.to_string(),
                span: name.span,
            });
            None
        }
    }

    /// Infer type for a literal value
    fn infer_literal_type(&self, value: &str) -> Type {
        // Simple type inference for literals
        if value.parse::<i64>().is_ok() {
            Type::Int
        } else if value.parse::<f64>().is_ok() {
            Type::Float
        } else if value == "True" || value == "False" {
            Type::Bool
        } else if value == "None" {
            Type::None
        } else {
            Type::Str
        }
    }

    /// Infer type for binary operation
    fn infer_binary_operation_type(
        &self,
        left: &TypedExpr<'a>,
        op: &str,
        right: &TypedExpr<'a>,
    ) -> Type {
        match op {
            "+" | "-" | "*" | "/" | "//" | "%" | "**" => {
                // Numeric operations
                if left.ty() == &Type::Int && right.ty() == &Type::Int {
                    Type::Int
                } else if left.ty() == &Type::Float || right.ty() == &Type::Float {
                    Type::Float
                } else {
                    Type::Unknown
                }
            }
            "==" | "!=" | "<" | "<=" | ">" | ">=" => Type::Bool,
            "and" | "or" => Type::Bool,
            _ => Type::Unknown,
        }
    }

    /// Infer type for unary operation
    fn infer_unary_operation_type(&self, op: &str, operand: &TypedExpr<'a>) -> Type {
        match op {
            "+" | "-" => operand.ty().clone(),
            "not" => Type::Bool,
            _ => Type::Unknown,
        }
    }

    /// Infer type for function call
    fn infer_call_type(
        &self,
        func: &TypedExpr<'a>,
        _args: &[TypedExpr<'a>],
        _keywords: &[TypedKeyword<'a>],
    ) -> Type {
        // Basic type inference for function calls
        match func {
            TypedExpr::Name(name_expr) => {
                // Check if it's a known function type
                if name_expr.ty.is_function() {
                    // Extract return type from function type
                    if let Type::Function { returns, .. } = &name_expr.ty {
                        returns.as_ref().clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
            TypedExpr::Attribute(attr_expr) => {
                // Method call - check if it's a method type
                if attr_expr.ty.is_function() {
                    if let Type::Function { returns, .. } = &attr_expr.ty {
                        returns.as_ref().clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            }
            _ => Type::Unknown,
        }
    }

    /// Infer type for attribute access
    fn infer_attribute_type(
        &self,
        value: &TypedExpr<'a>,
        attr: &crate::arena::symbol::Symbol,
    ) -> Type {
        use crate::semantic::types::builtins::BUILTIN_ATTRIBUTE_REGISTRY;

        let base_ty = value.ty();

        // Get the attribute name as a string
        if let Some(attr_name) = self.interner.resolve(*attr) {
            // For user-defined class instances, the ClassAnalyzer has already
            // collected attribute types during lowering. Those are available
            // in TypedClassDefStmt.attributes and TypedClassDefStmt.methods.
            // For HIR types, we use the builtin registry as a fallback.
            match base_ty {
                Type::Instance(_class_name) => {
                    // Class instance attributes would be resolved by consumers
                    // using the ClassAnalyzer after lowering is complete
                    BUILTIN_ATTRIBUTE_REGISTRY
                        .lookup_builtin_attribute(base_ty, attr_name)
                        .unwrap_or(Type::Unknown)
                }
                _ => {
                    // Use built-in attribute registry for other types
                    BUILTIN_ATTRIBUTE_REGISTRY
                        .lookup_builtin_attribute(base_ty, attr_name)
                        .unwrap_or(Type::Unknown)
                }
            }
        } else {
            Type::Unknown
        }
    }

    /// Infer type for subscript access
    fn infer_subscript_type(&self, value: &TypedExpr<'a>, _slice: &TypedExpr<'a>) -> Type {
        // Basic subscript type inference based on the value type
        match value {
            TypedExpr::Name(name_expr) => {
                match &name_expr.ty {
                    Type::List(element_type) => element_type.as_ref().clone(),
                    Type::Dict(_, value_type) => value_type.as_ref().clone(),
                    Type::Str => Type::Str, // String indexing returns string
                    Type::Tuple(element_types) => {
                        // For tuples, we'd need to know the index at compile time
                        // For now, return the union of all element types
                        if element_types.is_empty() {
                            Type::Unknown
                        } else {
                            element_types[0].clone()
                        }
                    }
                    _ => Type::Unknown,
                }
            }
            _ => Type::Unknown,
        }
    }

    /// Infer type for list
    fn infer_list_type(&self, elts: &[TypedExpr<'a>]) -> Type {
        if elts.is_empty() {
            Type::List(Box::new(Type::Unknown))
        } else {
            let element_type = elts[0].ty().clone();
            Type::List(Box::new(element_type))
        }
    }

    /// Infer type for tuple
    fn infer_tuple_type(&self, elts: &[TypedExpr<'a>]) -> Type {
        let element_types: Vec<Type> = elts.iter().map(|e| e.ty().clone()).collect();
        Type::Tuple(element_types)
    }

    /// Infer type for set
    fn infer_set_type(&self, elts: &[TypedExpr<'a>]) -> Type {
        if elts.is_empty() {
            Type::Set(Box::new(Type::Unknown))
        } else {
            let element_type = elts[0].ty().clone();
            Type::Set(Box::new(element_type))
        }
    }

    /// Infer type for dictionary
    fn infer_dict_type(&self, keys: &[Option<TypedExpr<'a>>], values: &[TypedExpr<'a>]) -> Type {
        let key_type = if let Some(Some(key)) = keys.first() {
            key.ty().clone()
        } else {
            Type::Unknown
        };
        let value_type = if let Some(value) = values.first() {
            value.ty().clone()
        } else {
            Type::Unknown
        };
        Type::Dict(Box::new(key_type), Box::new(value_type))
    }

    /// Infer type for lambda
    fn infer_lambda_type(&self, args: &TypedArguments<'a>, body: &TypedExpr<'a>) -> Type {
        // Construct a proper function type with parameter types from args and return type from body
        let mut param_types = Vec::new();

        // Add positional arguments
        for arg in args.posonlyargs {
            param_types.push(arg.ty.clone());
        }
        for arg in args.args {
            param_types.push(arg.ty.clone());
        }

        // Add keyword-only arguments
        for arg in args.kwonlyargs {
            param_types.push(arg.ty.clone());
        }

        // Add vararg if present
        if let Some(vararg) = args.vararg {
            param_types.push(vararg.ty.clone());
        }

        // Add kwarg if present
        if let Some(kwarg) = args.kwarg {
            param_types.push(kwarg.ty.clone());
        }

        Type::function(param_types, body.ty().clone())
    }

    /// Infer type for conditional expression
    fn infer_conditional_type(&self, body: &TypedExpr<'a>, orelse: &TypedExpr<'a>) -> Type {
        // Union of both branches
        Type::Union(vec![body.ty().clone(), orelse.ty().clone()])
    }

    /// Infer type for await expression
    fn infer_await_type(&self, value: &TypedExpr<'a>) -> Type {
        // Basic await type inference - await unwraps the coroutine type
        // In a full implementation, we'd extract the return type from the coroutine
        match &value.ty() {
            Type::Union(types) => {
                // If it's a union, await each type
                if types.is_empty() {
                    Type::Unknown
                } else {
                    types[0].clone()
                }
            }
            _ => value.ty().clone(),
        }
    }

    /// Infer type for any expression
    fn infer_type(&self, expr: &TypedExpr<'a>) -> Type {
        expr.ty().clone()
    }

    /// Create iterator call for for loop
    fn create_iter_call(&mut self, iter: &'a TypedExpr<'a>) -> Option<TypedExpr<'a>> {
        // Create a call to __iter__() method
        let iter_method = TypedExpr::Attribute(TypedAttributeExpr {
            value: iter,
            attr: self.intern("__iter__"),
            ty: Type::Unknown,
            span: iter.span(),
        });
        let iter_ref = self.arena.alloc(iter_method);
        Some(TypedExpr::Call(TypedCallExpr {
            func: iter_ref,
            args: &[],
            keywords: &[],
            ty: Type::Unknown,
            span: iter.span(),
        }))
    }

    /// Create next call for for loop
    fn create_next_call(&mut self, iter: &'a TypedExpr<'a>) -> Option<TypedExpr<'a>> {
        // Create a call to __next__() method
        let next_method = TypedExpr::Attribute(TypedAttributeExpr {
            value: iter,
            attr: self.intern("__next__"),
            ty: Type::Unknown,
            span: iter.span(),
        });
        let next_ref = self.arena.alloc(next_method);
        Some(TypedExpr::Call(TypedCallExpr {
            func: next_ref,
            args: &[],
            keywords: &[],
            ty: Type::Unknown,
            span: iter.span(),
        }))
    }

    /// Lower keywords
    fn lower_keywords(&mut self, keywords: &[Keyword<'a>]) -> Option<&'a [TypedKeyword<'a>]> {
        let mut typed_keywords = Vec::new();

        for kw in keywords {
            let value = self.lower_expression(&kw.value)?;
            let value_ref = self.arena.alloc(value);
            typed_keywords.push(TypedKeyword {
                arg: kw.arg,
                value: value_ref,
            });
        }

        Some(self.arena.alloc_slice_vec(typed_keywords))
    }

    /// Lower dictionary keys
    fn lower_dict_keys(
        &mut self,
        keys: &[Option<Expr<'a>>],
    ) -> Option<&'a [Option<TypedExpr<'a>>]> {
        let mut typed_keys = Vec::new();

        for key in keys {
            let typed_key = key.as_ref().map(|k| self.lower_expression(k))?;
            typed_keys.push(typed_key);
        }

        Some(self.arena.alloc_slice_vec(typed_keys))
    }

    /// Lower typed arguments
    fn lower_typed_args(&mut self, args: &[Arg<'a>]) -> Option<&'a [TypedArg<'a>]> {
        let mut typed_args = Vec::new();
        for arg in args {
            let annotation = arg
                .annotation
                .as_ref()
                .map(|ann| self.lower_expression(ann));
            let annotation = match annotation {
                Some(Some(expr)) => Some(self.arena.alloc(expr)),
                Some(None) => return None,
                None => None,
            };
            typed_args.push(TypedArg {
                symbol: self.intern(arg.arg),
                annotation,
                ty: Type::Unknown,
            });
        }
        Some(self.arena.alloc_slice_vec(typed_args))
    }

    /// Lower type parameters
    fn lower_type_parameters(
        &mut self,
        type_params: &[TypeParam<'a>],
    ) -> Option<&'a [TypedTypeParam<'a>]> {
        let mut typed_params = Vec::new();

        for param in type_params {
            let bound = if let Some(b) = param.bound.as_ref() {
                Some(self.lower_expression(b)?)
            } else {
                None
            };
            let default = if let Some(d) = param.default.as_ref() {
                Some(self.lower_expression(d)?)
            } else {
                None
            };

            let bound_ref = bound.map(|b| self.arena.alloc(b));
            let default_ref = default.map(|d| self.arena.alloc(d));

            typed_params.push(TypedTypeParam {
                name: self.intern(param.name),
                bound: bound_ref,
                default: default_ref,
                ty: Type::TypeVar {
                    name: param.name.to_string(),
                    bounds: bound_ref
                        .as_ref()
                        .map(|b| vec![b.ty().clone()])
                        .unwrap_or_default(),
                },
                span: param.span,
            });
        }

        Some(self.arena.alloc_slice_vec(typed_params))
    }

    /// Lower arguments
    fn lower_arguments(&mut self, args: &Arguments<'a>) -> Option<TypedArguments<'a>> {
        // Lower positional arguments
        let posonlyargs = self.lower_typed_args(args.posonlyargs)?;
        let typed_args = self.lower_typed_args(args.args)?;

        // Lower keyword-only arguments
        let kwonlyargs = self.lower_typed_args(args.kwonlyargs)?;

        // Lower defaults
        let defaults = self.lower_expressions(args.defaults)?;
        let mut kw_defaults = Vec::new();
        for expr_opt in args.kw_defaults {
            if let Some(expr) = expr_opt {
                let typed_expr = self.lower_expression(expr)?;
                kw_defaults.push(Some(typed_expr));
            } else {
                kw_defaults.push(None);
            }
        }
        let kw_defaults = self.arena.alloc_slice_vec(kw_defaults);

        // Handle vararg and kwarg
        let vararg = if let Some(arg) = args.vararg.as_ref() {
            let annotation = arg
                .annotation
                .as_ref()
                .map(|ann| self.lower_expression(ann));
            let annotation = match annotation {
                Some(Some(expr)) => Some(self.arena.alloc(expr)),
                Some(None) => return None,
                None => None,
            };
            Some(self.arena.alloc(TypedArg {
                symbol: self.intern(arg.arg),
                annotation,
                ty: Type::Unknown,
            }))
        } else {
            None
        };
        // vararg is already properly handled above

        let kwarg = if let Some(arg) = args.kwarg.as_ref() {
            let annotation = arg
                .annotation
                .as_ref()
                .map(|ann| self.lower_expression(ann));
            let annotation = match annotation {
                Some(Some(expr)) => Some(self.arena.alloc(expr)),
                Some(None) => return None,
                None => None,
            };
            Some(self.arena.alloc(TypedArg {
                symbol: self.intern(arg.arg),
                annotation,
                ty: Type::Unknown,
            }))
        } else {
            None
        };

        Some(TypedArguments {
            posonlyargs,
            args: typed_args,
            vararg,
            kwonlyargs,
            kwarg,
            defaults,
            kw_defaults,
        })
    }

    /// Lower joined string
    fn lower_joined_str(
        &mut self,
        joined_str: &JoinedStrExpr<'a>,
    ) -> Option<TypedJoinedStrExpr<'a>> {
        let values = self.lower_expressions(joined_str.values)?;
        Some(TypedJoinedStrExpr {
            values,
            ty: Type::Str,
            span: joined_str.span,
        })
    }

    /// Lower function definition
    fn lower_function_definition(&mut self, func: &FuncDefStmt<'a>) -> Option<TypedStmt<'a>> {
        // Lower arguments
        let args = self.lower_arguments(&func.args)?;

        // Lower return type annotation
        let returns = func.returns.as_ref().map(|ret| self.lower_expression(ret));
        let returns = match returns {
            Some(Some(expr)) => Some(self.arena.alloc(expr)),
            Some(None) => return None,
            None => None,
        };

        // Lower function body
        let body = self.lower_statements(func.body);

        // Lower decorators
        let decorators = self.lower_expressions(func.decorators)?;

        // Create function name symbol
        let name = self.intern(func.name);

        // Lower type parameters
        let type_params = self.lower_type_parameters(func.type_params).unwrap_or(&[]);

        Some(TypedStmt::FuncDef(TypedFuncDefStmt {
            name,
            type_params,
            args,
            returns,
            body,
            decorators,
            is_async: func.is_async,
            ty: Type::Unknown,
            span: func.span,
            docstring: func.docstring,
        }))
    }

    /// Lower class definition
    fn lower_class_definition(&mut self, class: &ClassDefStmt<'a>) -> Option<TypedStmt<'a>> {
        // Lower base classes
        let bases = self.lower_expressions(class.bases)?;

        // Lower keyword arguments
        let keywords = self.lower_keywords(class.keywords)?;

        // Lower class body
        let body = self.lower_statements(class.body);

        // Lower decorators
        let decorators = self.lower_expressions(class.decorators)?;

        // Create class name symbol
        let name = self.intern(class.name);

        // For now, use empty slices - full MRO computation will be implemented later
        let mro = &[];
        let attributes = &[];
        let methods = &[];

        // Lower type parameters
        let type_params = self.lower_type_parameters(class.type_params).unwrap_or(&[]);

        Some(TypedStmt::ClassDef(TypedClassDefStmt {
            name,
            type_params,
            bases,
            keywords,
            body,
            decorators,
            mro,
            attributes,
            methods,
            ty: Type::Class(self.interner.resolve(name).unwrap_or("Unknown").to_string()),
            span: class.span,
            docstring: class.docstring,
        }))
    }

    /// Lower import statement
    fn lower_import_statement(&mut self, import: &ImportStmt<'a>) -> Option<TypedStmt<'a>> {
        // Lower import names
        let mut names = Vec::new();
        for alias in import.names {
            let name_symbol = self.intern(alias.0);
            let asname_symbol = alias.1.map(|s| self.intern(s));
            names.push((name_symbol, asname_symbol));
        }
        let names = self.arena.alloc_slice_vec(names);

        Some(TypedStmt::Import(TypedImportStmt {
            names,
            span: import.span,
        }))
    }

    /// Lower from statement
    fn lower_from_statement(&mut self, from: &FromStmt<'a>) -> Option<TypedStmt<'a>> {
        // Lower module name
        let module = from.module.map(|m| self.intern(m));

        // Lower import names
        let mut names = Vec::new();
        for alias in from.names {
            let name_symbol = self.intern(alias.0);
            let asname_symbol = alias.1.map(|s| self.intern(s));
            names.push((name_symbol, asname_symbol));
        }
        let names = self.arena.alloc_slice_vec(names);

        Some(TypedStmt::From(TypedFromStmt {
            level: from.level,
            module,
            names,
            span: from.span,
        }))
    }

    /// Lower export statement
    fn lower_export_statement(&mut self, export: &ExportStmt<'a>) -> Option<TypedStmt<'a>> {
        // Lower export names
        let mut names = Vec::new();
        for alias in export.names {
            let name_symbol = self.intern(alias.0);
            let asname_symbol = alias.1.map(|s| self.intern(s));
            names.push((name_symbol, asname_symbol));
        }
        let names = self.arena.alloc_slice_vec(names);

        // Lower module name if present
        let module = export.module.map(|m| self.intern(m));

        Some(TypedStmt::Export(TypedExportStmt {
            names,
            module,
            span: export.span,
        }))
    }

    /// Lower except handlers
    fn lower_except_handlers(
        &mut self,
        handlers: &[ExceptHandler<'a>],
    ) -> Option<&'a [TypedExceptHandler<'a>]> {
        let mut typed_handlers = Vec::new();

        for handler in handlers {
            let typ = handler.typ.as_ref().map(|t| self.lower_expression(t))?;
            let body = self.lower_statements(handler.body);
            typed_handlers.push(TypedExceptHandler {
                typ,
                name: handler.name.map(|n| self.intern(n)),
                body,
                is_exception_group: handler.is_exception_group,
                span: handler.span,
            });
        }

        Some(self.arena.alloc_slice_vec(typed_handlers))
    }

    /// Lower with items
    fn lower_with_items(&mut self, items: &[WithItem<'a>]) -> Option<&'a [TypedWithItem<'a>]> {
        let mut typed_items = Vec::new();

        for item in items {
            let context_expr = self.lower_expression(&item.context_expr)?;
            let context_ref = self.arena.alloc(context_expr);
            let enter_call = self.create_enter_call(context_ref)?;
            let exit_call = self.create_exit_call(context_ref)?;
            let optional_vars = item
                .optional_vars
                .as_ref()
                .map(|v| self.lower_expression(v))?;

            typed_items.push(TypedWithItem {
                context_expr: context_ref.clone(),
                enter_call,
                exit_call,
                optional_vars,
            });
        }

        Some(self.arena.alloc_slice_vec(typed_items))
    }

    /// Create enter call for with statement
    fn create_enter_call(&mut self, context: &'a TypedExpr<'a>) -> Option<TypedExpr<'a>> {
        // Create a call to __enter__() method
        let enter_method = TypedExpr::Attribute(TypedAttributeExpr {
            value: context,
            attr: self.intern("__enter__"),
            ty: Type::Unknown,
            span: context.span(),
        });
        let enter_ref = self.arena.alloc(enter_method);
        Some(TypedExpr::Call(TypedCallExpr {
            func: enter_ref,
            args: &[],
            keywords: &[],
            ty: Type::Unknown,
            span: context.span(),
        }))
    }

    /// Create exit call for with statement
    fn create_exit_call(&mut self, context: &'a TypedExpr<'a>) -> Option<TypedExpr<'a>> {
        // Create a call to __exit__() method
        let exit_method = TypedExpr::Attribute(TypedAttributeExpr {
            value: context,
            attr: self.intern("__exit__"),
            ty: Type::Unknown,
            span: context.span(),
        });
        let exit_ref = self.arena.alloc(exit_method);
        Some(TypedExpr::Call(TypedCallExpr {
            func: exit_ref,
            args: &[],
            keywords: &[],
            ty: Type::Unknown,
            span: context.span(),
        }))
    }

    /// Lower match cases
    fn lower_match_cases(&mut self, cases: &[MatchCase<'a>]) -> Option<&'a [TypedMatchCase<'a>]> {
        let mut typed_cases = Vec::new();

        for case in cases {
            let pattern = self.lower_pattern(&case.pattern)?;
            let guard = case.guard.as_ref().map(|g| self.lower_expression(g))?;
            let body = self.lower_statements(case.body);

            typed_cases.push(TypedMatchCase {
                pattern,
                guard,
                body,
                span: case.span,
            });
        }

        Some(self.arena.alloc_slice_vec(typed_cases))
    }

    /// Lower pattern
    fn lower_pattern(&mut self, pattern: &Pattern<'a>) -> Option<TypedPattern<'a>> {
        match pattern {
            Pattern::MatchAs(match_as) => {
                let pattern = match_as.pattern.as_ref().map(|p| self.lower_pattern(p))?;
                let name = match_as.name.map(|n| self.intern(n));
                let ty = pattern
                    .as_ref()
                    .map(|p| p.ty().clone())
                    .unwrap_or(Type::Any);
                let pattern_ref = pattern.map(|p| self.arena.alloc(p));
                Some(TypedPattern::MatchAs(TypedMatchAsPattern {
                    pattern: pattern_ref,
                    name,
                    ty,
                    span: match_as.span,
                }))
            }
            Pattern::MatchValue(match_value) => {
                let value = self.lower_expression(&match_value.value)?;
                let ty = value.ty().clone();
                Some(TypedPattern::MatchValue(TypedMatchValuePattern {
                    value,
                    ty,
                    span: match_value.span,
                }))
            }
            Pattern::MatchOr(match_or) => {
                let patterns = self.lower_patterns(match_or.patterns)?;
                let ty = if patterns.is_empty() {
                    Type::Never
                } else {
                    Type::union(patterns.iter().map(|p| p.ty().clone()).collect())
                };
                Some(TypedPattern::MatchOr(TypedMatchOrPattern {
                    patterns,
                    ty,
                    span: match_or.span,
                }))
            }
            Pattern::MatchSequence(match_seq) => {
                let patterns = self.lower_patterns(match_seq.patterns)?;
                let ty = if patterns.is_empty() {
                    Type::tuple(vec![])
                } else {
                    Type::tuple(patterns.iter().map(|p| p.ty().clone()).collect())
                };
                Some(TypedPattern::MatchSequence(TypedMatchSequencePattern {
                    patterns,
                    ty,
                    span: match_seq.span,
                }))
            }
            Pattern::MatchMapping(match_map) => {
                let keys = self.lower_expressions(match_map.keys)?;
                let patterns = self.lower_patterns(match_map.patterns)?;
                let rest = match_map.rest.map(|r| self.intern(r));
                let key_ty = if keys.is_empty() {
                    Type::Unknown
                } else {
                    keys[0].ty().clone()
                };
                let value_ty = if patterns.is_empty() {
                    Type::Unknown
                } else {
                    patterns[0].ty().clone()
                };
                let ty = Type::dict(key_ty, value_ty);
                Some(TypedPattern::MatchMapping(TypedMatchMappingPattern {
                    keys,
                    patterns,
                    rest,
                    ty,
                    span: match_map.span,
                }))
            }
            Pattern::MatchClass(match_class) => {
                let cls = self.lower_expression(&match_class.cls)?;
                let patterns = self.lower_patterns(match_class.patterns)?;
                let kwd_attrs = match_class
                    .kwd_attrs
                    .iter()
                    .map(|a| self.intern(a))
                    .collect::<Vec<_>>();
                let kwd_patterns = self.lower_patterns(match_class.kwd_patterns)?;
                let ty = cls.ty().clone();
                Some(TypedPattern::MatchClass(TypedMatchClassPattern {
                    cls,
                    patterns,
                    kwd_attrs: self.arena.alloc_slice_vec(kwd_attrs),
                    kwd_patterns,
                    ty,
                    span: match_class.span,
                }))
            }
            Pattern::MatchSingleton(match_singleton) => {
                let value = match match_singleton.value {
                    MatchSingleton::True => TypedMatchSingleton::True,
                    MatchSingleton::False => TypedMatchSingleton::False,
                    MatchSingleton::None => TypedMatchSingleton::None,
                };
                let ty = match value {
                    TypedMatchSingleton::True | TypedMatchSingleton::False => Type::Bool,
                    TypedMatchSingleton::None => Type::None,
                };
                Some(TypedPattern::MatchSingleton(TypedMatchSingletonPattern {
                    value,
                    ty,
                    span: match_singleton.span,
                }))
            }
        }
    }

    /// Lower patterns
    fn lower_patterns(&mut self, patterns: &[Pattern<'a>]) -> Option<&'a [TypedPattern<'a>]> {
        let mut typed_patterns = Vec::new();

        for pattern in patterns {
            if let Some(typed_pattern) = self.lower_pattern(pattern) {
                typed_patterns.push(typed_pattern);
            } else {
                return None;
            }
        }

        Some(self.arena.alloc_slice_vec(typed_patterns))
    }

    /// Collect classes for MRO computation
    fn collect_classes(&mut self, module: &Module<'a>) {
        self.collect_classes_from_statements(module.body);
    }

    /// Collect classes from a list of statements
    fn collect_classes_from_statements(&mut self, statements: &[Stmt<'a>]) {
        for stmt in statements {
            match stmt {
                Stmt::ClassDef(class) => {
                    // Collect class name for analysis
                    let _name = self.intern(class.name);
                    // Note: Full class analysis is done during lowering, not here
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_classes_from_nested_stmt(stmt);
                }
            }
        }
    }

    /// Collect classes from nested statements
    fn collect_classes_from_nested_stmt(&mut self, stmt: &Stmt<'a>) {
        match stmt {
            Stmt::If(if_stmt) => {
                self.collect_classes_from_statements(if_stmt.body);
                self.collect_classes_from_statements(if_stmt.orelse);
            }
            Stmt::While(while_stmt) => {
                self.collect_classes_from_statements(while_stmt.body);
                self.collect_classes_from_statements(while_stmt.orelse);
            }
            Stmt::For(for_stmt) => {
                self.collect_classes_from_statements(for_stmt.body);
                self.collect_classes_from_statements(for_stmt.orelse);
            }
            Stmt::Try(try_stmt) => {
                self.collect_classes_from_statements(try_stmt.body);
                self.collect_classes_from_statements(try_stmt.orelse);
                self.collect_classes_from_statements(try_stmt.finalbody);
                for handler in try_stmt.handlers {
                    self.collect_classes_from_statements(handler.body);
                }
            }
            Stmt::With(with_stmt) => {
                self.collect_classes_from_statements(with_stmt.body);
            }
            Stmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_classes_from_statements(case.body);
                }
            }
            _ => {}
        }
    }

    /// Extract imports from module
    fn extract_imports(&mut self, statements: &[Stmt<'a>]) -> &'a [TypedImport<'a>] {
        let mut imports = Vec::new();
        self.collect_imports_from_statements(statements, &mut imports);
        self.arena.alloc_slice_vec(imports)
    }

    /// Collect imports from a list of statements
    fn collect_imports_from_statements(
        &mut self,
        statements: &[Stmt<'a>],
        imports: &mut Vec<TypedImport<'a>>,
    ) {
        for stmt in statements {
            match stmt {
                Stmt::Import(import) => {
                    let mut names = Vec::new();
                    for alias in import.names {
                        let name_symbol = self.intern(alias.0);
                        let asname_symbol = alias.1.map(|s| self.intern(s));
                        names.push((name_symbol, asname_symbol));
                    }
                    let names = self.arena.alloc_slice_vec(names);
                    imports.push(TypedImport {
                        module: None,
                        names,
                        level: 0,
                        span: import.span,
                    });
                }
                Stmt::From(from) => {
                    let mut names = Vec::new();
                    for alias in from.names {
                        let name_symbol = self.intern(alias.0);
                        let asname_symbol = alias.1.map(|s| self.intern(s));
                        names.push((name_symbol, asname_symbol));
                    }
                    let names = self.arena.alloc_slice_vec(names);
                    let module = from.module.map(|m| self.intern(m));
                    imports.push(TypedImport {
                        module,
                        names,
                        level: from.level,
                        span: from.span,
                    });
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_imports_from_nested_stmt(stmt, imports);
                }
            }
        }
    }

    /// Collect imports from nested statements
    fn collect_imports_from_nested_stmt(
        &mut self,
        stmt: &Stmt<'a>,
        imports: &mut Vec<TypedImport<'a>>,
    ) {
        match stmt {
            Stmt::If(if_stmt) => {
                self.collect_imports_from_statements(if_stmt.body, imports);
                self.collect_imports_from_statements(if_stmt.orelse, imports);
            }
            Stmt::While(while_stmt) => {
                self.collect_imports_from_statements(while_stmt.body, imports);
                self.collect_imports_from_statements(while_stmt.orelse, imports);
            }
            Stmt::For(for_stmt) => {
                self.collect_imports_from_statements(for_stmt.body, imports);
                self.collect_imports_from_statements(for_stmt.orelse, imports);
            }
            Stmt::Try(try_stmt) => {
                self.collect_imports_from_statements(try_stmt.body, imports);
                self.collect_imports_from_statements(try_stmt.orelse, imports);
                self.collect_imports_from_statements(try_stmt.finalbody, imports);
                for handler in try_stmt.handlers {
                    self.collect_imports_from_statements(handler.body, imports);
                }
            }
            Stmt::With(with_stmt) => {
                self.collect_imports_from_statements(with_stmt.body, imports);
            }
            Stmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_imports_from_statements(case.body, imports);
                }
            }
            _ => {}
        }
    }

    /// Extract exports from module
    fn extract_exports(&mut self, statements: &[Stmt<'a>]) -> &'a [TypedExport<'a>] {
        let mut exports = Vec::new();
        self.collect_exports_from_statements(statements, &mut exports);
        self.arena.alloc_slice_vec(exports)
    }

    /// Collect exports from a list of statements
    fn collect_exports_from_statements(
        &mut self,
        statements: &[Stmt<'a>],
        exports: &mut Vec<TypedExport<'a>>,
    ) {
        for stmt in statements {
            match stmt {
                Stmt::Export(export) => {
                    let mut names = Vec::new();
                    for alias in export.names {
                        let name_symbol = self.intern(alias.0);
                        let asname_symbol = alias.1.map(|s| self.intern(s));
                        names.push((name_symbol, asname_symbol));
                    }
                    let names = self.arena.alloc_slice_vec(names);
                    let module = export.module.map(|m| self.intern(m));
                    exports.push(TypedExport {
                        names,
                        module,
                        span: export.span,
                    });
                }
                _ => {
                    // Recursively check nested statements
                    self.collect_exports_from_nested_stmt(stmt, exports);
                }
            }
        }
    }

    /// Collect exports from nested statements
    fn collect_exports_from_nested_stmt(
        &mut self,
        stmt: &Stmt<'a>,
        exports: &mut Vec<TypedExport<'a>>,
    ) {
        match stmt {
            Stmt::If(if_stmt) => {
                self.collect_exports_from_statements(if_stmt.body, exports);
                self.collect_exports_from_statements(if_stmt.orelse, exports);
            }
            Stmt::While(while_stmt) => {
                self.collect_exports_from_statements(while_stmt.body, exports);
                self.collect_exports_from_statements(while_stmt.orelse, exports);
            }
            Stmt::For(for_stmt) => {
                self.collect_exports_from_statements(for_stmt.body, exports);
                self.collect_exports_from_statements(for_stmt.orelse, exports);
            }
            Stmt::Try(try_stmt) => {
                self.collect_exports_from_statements(try_stmt.body, exports);
                self.collect_exports_from_statements(try_stmt.orelse, exports);
                self.collect_exports_from_statements(try_stmt.finalbody, exports);
                for handler in try_stmt.handlers {
                    self.collect_exports_from_statements(handler.body, exports);
                }
            }
            Stmt::With(with_stmt) => {
                self.collect_exports_from_statements(with_stmt.body, exports);
            }
            Stmt::Match(match_stmt) => {
                for case in match_stmt.cases {
                    self.collect_exports_from_statements(case.body, exports);
                }
            }
            _ => {}
        }
    }

    /// Lower list comprehension by desugaring to explicit loop
    fn lower_list_comprehension(&mut self, list_comp: &ListCompExpr<'a>) -> Option<TypedExpr<'a>> {
        // For now, create a simple list with the expression
        // In a full implementation, this would desugar to:
        // result = []
        // for <comprehension_targets> in <iterable>:
        //     if <conditions>:
        //         result.append(<expression>)
        // return result

        let elt = self.lower_expression(list_comp.elt)?;
        let elt_ref = self.arena.alloc(elt);
        let elts = self.arena.alloc_slice_vec(vec![elt_ref.clone()]);
        Some(TypedExpr::List(TypedListExpr {
            elts,
            ty: Type::List(Box::new(elt_ref.ty().clone())),
            span: list_comp.span,
        }))
    }

    /// Lower dict comprehension by desugaring to explicit loop
    fn lower_dict_comprehension(&mut self, dict_comp: &DictCompExpr<'a>) -> Option<TypedExpr<'a>> {
        // For now, create a simple dict with the key-value pair
        // In a full implementation, this would desugar to:
        // result = {}
        // for <comprehension_targets> in <iterable>:
        //     if <conditions>:
        //         result[<key>] = <value>
        // return result

        let key = self.lower_expression(dict_comp.key)?;
        let value = self.lower_expression(dict_comp.value)?;
        let key_ref = self.arena.alloc(key);
        let value_ref = self.arena.alloc(value);
        let keys = self.arena.alloc_slice_vec(vec![Some(key_ref.clone())]);
        let values = self.arena.alloc_slice_vec(vec![value_ref.clone()]);
        Some(TypedExpr::Dict(TypedDictExpr {
            keys,
            values,
            ty: Type::Dict(
                Box::new(key_ref.ty().clone()),
                Box::new(value_ref.ty().clone()),
            ),
            span: dict_comp.span,
        }))
    }

    /// Lower set comprehension by desugaring to explicit loop
    fn lower_set_comprehension(&mut self, set_comp: &SetCompExpr<'a>) -> Option<TypedExpr<'a>> {
        // For now, create a simple set with the expression
        // In a full implementation, this would desugar to:
        // result = set()
        // for <comprehension_targets> in <iterable>:
        //     if <conditions>:
        //         result.add(<expression>)
        // return result

        let elt = self.lower_expression(set_comp.elt)?;
        let elt_ref = self.arena.alloc(elt);
        let elts = self.arena.alloc_slice_vec(vec![elt_ref.clone()]);
        Some(TypedExpr::Set(TypedSetExpr {
            elts,
            ty: Type::Set(Box::new(elt_ref.ty().clone())),
            span: set_comp.span,
        }))
    }

    /// Lower generator expression by desugaring to explicit generator
    fn lower_generator_expression(
        &mut self,
        gen_exp: &GeneratorExpExpr<'a>,
    ) -> Option<TypedExpr<'a>> {
        // For now, create a simple yield expression
        // In a full implementation, this would desugar to:
        // def _gen():
        //     for <comprehension_targets> in <iterable>:
        //         if <conditions>:
        //             yield <expression>
        // return _gen()

        let elt = self.lower_expression(gen_exp.elt)?;
        let elt_ref = self.arena.alloc(elt);
        Some(TypedExpr::Yield(TypedYieldExpr {
            value: Some(elt_ref),
            ty: Type::Generator(Box::new(elt_ref.ty().clone())),
            span: gen_exp.span,
        }))
    }
}
