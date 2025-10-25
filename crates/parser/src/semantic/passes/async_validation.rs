//! Async/Await Validation Pass
//!
//! This pass validates the correct usage of async/await in Coral programs:
//! - Ensures `await` expressions only appear inside async functions
//! - Validates `async for` and `async with` are used in async contexts
//! - Detects potentially blocking calls in async contexts
//! - Validates Future types for await expressions
//! - Tracks variable lifetimes across await points

use crate::ast::Expr;
use crate::ast::nodes::{Module, Stmt};
use crate::error::types::Error;
use crate::error::{ErrorKind, error};
use crate::semantic::passes::control_flow::ControlFlowGraph;
use crate::semantic::passes::type_inference::TypeInferenceContext;
use crate::semantic::types::Type;
use std::collections::{HashMap, HashSet};
use text_size::TextRange;

/// Async context information for tracking async function scope
#[derive(Debug, Clone)]
struct AsyncContextInfo {
    in_async_function: bool,
    function_name: Option<String>,
    #[allow(dead_code)]
    span: TextRange,
}

/// Variable lifetime information for tracking across await points
#[derive(Debug, Clone)]
struct VariableLifetime {
    /// Variable name
    #[allow(dead_code)]
    name: String,
    /// Where the variable was declared/assigned
    #[allow(dead_code)]
    declaration_span: TextRange,
    /// Last usage span
    last_use_span: Option<TextRange>,
    /// Whether this variable is still in scope
    in_scope: bool,
}

/// Validator for async/await usage in Coral programs
pub struct AsyncValidator<'a> {
    /// Stack of async contexts (for nested async functions)
    async_context_stack: Vec<AsyncContextInfo>,
    /// Variable lifetime tracking (for async lifetime validation)
    /// Maps variable name to its lifetime information
    variable_lifetimes: HashMap<String, VariableLifetime>,
    /// Set of variables that are live at await points
    /// Used to validate that variables are still valid after await
    variables_before_await: HashMap<TextRange, HashSet<String>>,
    /// Type inference context for getting actual expression types
    type_context: Option<&'a TypeInferenceContext>,
    /// Control flow graphs for flow-sensitive analysis
    cfg_cache: &'a HashMap<String, ControlFlowGraph>,
    /// Current function name for CFG lookup
    current_function: Option<String>,
    /// Accumulated errors during validation
    errors: Vec<Error>,
}

impl<'a> AsyncValidator<'a> {
    /// Create a new async validator with type context and CFG cache
    pub fn new(
        type_context: Option<&'a TypeInferenceContext>,
        cfg_cache: &'a HashMap<String, ControlFlowGraph>,
    ) -> Self {
        Self {
            async_context_stack: vec![AsyncContextInfo {
                in_async_function: false,
                function_name: None,
                span: TextRange::default(),
            }],
            variable_lifetimes: HashMap::new(),
            variables_before_await: HashMap::new(),
            type_context,
            cfg_cache,
            current_function: None,
            errors: Vec::new(),
        }
    }

    /// Check if currently inside an async function
    fn is_in_async_function(&self) -> bool {
        self.async_context_stack
            .last()
            .map(|ctx| ctx.in_async_function)
            .unwrap_or(false)
    }

    /// Get the current async function name for better error messages
    #[allow(dead_code)]
    fn current_async_function_name(&self) -> Option<&str> {
        self.async_context_stack
            .last()
            .and_then(|ctx| ctx.function_name.as_deref())
    }

    /// Enter an async context
    fn enter_async_context(&mut self, function_name: Option<String>, span: TextRange) {
        self.async_context_stack.push(AsyncContextInfo {
            in_async_function: true,
            function_name,
            span,
        });
    }

    /// Exit an async context
    fn exit_async_context(&mut self) {
        // Never pop the base context (module-level)
        if self.async_context_stack.len() > 1 {
            self.async_context_stack.pop();
        }
    }

    /// Track a variable declaration/assignment
    fn track_variable(&mut self, name: &str, span: TextRange) {
        self.variable_lifetimes.insert(
            name.to_string(),
            VariableLifetime {
                name: name.to_string(),
                declaration_span: span,
                last_use_span: None,
                in_scope: true,
            },
        );
    }

    /// Track a variable usage
    fn track_variable_use(&mut self, name: &str, span: TextRange) {
        if let Some(lifetime) = self.variable_lifetimes.get_mut(name) {
            lifetime.last_use_span = Some(span);
        }
    }

    /// Mark a variable as out of scope
    #[allow(dead_code)]
    fn mark_variable_out_of_scope(&mut self, name: &str) {
        if let Some(lifetime) = self.variable_lifetimes.get_mut(name) {
            lifetime.in_scope = false;
        }
    }

    /// Validate that the awaited expression has an awaitable type
    fn validate_await_type(&mut self, expr: &Expr, span: TextRange) {
        let expr_type = self.infer_expr_type(expr);

        if matches!(expr_type, Type::Unknown) {
            return;
        }

        let is_awaitable = match &expr_type {
            Type::Coroutine(_) => true,
            Type::Generator(_) => true,
            Type::Instance(class_name) => {
                if let Some(type_ctx) = self.type_context {
                    type_ctx
                        .get_type_by_span(expr.span().start().into(), expr.span().end().into())
                        .map(|_| true)
                        .unwrap_or_else(|| {
                            ["Future", "Task", "Coroutine"].contains(&class_name.as_str())
                        })
                } else {
                    ["Future", "Task", "Coroutine"].contains(&class_name.as_str())
                }
            }
            _ => false,
        };

        if !is_awaitable {
            self.errors.push(*error(
                ErrorKind::InvalidFutureType {
                    expr: crate::ast::expr_to_string(expr),
                    actual_type: expr_type.display_name(),
                },
                span,
            ));
        }
    }

    /// Infer expression type using the type inference context
    fn infer_expr_type(&self, expr: &Expr) -> Type {
        // Try to get the actual inferred type from the type context
        if let Some(type_ctx) = self.type_context {
            let span = expr.span();
            if let Some(inferred_ty) =
                type_ctx.get_type_by_span(span.start().into(), span.end().into())
            {
                return inferred_ty.clone();
            }
        }

        // Fallback to pattern-based inference for known async patterns
        match expr {
            // Async function calls return coroutines
            Expr::Call(call) => {
                // If we have type context, we should have gotten the type above
                // This is a fallback for when type inference hasn't run yet
                if let Some(type_ctx) = self.type_context {
                    let func_span = call.func.span();
                    if let Some(func_ty) =
                        type_ctx.get_type_by_span(func_span.start().into(), func_span.end().into())
                    {
                        // If it's a coroutine-returning function, return Coroutine type
                        if matches!(func_ty, Type::Coroutine(_)) {
                            return func_ty.clone();
                        }
                    }
                }
                Type::Unknown
            }
            // Attribute access on known async libraries
            Expr::Attribute(attr) => {
                if let Expr::Name(name) = attr.value {
                    // Check for known async library patterns
                    if ["asyncio", "aiohttp", "aiofiles"].contains(&name.id) {
                        return Type::Coroutine(Box::new(Type::Unknown));
                    }
                }
                Type::Unknown
            }
            // Name could be a coroutine if it's an async function reference
            Expr::Name(_) => Type::Unknown,
            // For other expressions, conservatively assume Unknown
            _ => Type::Unknown,
        }
    }

    /// Validate variable lifetimes across an await point using flow analysis
    fn validate_lifetime_across_await(&mut self, await_span: TextRange) {
        // Record which variables are live before this await
        let live_vars: HashSet<String> = self
            .variable_lifetimes
            .iter()
            .filter(|(_, lifetime)| lifetime.in_scope)
            .map(|(name, _)| name.clone())
            .collect();

        self.variables_before_await
            .insert(await_span, live_vars.clone());

        // Use CFG for flow-sensitive analysis if available
        if let Some(func_name) = &self.current_function
            && let Some(cfg) = self.cfg_cache.get(func_name)
        {
            // Find all variables that are used after this await point
            let vars_used_after_await = self.find_variables_used_after_span(cfg, await_span);

            // Check if any resource-holding variables are used after await
            for var_name in &vars_used_after_await {
                if let Some(lifetime) = self.variable_lifetimes.get(var_name)
                    && lifetime.in_scope
                    && self.is_potentially_unsafe_across_await(var_name)
                    && lifetime.declaration_span.end() < await_span.start()
                    && let Some(var_type) = self.type_context.and_then(|ctx| {
                        ctx.get_type_by_span(
                            lifetime.declaration_span.start().into(),
                            lifetime.declaration_span.end().into(),
                        )
                        .cloned()
                    })
                {
                    let type_name = var_type.display_name();
                    let var_display = format!("'{}' of type '{}'", var_name, type_name);

                    self.errors.push(*error(
                        ErrorKind::InvalidFutureType {
                            expr: var_display,
                            actual_type: format!(
                                "Variable may become invalid across await. \
                                 {} resources cannot safely be held across await points.",
                                type_name
                            ),
                        },
                        await_span,
                    ));
                }
            }
        }
    }

    /// Find variables used after a given span using CFG
    fn find_variables_used_after_span(
        &self,
        cfg: &ControlFlowGraph,
        span: TextRange,
    ) -> HashSet<String> {
        let mut used_vars = HashSet::new();

        // Iterate through all blocks in the CFG
        for block in cfg.blocks.values() {
            // Check each statement in the block
            for stmt_info in &block.statements {
                // If the statement comes after the await span, collect its uses
                if stmt_info.span.start() > span.end() {
                    for var in &stmt_info.uses {
                        used_vars.insert(var.clone());
                    }
                }
            }
        }

        used_vars
    }

    /// Check if a type is unsafe to hold across await points
    fn is_type_unsafe_across_await(&self, ty: &Type) -> bool {
        match ty {
            // File handles, network streams, locks are unsafe across await
            Type::Class(name) => {
                ["File", "Lock", "Mutex", "RwLock", "Socket"].contains(&name.as_str())
            }
            _ => false,
        }
    }

    /// Check if a variable is potentially unsafe to use across await points
    /// (e.g., holds locks, file handles, or local references)
    fn is_potentially_unsafe_across_await(&self, var_name: &str) -> bool {
        if let Some(lifetime) = self.variable_lifetimes.get(var_name)
            && let Some(var_type) = self.type_context.and_then(|ctx| {
                ctx.get_type_by_span(
                    lifetime.declaration_span.start().into(),
                    lifetime.declaration_span.end().into(),
                )
                .cloned()
            })
        {
            self.is_type_unsafe_across_await(&var_type)
        } else {
            false
        }
    }

    /// Validate a module
    pub fn validate_module(&mut self, module: &Module) -> Vec<Error> {
        // Validate all statements in the module
        for stmt in module.body {
            self.validate_stmt(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    /// Validate an expression (recursively traverses nested expressions)
    fn validate_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Await(await_expr) => {
                // Validate await is in async context
                if !self.is_in_async_function() {
                    self.errors
                        .push(*error(ErrorKind::AwaitOutsideAsync, await_expr.span));
                }
                // Validate the awaited expression has an awaitable type
                self.validate_await_type(await_expr.value, await_expr.span);
                // Validate variable lifetimes across this await point
                self.validate_lifetime_across_await(await_expr.span);
                // Recursively validate the awaited expression
                self.validate_expr(await_expr.value);
            }
            Expr::Call(call_expr) => {
                // Check for blocking calls in async context
                if self.is_in_async_function() {
                    self.check_blocking_call(call_expr);
                }
                // Validate function and arguments
                self.validate_expr(call_expr.func);
                for arg in call_expr.args {
                    self.validate_expr(arg);
                }
                for keyword in call_expr.keywords {
                    self.validate_expr(&keyword.value);
                }
            }
            Expr::BinOp(binop) => {
                self.validate_expr(binop.left);
                self.validate_expr(binop.right);
            }
            Expr::UnaryOp(unop) => {
                self.validate_expr(unop.operand);
            }
            Expr::IfExp(if_exp) => {
                self.validate_expr(if_exp.test);
                self.validate_expr(if_exp.body);
                self.validate_expr(if_exp.orelse);
            }
            Expr::Lambda(lambda) => {
                self.validate_expr(lambda.body);
            }
            Expr::ListComp(comp) => {
                self.validate_expr(comp.elt);
                for generator in comp.generators {
                    self.validate_expr(&generator.iter);
                    for if_clause in generator.ifs {
                        self.validate_expr(if_clause);
                    }
                }
            }
            Expr::SetComp(comp) => {
                self.validate_expr(comp.elt);
                for generator in comp.generators {
                    self.validate_expr(&generator.iter);
                    for if_clause in generator.ifs {
                        self.validate_expr(if_clause);
                    }
                }
            }
            Expr::DictComp(comp) => {
                self.validate_expr(comp.key);
                self.validate_expr(comp.value);
                for generator in comp.generators {
                    self.validate_expr(&generator.iter);
                    for if_clause in generator.ifs {
                        self.validate_expr(if_clause);
                    }
                }
            }
            Expr::GeneratorExp(comp) => {
                self.validate_expr(comp.elt);
                for generator in comp.generators {
                    self.validate_expr(&generator.iter);
                    for if_clause in generator.ifs {
                        self.validate_expr(if_clause);
                    }
                }
            }
            Expr::Yield(yield_expr) => {
                if let Some(value) = &yield_expr.value {
                    self.validate_expr(value);
                }
            }
            Expr::YieldFrom(yield_from) => {
                self.validate_expr(yield_from.value);
            }
            Expr::Compare(compare) => {
                self.validate_expr(compare.left);
                for comparator in compare.comparators {
                    self.validate_expr(comparator);
                }
            }
            Expr::BoolOp(bool_op) => {
                for value in bool_op.values {
                    self.validate_expr(value);
                }
            }
            Expr::Subscript(subscript) => {
                self.validate_expr(subscript.value);
                self.validate_expr(subscript.slice);
            }
            Expr::Slice(slice) => {
                if let Some(lower) = &slice.lower {
                    self.validate_expr(lower);
                }
                if let Some(upper) = &slice.upper {
                    self.validate_expr(upper);
                }
                if let Some(step) = &slice.step {
                    self.validate_expr(step);
                }
            }
            Expr::Attribute(attr) => {
                self.validate_expr(attr.value);
            }
            Expr::List(list) => {
                for elt in list.elts {
                    self.validate_expr(elt);
                }
            }
            Expr::Tuple(tuple) => {
                for elt in tuple.elts {
                    self.validate_expr(elt);
                }
            }
            Expr::Set(set) => {
                for elt in set.elts {
                    self.validate_expr(elt);
                }
            }
            Expr::Dict(dict) => {
                for key in dict.keys.iter().flatten() {
                    self.validate_expr(key);
                }
                for value in dict.values {
                    self.validate_expr(value);
                }
            }
            Expr::FormattedValue(fval) => {
                self.validate_expr(fval.value);
            }
            Expr::JoinedStr(joined) => {
                for value in joined.values {
                    self.validate_expr(value);
                }
            }
            Expr::NamedExpr(named) => {
                self.validate_expr(named.value);
            }
            Expr::Starred(starred) => {
                self.validate_expr(starred.value);
            }
            Expr::Name(name) => {
                // Track variable usage
                self.track_variable_use(name.id, name.span);
            }
            // Literals (Constant, Complex, Bytes, TString), and ModuleIntrospection
            // don't contain nested expressions
            _ => {}
        }
    }

    /// Validate a statement (recursively traverses nested statements)
    fn validate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FuncDef(func_def) => {
                let func_name = func_def.name.to_string();

                if func_def.is_async {
                    // Enter async context for async functions
                    let name = Some(func_name.clone());
                    self.enter_async_context(name, func_def.span);

                    // Set current function for CFG lookup
                    let old_function = self.current_function.replace(func_name);

                    // Validate function body
                    for stmt in func_def.body {
                        self.validate_stmt(stmt);
                    }

                    // Restore previous function
                    self.current_function = old_function;

                    // Exit async context
                    self.exit_async_context();
                } else {
                    // Regular function - validate body but don't enter async context
                    // Set current function for CFG lookup
                    let old_function = self.current_function.replace(func_name);

                    for stmt in func_def.body {
                        self.validate_stmt(stmt);
                    }

                    // Restore previous function
                    self.current_function = old_function;
                }
            }
            Stmt::For(for_stmt) => {
                // Validate async for is in async context
                if for_stmt.is_async && !self.is_in_async_function() {
                    self.errors
                        .push(*error(ErrorKind::AsyncForOutsideAsync, for_stmt.span));
                }
                // Validate body
                for stmt in for_stmt.body {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::With(with_stmt) => {
                // Validate async with is in async context
                if with_stmt.is_async && !self.is_in_async_function() {
                    self.errors
                        .push(*error(ErrorKind::AsyncWithOutsideAsync, with_stmt.span));
                }
                // Validate body
                for stmt in with_stmt.body {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::Expr(expr_stmt) => {
                self.validate_expr(&expr_stmt.value);
            }
            Stmt::Assign(assign_stmt) => {
                // Track variable assignments
                for target in assign_stmt.targets {
                    if let Expr::Name(name) = target {
                        self.track_variable(name.id, target.span());
                    }
                }
                self.validate_expr(&assign_stmt.value);
            }
            Stmt::AnnAssign(ann_assign) => {
                // Track annotated variable assignment
                if let Expr::Name(name) = &ann_assign.target {
                    self.track_variable(name.id, ann_assign.target.span());
                }
                if let Some(value) = &ann_assign.value {
                    self.validate_expr(value);
                }
            }
            Stmt::Return(ret_stmt) => {
                if let Some(value) = &ret_stmt.value {
                    self.validate_expr(value);
                }
            }
            Stmt::If(if_stmt) => {
                self.validate_expr(&if_stmt.test);
                for stmt in if_stmt.body {
                    self.validate_stmt(stmt);
                }
                for stmt in if_stmt.orelse {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::While(while_stmt) => {
                self.validate_expr(&while_stmt.test);
                for stmt in while_stmt.body {
                    self.validate_stmt(stmt);
                }
                for stmt in while_stmt.orelse {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::Try(try_stmt) => {
                for stmt in try_stmt.body {
                    self.validate_stmt(stmt);
                }
                for handler in try_stmt.handlers {
                    for stmt in handler.body {
                        self.validate_stmt(stmt);
                    }
                }
                for stmt in try_stmt.orelse {
                    self.validate_stmt(stmt);
                }
                for stmt in try_stmt.finalbody {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::Match(match_stmt) => {
                self.validate_expr(&match_stmt.subject);
                for case in match_stmt.cases {
                    for stmt in case.body {
                        self.validate_stmt(stmt);
                    }
                    if let Some(guard) = &case.guard {
                        self.validate_expr(guard);
                    }
                }
            }
            Stmt::ClassDef(class_def) => {
                // Validate base classes
                for base in class_def.bases {
                    self.validate_expr(base);
                }
                // Validate decorators
                for decorator in class_def.decorators {
                    self.validate_expr(decorator);
                }
                // Validate class body
                for stmt in class_def.body {
                    self.validate_stmt(stmt);
                }
            }
            Stmt::Raise(raise_stmt) => {
                if let Some(exc) = &raise_stmt.exc {
                    self.validate_expr(exc);
                }
                if let Some(cause) = &raise_stmt.cause {
                    self.validate_expr(cause);
                }
            }
            Stmt::Assert(assert_stmt) => {
                self.validate_expr(&assert_stmt.test);
                if let Some(msg) = &assert_stmt.msg {
                    self.validate_expr(msg);
                }
            }
            Stmt::Delete(delete_stmt) => {
                for target in delete_stmt.targets {
                    self.validate_expr(target);
                }
            }
            Stmt::AugAssign(aug_assign) => {
                self.validate_expr(&aug_assign.value);
            }
            Stmt::Yield(yield_stmt) => {
                if let Some(value) = &yield_stmt.value {
                    self.validate_expr(value);
                }
            }
            // Pass, Break, Continue, Import, From, Export, Global, Nonlocal, TypeAlias
            // don't contain expressions or nested statements that need validation
            _ => {}
        }
    }

    /// Check for blocking calls in async context
    fn check_blocking_call(&mut self, call_expr: &crate::ast::expr::CallExpr) {
        // Extract function name for blocking call detection
        let (module_name, func_name) = match call_expr.func {
            Expr::Name(name) => (None, Some(name.id)),
            Expr::Attribute(attr) => {
                // Handle cases like time.sleep, requests.get, etc.
                if let Expr::Name(obj) = attr.value {
                    (Some(obj.id), Some(attr.attr))
                } else {
                    (None, Some(attr.attr))
                }
            }
            _ => (None, None),
        };

        if let Some(func) = func_name {
            // Known async-safe modules/patterns - don't flag these
            let async_safe_modules = &[
                "asyncio", "aiofiles", "aiohttp", "httpx", "aiomysql", "aiopg",
            ];

            // Check if this is from a known async library
            if let Some(module) = module_name
                && async_safe_modules.contains(&module)
            {
                return; // This is async-safe, skip validation
            }

            // Blocking module.function combinations
            let blocking_module_funcs = &[
                ("time", "sleep"),
                ("threading", "Thread"),
                ("socket", "recv"),
                ("socket", "send"),
                ("socket", "accept"),
                ("socket", "connect"),
                ("requests", "get"),
                ("requests", "post"),
                ("requests", "put"),
                ("requests", "delete"),
                ("requests", "patch"),
                ("urllib", "urlopen"),
                ("subprocess", "run"),
                ("subprocess", "call"),
                ("subprocess", "check_output"),
            ];

            // Check module.function patterns
            if let Some(module) = module_name {
                for (block_mod, block_func) in blocking_module_funcs {
                    if module == *block_mod && func == *block_func {
                        let full_name = format!("{}.{}", module, func);
                        self.errors.push(*error(
                            ErrorKind::BlockingCallInAsync { call: full_name },
                            call_expr.span,
                        ));
                        return;
                    }
                }
            }

            // Blocking built-in functions (no module prefix)
            let blocking_builtins = &[
                "open",  // File I/O
                "input", // Console input
                "sleep", // If used without module
            ];

            if module_name.is_none() && blocking_builtins.contains(&func) {
                self.errors.push(*error(
                    ErrorKind::BlockingCallInAsync {
                        call: func.to_string(),
                    },
                    call_expr.span,
                ));
            }
        }
    }
}

// Note: We don't implement Visitor trait as it requires immutable self,
// but we need mutable self to collect errors. Instead, we provide our own
// traversal methods.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    #[test]
    fn test_detects_blocking_sleep_in_async() {
        let source = r#"
async def fetch_data():
    time.sleep(1)
    return "done"
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking call time.sleep in async context"
        );
    }

    #[test]
    fn test_detects_blocking_open_in_async() {
        let source = r#"
async def read_file():
    f = open("file.txt")
    return f.read()
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking call open in async context"
        );
    }

    #[test]
    fn test_detects_blocking_requests_in_async() {
        let source = r#"
async def fetch_data():
    response = requests.get("http://example.com")
    return response.text()
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking call requests.get in async context"
        );
    }

    #[test]
    fn test_no_false_positive_in_regular_function() {
        let source = r#"
def regular_function():
    time.sleep(1)
    return "done"
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            !errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should not report blocking calls in regular functions"
        );
    }

    #[test]
    fn test_nested_async_context_tracking() {
        let source = r#"
async def outer():
    result = await operation()

    async def inner():
        time.sleep(1)  # Should be detected
        return data

    return await inner()
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking calls in nested async functions"
        );
    }

    #[test]
    fn test_async_context_exits_correctly() {
        let source = r#"
async def async_func():
    await operation()

def regular_func():
    time.sleep(1)  # Should NOT be detected
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            !errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Context should exit async properly, not detecting in regular function"
        );
    }

    #[test]
    fn test_blocking_call_in_nested_expression() {
        let source = r#"
async def process():
    result = compute(time.sleep(1))
    return result
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking calls in nested expressions"
        );
    }

    #[test]
    fn test_blocking_call_in_comprehension() {
        let source = r#"
async def process():
    results = [open(f) for f in files]
    return results
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        assert!(
            errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::BlockingCallInAsync { .. })),
            "Should detect blocking calls in comprehensions"
        );
    }

    #[test]
    fn test_stack_never_becomes_empty() {
        let source = r#"
async def level1():
    async def level2():
        async def level3():
            await operation()
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let _errors = validator.validate_module(parse_result.module);

        // Stack should still have base context after validation
        assert!(
            !validator.async_context_stack.is_empty(),
            "Stack should never be empty"
        );
        assert_eq!(
            validator.async_context_stack.len(),
            1,
            "Stack should return to base context (1 element)"
        );
    }

    #[test]
    fn test_tracks_variable_declarations() {
        let source = r#"
async def process():
    x = 10
    y = await get_value()
    return x + y
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let _errors = validator.validate_module(parse_result.module);

        // Check that variables were tracked
        assert!(
            validator.variable_lifetimes.contains_key("x"),
            "Variable 'x' should be tracked"
        );
        assert!(
            validator.variable_lifetimes.contains_key("y"),
            "Variable 'y' should be tracked"
        );
    }

    #[test]
    fn test_tracks_variable_usage_across_await() {
        let source = r#"
async def process():
    data = fetch_initial()
    result = await process_data()
    final = combine(data, result)
    return final
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let _errors = validator.validate_module(parse_result.module);

        // Variables should be tracked with usage information
        assert!(
            validator.variable_lifetimes.contains_key("data"),
            "Variable 'data' should be tracked"
        );
        assert!(
            validator.variable_lifetimes.contains_key("result"),
            "Variable 'result' should be tracked"
        );
    }

    #[test]
    fn test_await_validates_expression_type() {
        // This test verifies that the await validation type checker is called
        // Type inference integration allows proper validation when context is provided
        let source = r#"
async def process():
    result = await compute()
    return result
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let errors = validator.validate_module(parse_result.module);

        // Should not report InvalidFutureType for unknown types (conservative approach)
        assert!(
            !errors
                .iter()
                .any(|e| matches!(e.kind, ErrorKind::InvalidFutureType { .. })),
            "Should not report type errors for unknown expressions"
        );
    }

    #[test]
    fn test_lifetime_tracking_with_await_points() {
        let source = r#"
async def multi_await():
    x = 1
    y = await first()
    z = await second()
    return x + y + z
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let _errors = validator.validate_module(parse_result.module);

        // Should track await points
        assert!(
            !validator.variables_before_await.is_empty(),
            "Should record variables live before await points"
        );
    }

    #[test]
    fn test_annotated_assignment_tracking() {
        let source = r#"
async def typed_function():
    x: int = 10
    y: str = await get_string()
    return y
"#;
        let parse_result = parse(source).expect("Failed to parse");
        let cfg_cache = HashMap::new();
        let mut validator = AsyncValidator::new(None, &cfg_cache);
        let _errors = validator.validate_module(parse_result.module);

        // Annotated assignments should also be tracked
        assert!(
            validator.variable_lifetimes.contains_key("x"),
            "Annotated variable 'x' should be tracked"
        );
        assert!(
            validator.variable_lifetimes.contains_key("y"),
            "Annotated variable 'y' should be tracked"
        );
    }
}
