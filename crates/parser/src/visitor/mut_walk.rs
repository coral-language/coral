//! Walking functions for AST traversal with mutable visitors.
//!
//! These functions provide the default traversal behavior for each AST node type.
//! They are called by the default implementations of the MutVisitor trait methods.
//!
//! This module mirrors the `walk` module but works with mutable visitors that can
//! maintain state during traversal.

use super::mut_visit::MutVisitor;
use crate::ast::*;

/// Walk a module, visiting all statements.
pub fn walk_module<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, module: &Module<'a>) {
    for stmt in module.body {
        visitor.visit_stmt(stmt);
    }
}

/// Walk a statement, visiting all child nodes.
pub fn walk_stmt<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, stmt: &Stmt<'a>) {
    match stmt {
        Stmt::FuncDef(f) => {
            for tp in f.type_params.iter() {
                visitor.visit_type_param(tp);
            }
            visitor.visit_arguments(&f.args);
            for decorator in f.decorators {
                visitor.visit_expr(decorator);
            }
            if let Some(ref returns) = f.returns {
                visitor.visit_expr(returns);
            }
            for s in f.body {
                visitor.visit_stmt(s);
            }
        }
        Stmt::ClassDef(c) => {
            for tp in c.type_params.iter() {
                visitor.visit_type_param(tp);
            }
            for base in c.bases {
                visitor.visit_expr(base);
            }
            for keyword in c.keywords {
                visitor.visit_keyword(keyword);
            }
            for decorator in c.decorators {
                visitor.visit_expr(decorator);
            }
            for s in c.body {
                visitor.visit_stmt(s);
            }
        }
        Stmt::Return(r) => {
            if let Some(ref value) = r.value {
                visitor.visit_expr(value);
            }
        }
        Stmt::Delete(d) => {
            for target in d.targets {
                visitor.visit_expr(target);
            }
        }
        Stmt::Assign(a) => {
            for target in a.targets {
                visitor.visit_expr(target);
            }
            visitor.visit_expr(&a.value);
        }
        Stmt::TypeAlias(ta) => {
            for tp in ta.type_params.iter() {
                visitor.visit_type_param(tp);
            }
            visitor.visit_expr(&ta.value);
        }
        Stmt::AugAssign(a) => {
            visitor.visit_expr(&a.target);
            visitor.visit_expr(&a.value);
        }
        Stmt::AnnAssign(a) => {
            visitor.visit_expr(&a.target);
            visitor.visit_expr(&a.annotation);
            if let Some(ref value) = a.value {
                visitor.visit_expr(value);
            }
        }
        Stmt::For(f) => {
            visitor.visit_expr(&f.target);
            visitor.visit_expr(&f.iter);
            for s in f.body {
                visitor.visit_stmt(s);
            }
            for s in f.orelse {
                visitor.visit_stmt(s);
            }
        }
        Stmt::While(w) => {
            visitor.visit_expr(&w.test);
            for s in w.body {
                visitor.visit_stmt(s);
            }
            for s in w.orelse {
                visitor.visit_stmt(s);
            }
        }
        Stmt::If(i) => {
            visitor.visit_expr(&i.test);
            for s in i.body {
                visitor.visit_stmt(s);
            }
            for s in i.orelse {
                visitor.visit_stmt(s);
            }
        }
        Stmt::With(w) => {
            for item in w.items {
                visitor.visit_withitem(item);
            }
            for s in w.body {
                visitor.visit_stmt(s);
            }
        }
        Stmt::Match(m) => {
            visitor.visit_expr(&m.subject);
            for case in m.cases {
                visitor.visit_match_case(case);
            }
        }
        Stmt::Raise(r) => {
            if let Some(ref exc) = r.exc {
                visitor.visit_expr(exc);
            }
            if let Some(ref cause) = r.cause {
                visitor.visit_expr(cause);
            }
        }
        Stmt::Try(t) => {
            for s in t.body {
                visitor.visit_stmt(s);
            }
            for handler in t.handlers {
                visitor.visit_excepthandler(handler);
            }
            for s in t.orelse {
                visitor.visit_stmt(s);
            }
            for s in t.finalbody {
                visitor.visit_stmt(s);
            }
        }
        Stmt::Assert(a) => {
            visitor.visit_expr(&a.test);
            if let Some(ref msg) = a.msg {
                visitor.visit_expr(msg);
            }
        }
        Stmt::Import(_) | Stmt::From(_) | Stmt::Export(_) => {}
        Stmt::Global(_)
        | Stmt::Nonlocal(_)
        | Stmt::Pass(_)
        | Stmt::Break(_)
        | Stmt::Continue(_)
        | Stmt::Yield(_) => {}
        Stmt::Expr(e) => {
            visitor.visit_expr(&e.value);
        }
    }
}

/// Walk an expression, visiting all child nodes.
pub fn walk_expr<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, expr: &Expr<'a>) {
    match expr {
        Expr::BoolOp(b) => {
            for value in b.values {
                visitor.visit_expr(value);
            }
        }
        Expr::NamedExpr(n) => {
            visitor.visit_expr(n.target);
            visitor.visit_expr(n.value);
        }
        Expr::BinOp(b) => {
            visitor.visit_expr(b.left);
            visitor.visit_expr(b.right);
        }
        Expr::UnaryOp(u) => {
            visitor.visit_expr(u.operand);
        }
        Expr::Lambda(l) => {
            visitor.visit_arguments(&l.args);
            visitor.visit_expr(l.body);
        }
        Expr::IfExp(i) => {
            visitor.visit_expr(i.test);
            visitor.visit_expr(i.body);
            visitor.visit_expr(i.orelse);
        }
        Expr::Dict(d) => {
            for k in d.keys.iter().flatten() {
                visitor.visit_expr(k);
            }
            for value in d.values {
                visitor.visit_expr(value);
            }
        }
        Expr::Set(s) => {
            for elt in s.elts {
                visitor.visit_expr(elt);
            }
        }
        Expr::ListComp(l) => {
            visitor.visit_expr(l.elt);
            for generator in l.generators {
                visitor.visit_comprehension(generator);
            }
        }
        Expr::SetComp(s) => {
            visitor.visit_expr(s.elt);
            for generator in s.generators {
                visitor.visit_comprehension(generator);
            }
        }
        Expr::DictComp(d) => {
            visitor.visit_expr(d.key);
            visitor.visit_expr(d.value);
            for generator in d.generators {
                visitor.visit_comprehension(generator);
            }
        }
        Expr::GeneratorExp(g) => {
            visitor.visit_expr(g.elt);
            for generator in g.generators {
                visitor.visit_comprehension(generator);
            }
        }
        Expr::Await(a) => {
            visitor.visit_expr(a.value);
        }
        Expr::Yield(y) => {
            if let Some(value) = y.value {
                visitor.visit_expr(value);
            }
        }
        Expr::YieldFrom(y) => {
            visitor.visit_expr(y.value);
        }
        Expr::Compare(c) => {
            visitor.visit_expr(c.left);
            for comparator in c.comparators {
                visitor.visit_expr(comparator);
            }
        }
        Expr::Call(c) => {
            visitor.visit_expr(c.func);
            for arg in c.args {
                visitor.visit_expr(arg);
            }
            for keyword in c.keywords {
                visitor.visit_keyword(keyword);
            }
        }
        Expr::FormattedValue(f) => {
            visitor.visit_expr(f.value);
            if let Some(format_spec) = f.format_spec {
                for value in format_spec.values {
                    visitor.visit_expr(value);
                }
            }
        }
        Expr::JoinedStr(j) => {
            for value in j.values {
                visitor.visit_expr(value);
            }
        }
        Expr::Constant(_) | Expr::Complex(_) | Expr::Bytes(_) => {}
        Expr::TString(t) => {
            for value in t.values {
                visitor.visit_expr(value);
            }
        }
        Expr::Attribute(a) => {
            visitor.visit_expr(a.value);
        }
        Expr::Subscript(s) => {
            visitor.visit_expr(s.value);
            visitor.visit_expr(s.slice);
        }
        Expr::Starred(s) => {
            visitor.visit_expr(s.value);
        }
        Expr::Name(_) => {}
        Expr::List(l) => {
            for elt in l.elts {
                visitor.visit_expr(elt);
            }
        }
        Expr::Tuple(t) => {
            for elt in t.elts {
                visitor.visit_expr(elt);
            }
        }
        Expr::Slice(s) => {
            if let Some(lower) = s.lower {
                visitor.visit_expr(lower);
            }
            if let Some(upper) = s.upper {
                visitor.visit_expr(upper);
            }
            if let Some(step) = s.step {
                visitor.visit_expr(step);
            }
        }
        Expr::ModuleIntrospection(_) => {}
    }
}

/// Walk a pattern, visiting all child nodes.
pub fn walk_pattern<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, pattern: &Pattern<'a>) {
    match pattern {
        Pattern::MatchValue(m) => {
            visitor.visit_expr(&m.value);
        }
        Pattern::MatchSingleton(_) => {}
        Pattern::MatchSequence(m) => {
            for pattern in m.patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchMapping(m) => {
            for key in m.keys {
                visitor.visit_expr(key);
            }
            for pattern in m.patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchClass(m) => {
            visitor.visit_expr(&m.cls);
            for pattern in m.patterns {
                visitor.visit_pattern(pattern);
            }
            for pattern in m.kwd_patterns {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchAs(m) => {
            if let Some(ref pattern) = m.pattern {
                visitor.visit_pattern(pattern);
            }
        }
        Pattern::MatchOr(m) => {
            for pattern in m.patterns {
                visitor.visit_pattern(pattern);
            }
        }
    }
}

/// Walk a match case, visiting the pattern, guard, and body.
pub fn walk_match_case<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, case: &MatchCase<'a>) {
    visitor.visit_pattern(&case.pattern);
    if let Some(ref guard) = case.guard {
        visitor.visit_expr(guard);
    }
    for stmt in case.body {
        visitor.visit_stmt(stmt);
    }
}

/// Walk an exception handler, visiting the type and body.
pub fn walk_excepthandler<'a, V: MutVisitor<'a> + ?Sized>(
    visitor: &mut V,
    handler: &ExceptHandler<'a>,
) {
    if let Some(ref typ) = handler.typ {
        visitor.visit_expr(typ);
    }
    for stmt in handler.body {
        visitor.visit_stmt(stmt);
    }
}

/// Walk function arguments, visiting all argument nodes.
pub fn walk_arguments<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, args: &Arguments<'a>) {
    for arg in args.posonlyargs {
        visitor.visit_arg(arg);
    }
    for arg in args.args {
        visitor.visit_arg(arg);
    }
    if let Some(ref vararg) = args.vararg {
        visitor.visit_arg(vararg);
    }
    for arg in args.kwonlyargs {
        visitor.visit_arg(arg);
    }
    if let Some(ref kwarg) = args.kwarg {
        visitor.visit_arg(kwarg);
    }
    for default in args.defaults {
        visitor.visit_expr(default);
    }
    for d in args.kw_defaults.iter().flatten() {
        visitor.visit_expr(d);
    }
}

/// Walk an argument, visiting its annotation.
pub fn walk_arg<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, arg: &Arg<'a>) {
    if let Some(ref annotation) = arg.annotation {
        visitor.visit_expr(annotation);
    }
}

/// Walk a keyword argument, visiting its value.
pub fn walk_keyword<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, keyword: &Keyword<'a>) {
    visitor.visit_expr(&keyword.value);
}

/// Walk a with item, visiting the context expression and optional variable.
pub fn walk_withitem<'a, V: MutVisitor<'a> + ?Sized>(visitor: &mut V, item: &WithItem<'a>) {
    visitor.visit_expr(&item.context_expr);
    if let Some(ref optional_vars) = item.optional_vars {
        visitor.visit_expr(optional_vars);
    }
}

/// Walk a comprehension, visiting target, iter, and filters.
pub fn walk_comprehension<'a, V: MutVisitor<'a> + ?Sized>(
    visitor: &mut V,
    comp: &Comprehension<'a>,
) {
    visitor.visit_expr(&comp.target);
    visitor.visit_expr(&comp.iter);
    for if_clause in comp.ifs {
        visitor.visit_expr(if_clause);
    }
}

/// Walk a type parameter, visiting its bound and default.
pub fn walk_type_param<'a, V: MutVisitor<'a> + ?Sized>(
    visitor: &mut V,
    type_param: &TypeParam<'a>,
) {
    if let Some(ref bound) = type_param.bound {
        visitor.visit_expr(bound);
    }
    if let Some(ref default) = type_param.default {
        visitor.visit_expr(default);
    }
}
