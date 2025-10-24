//! Expression display utilities for human-readable error messages

use super::expr::*;

/// Convert an expression to a concise, human-readable string
/// for use in error messages and diagnostics.
pub fn expr_to_string(expr: &Expr) -> String {
    match expr {
        Expr::Constant(c) => format_constant(c.value),
        Expr::Complex(c) => c.value.to_string(),
        Expr::Bytes(b) => b.value.to_string(),
        Expr::Name(n) => n.id.to_string(),
        Expr::BinOp(b) => format!("{} {} {}", expr_to_string(b.left), b.op, expr_to_string(b.right)),
        Expr::UnaryOp(u) => format!("{}{}", u.op, expr_to_string(u.operand)),
        Expr::Compare(c) => {
            let mut result = expr_to_string(c.left);
            for (op, comparator) in c.ops.iter().zip(c.comparators.iter()) {
                result.push_str(&format!(" {} {}", op, expr_to_string(comparator)));
            }
            result
        }
        Expr::Call(c) => {
            let func = expr_to_string(c.func);
            let args = if c.args.len() <= 3 {
                c.args.iter().map(expr_to_string).collect::<Vec<_>>().join(", ")
            } else {
                format!("{} args", c.args.len())
            };
            format!("{}({})", func, args)
        }
        Expr::Attribute(a) => format!("{}.{}", expr_to_string(a.value), a.attr),
        Expr::Subscript(s) => format!("{}[{}]", expr_to_string(s.value), expr_to_string(s.slice)),
        Expr::Slice(s) => {
            let lower = s.lower.map(expr_to_string).unwrap_or_default();
            let upper = s.upper.map(expr_to_string).unwrap_or_default();
            let step = s.step.map(|e| format!(":{}", expr_to_string(e))).unwrap_or_default();
            format!("{}:{}{}", lower, upper, step)
        }
        Expr::List(l) => format_sequence("[", "]", l.elts),
        Expr::Tuple(t) => {
            if t.elts.len() == 1 {
                format!("({},)", expr_to_string(&t.elts[0]))
            } else {
                format_sequence("(", ")", t.elts)
            }
        }
        Expr::Set(s) => {
            if s.elts.is_empty() {
                "set()".to_string()
            } else {
                format_sequence("{", "}", s.elts)
            }
        }
        Expr::Dict(d) => {
            if d.keys.is_empty() {
                "{}".to_string()
            } else if d.keys.len() <= 2 {
                let pairs: Vec<String> = d.keys.iter().zip(d.values.iter())
                    .map(|(k, v)| {
                        let key = k.as_ref().map(expr_to_string).unwrap_or_else(|| "**".to_string());
                        format!("{}: {}", key, expr_to_string(v))
                    })
                    .collect();
                format!("{{{}}}", pairs.join(", "))
            } else {
                format!("{{...{} entries...}}", d.keys.len())
            }
        }
        Expr::Lambda(l) => {
            let params = format_args(&l.args);
            format!("lambda {}: ...", params)
        }
        Expr::IfExp(i) => format!(
            "{} if {} else {}",
            expr_to_string(i.body),
            expr_to_string(i.test),
            expr_to_string(i.orelse)
        ),
        Expr::BoolOp(b) => {
            let values: Vec<String> = b.values.iter().map(expr_to_string).collect();
            values.join(&format!(" {} ", b.op))
        }
        Expr::ListComp(l) => format!("[{} ...]", expr_to_string(l.elt)),
        Expr::DictComp(d) => format!(
            "{{{}: {} ...}}",
            expr_to_string(d.key),
            expr_to_string(d.value)
        ),
        Expr::SetComp(s) => format!("{{{} ...}}", expr_to_string(s.elt)),
        Expr::GeneratorExp(g) => format!("({} ...)", expr_to_string(g.elt)),
        Expr::Await(a) => format!("await {}", expr_to_string(a.value)),
        Expr::NamedExpr(n) => format!(
            "{} := {}",
            expr_to_string(n.target),
            expr_to_string(n.value)
        ),
        Expr::JoinedStr(_) => "f\"...\"".to_string(),
        Expr::FormattedValue(f) => format!("{{{}}}", expr_to_string(f.value)),
        Expr::TString(_) => "t\"...\"".to_string(),
        Expr::Starred(s) => format!("*{}", expr_to_string(s.value)),
        Expr::Yield(y) => {
            if let Some(value) = y.value {
                format!("yield {}", expr_to_string(value))
            } else {
                "yield".to_string()
            }
        }
        Expr::YieldFrom(y) => format!("yield from {}", expr_to_string(y.value)),
        Expr::ModuleIntrospection(m) => format!("__{}", m.function),
    }
}

/// Format a constant value
fn format_constant(value: &str) -> String {
    // Truncate very long string constants
    if value.len() > 50 {
        format!("{}...", &value[..47])
    } else {
        value.to_string()
    }
}

/// Format a sequence (list, tuple, set)
fn format_sequence(open: &str, close: &str, elts: &[Expr]) -> String {
    if elts.is_empty() {
        format!("{}{}", open, close)
    } else if elts.len() <= 3 {
        let items: Vec<String> = elts.iter().map(expr_to_string).collect();
        format!("{}{}{}", open, items.join(", "), close)
    } else {
        format!("{}...{} items...{}", open, elts.len(), close)
    }
}

/// Format function arguments for display
fn format_args(args: &crate::ast::Arguments) -> String {
    let mut params = Vec::new();

    // Positional-only args
    for arg in args.posonlyargs {
        params.push(arg.arg.to_string());
    }

    if !args.posonlyargs.is_empty() {
        params.push("/".to_string());
    }

    // Regular args
    for arg in args.args {
        params.push(arg.arg.to_string());
    }

    // *args
    if let Some(vararg) = &args.vararg {
        params.push(format!("*{}", vararg.arg));
    }

    // Keyword-only args
    for arg in args.kwonlyargs {
        params.push(arg.arg.to_string());
    }

    // **kwargs
    if let Some(kwarg) = &args.kwarg {
        params.push(format!("**{}", kwarg.arg));
    }

    if params.len() > 4 {
        format!("...{} params...", params.len())
    } else {
        params.join(", ")
    }
}

