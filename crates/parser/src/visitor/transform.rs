//! Arena-aware AST transformation helpers.
//!
//! This module provides utilities for creating modified AST nodes in Coral's
//! arena-allocated AST system. Since the AST is immutable, transformations
//! require allocating new nodes rather than mutating existing ones.
//!
//! ## Basic Usage
//!
//! ```rust,ignore
//! use coral_parser::visitor::transform::Transformer;
//!
//! let arena = coral_parser::Arena::new();
//! let transformer = Transformer::new(&arena);
//!
//! // Transform an expression
//! let new_expr = transformer.transform_expr(old_expr, |expr| {
//!     match expr {
//!         Expr::Constant(c) if c.value == "42" => {
//!             Expr::Constant(ConstantExpr {
//!                 value: "forty-two",
//!                 span: c.span,
//!             })
//!         }
//!         other => other.clone(),
//!     }
//! });
//! ```
//!
//! ## Common Patterns
//!
//! ### Constant Folding
//!
//! ```rust,ignore
//! let folded = transformer.transform_expr(expr, |expr| {
//!     match expr {
//!         Expr::BinOp(b) if matches!((b.left, b.right), (Expr::Constant(_), Expr::Constant(_))) => {
//!             // Evaluate constant expressions
//!             if let (Some(left_val), Some(right_val)) = (eval_constant(b.left), eval_constant(b.right)) {
//!                 Expr::Constant(ConstantExpr {
//!                     value: &format!("{}", left_val + right_val),
//!                     span: b.span,
//!                 })
//!             } else {
//!                 expr.clone()
//!             }
//!         }
//!         other => other.clone(),
//!     }
//! });
//! ```
//!
//! ### Desugaring
//!
//! ```rust,ignore
//! // Convert `x += 1` to `x = x + 1`
//! let desugared = transformer.transform_stmt(stmt, |stmt| {
//!     match stmt {
//!         Stmt::AugAssign(a) => {
//!             let target = transformer.clone_expr(&a.target);
//!             let binop = transformer.alloc_binop(
//!                 target,
//!                 "+",
//!                 transformer.clone_expr(&a.value),
//!                 a.span,
//!             );
//!             Stmt::Assign(AssignStmt {
//!                 targets: transformer.alloc_slice(&[a.target.clone()]),
//!                 value: binop,
//!                 span: a.span,
//!             })
//!         }
//!         other => other.clone(),
//!     }
//! });
//! ```

use crate::arena::Arena;
use crate::ast::*;
use smallvec::SmallVec;
use text_size::TextRange;

/// Arena-aware AST transformer.
///
/// Provides helper methods for creating modified AST nodes while properly
/// managing arena allocation. All transformation methods return references
/// to arena-allocated nodes.
pub struct Transformer<'a> {
    arena: &'a Arena,
}

impl<'a> Transformer<'a> {
    /// Create a new transformer with the given arena.
    pub fn new(arena: &'a Arena) -> Self {
        Transformer { arena }
    }

    /// Get the underlying arena.
    pub fn arena(&self) -> &'a Arena {
        self.arena
    }

    /// Transform an expression using a closure.
    ///
    /// The closure receives the original expression and should return a new expression.
    /// The result is allocated in the arena.
    pub fn transform_expr<F>(&self, expr: &Expr<'a>, f: F) -> &'a Expr<'a>
    where
        F: FnOnce(&Expr<'a>) -> Expr<'a>,
    {
        self.arena.alloc(f(expr))
    }

    /// Transform a statement using a closure.
    ///
    /// The closure receives the original statement and should return a new statement.
    /// The result is allocated in the arena.
    pub fn transform_stmt<F>(&self, stmt: &Stmt<'a>, f: F) -> &'a Stmt<'a>
    where
        F: FnOnce(&Stmt<'a>) -> Stmt<'a>,
    {
        self.arena.alloc(f(stmt))
    }

    /// Transform a pattern using a closure.
    pub fn transform_pattern<F>(&self, pattern: &Pattern<'a>, f: F) -> &'a Pattern<'a>
    where
        F: FnOnce(&Pattern<'a>) -> Pattern<'a>,
    {
        self.arena.alloc(f(pattern))
    }

    /// Create a deep clone of an expression in the arena.
    pub fn clone_expr(&self, expr: &Expr<'a>) -> &'a Expr<'a> {
        self.arena.alloc(expr.clone())
    }

    /// Create a deep clone of a statement in the arena.
    pub fn clone_stmt(&self, stmt: &Stmt<'a>) -> &'a Stmt<'a> {
        self.arena.alloc(stmt.clone())
    }

    /// Create a deep clone of a pattern in the arena.
    pub fn clone_pattern(&self, pattern: &Pattern<'a>) -> &'a Pattern<'a> {
        self.arena.alloc(pattern.clone())
    }

    /// Transform all expressions in a slice, returning a new slice.
    pub fn map_exprs<F>(&self, exprs: &[&Expr<'a>], f: F) -> &'a [Expr<'a>]
    where
        F: Fn(&Expr<'a>) -> Expr<'a>,
    {
        let transformed: SmallVec<[Expr<'a>; 8]> = exprs.iter().map(|e| f(e)).collect();
        self.arena.alloc_slice_iter(transformed)
    }

    /// Transform all statements in a slice, returning a new slice.
    pub fn map_stmts<F>(&self, stmts: &[&Stmt<'a>], f: F) -> &'a [Stmt<'a>]
    where
        F: Fn(&Stmt<'a>) -> Stmt<'a>,
    {
        let transformed: SmallVec<[Stmt<'a>; 8]> = stmts.iter().map(|s| f(s)).collect();
        self.arena.alloc_slice_iter(transformed)
    }

    /// Replace one expression in a slice with a new expression, cloning the rest.
    pub fn replace_expr_in_slice(
        &self,
        exprs: &[&Expr<'a>],
        index: usize,
        new_expr: &Expr<'a>,
    ) -> &'a [Expr<'a>] {
        let transformed: SmallVec<[Expr<'a>; 8]> = exprs
            .iter()
            .enumerate()
            .map(|(i, e)| {
                if i == index {
                    new_expr.clone()
                } else {
                    (*e).clone()
                }
            })
            .collect();
        self.arena.alloc_slice_iter(transformed)
    }

    /// Replace one statement in a slice with a new statement, cloning the rest.
    pub fn replace_stmt_in_slice(
        &self,
        stmts: &[&Stmt<'a>],
        index: usize,
        new_stmt: &Stmt<'a>,
    ) -> &'a [Stmt<'a>] {
        let transformed: SmallVec<[Stmt<'a>; 8]> = stmts
            .iter()
            .enumerate()
            .map(|(i, s)| {
                if i == index {
                    new_stmt.clone()
                } else {
                    (*s).clone()
                }
            })
            .collect();
        self.arena.alloc_slice_iter(transformed)
    }

    /// Create a constant expression.
    pub fn alloc_constant(&self, value: &str, span: TextRange) -> &'a Expr<'a> {
        self.arena.alloc(Expr::Constant(ConstantExpr {
            value: self.arena.alloc_str(value),
            span,
        }))
    }

    /// Create a name expression.
    pub fn alloc_name(&self, id: &str, span: TextRange) -> &'a Expr<'a> {
        self.arena.alloc(Expr::Name(NameExpr {
            id: self.arena.alloc_str(id),
            span,
        }))
    }

    /// Create a binary operation expression.
    pub fn alloc_binop(
        &self,
        left: &'a Expr<'a>,
        op: &str,
        right: &'a Expr<'a>,
        span: TextRange,
    ) -> &'a Expr<'a> {
        self.arena.alloc(Expr::BinOp(BinOpExpr {
            left,
            op: self.arena.alloc_str(op),
            right,
            span,
        }))
    }

    /// Create a unary operation expression.
    pub fn alloc_unaryop(&self, op: &str, operand: &'a Expr<'a>, span: TextRange) -> &'a Expr<'a> {
        self.arena.alloc(Expr::UnaryOp(UnaryOpExpr {
            op: self.arena.alloc_str(op),
            operand,
            span,
        }))
    }

    /// Create an assignment statement.
    pub fn alloc_assign(
        &self,
        targets: &'a [&'a Expr<'a>],
        value: &'a Expr<'a>,
        span: TextRange,
    ) -> &'a Stmt<'a> {
        self.arena.alloc(Stmt::Assign(AssignStmt {
            targets: self
                .arena
                .alloc_slice_iter(targets.iter().map(|t| (*t).clone())),
            value: value.clone(),
            span,
        }))
    }

    /// Create an augmented assignment statement.
    pub fn alloc_aug_assign(
        &self,
        target: &'a Expr<'a>,
        op: &str,
        value: &'a Expr<'a>,
        span: TextRange,
    ) -> &'a Stmt<'a> {
        self.arena.alloc(Stmt::AugAssign(AugAssignStmt {
            target: target.clone(),
            op: self.arena.alloc_str(op),
            value: value.clone(),
            span,
        }))
    }

    /// Create a return statement.
    pub fn alloc_return(&self, value: Option<&'a Expr<'a>>, span: TextRange) -> &'a Stmt<'a> {
        self.arena.alloc(Stmt::Return(ReturnStmt {
            value: value.cloned(),
            span,
        }))
    }

    /// Create an if statement.
    pub fn alloc_if(
        &self,
        test: &'a Expr<'a>,
        body: &'a [&'a Stmt<'a>],
        orelse: &'a [&'a Stmt<'a>],
        span: TextRange,
    ) -> &'a Stmt<'a> {
        self.arena.alloc(Stmt::If(IfStmt {
            test: test.clone(),
            body: self
                .arena
                .alloc_slice_iter(body.iter().map(|s| (*s).clone())),
            orelse: self
                .arena
                .alloc_slice_iter(orelse.iter().map(|s| (*s).clone())),
            span,
        }))
    }

    /// Create a function call expression.
    pub fn alloc_call(
        &self,
        func: &'a Expr<'a>,
        args: &'a [&'a Expr<'a>],
        keywords: &'a [Keyword<'a>],
        span: TextRange,
    ) -> &'a Expr<'a> {
        self.arena.alloc(Expr::Call(CallExpr {
            func,
            args: self
                .arena
                .alloc_slice_iter(args.iter().map(|a| (*a).clone())),
            keywords: self.arena.alloc_slice_iter(keywords.iter().cloned()),
            span,
        }))
    }

    /// Create an attribute access expression.
    pub fn alloc_attribute(
        &self,
        value: &'a Expr<'a>,
        attr: &str,
        span: TextRange,
    ) -> &'a Expr<'a> {
        self.arena.alloc(Expr::Attribute(AttributeExpr {
            value,
            attr: self.arena.alloc_str(attr),
            span,
        }))
    }

    /// Create a list expression.
    pub fn alloc_list(&self, elts: &'a [&'a Expr<'a>], span: TextRange) -> &'a Expr<'a> {
        self.arena.alloc(Expr::List(ListExpr {
            elts: self
                .arena
                .alloc_slice_iter(elts.iter().map(|e| (*e).clone())),
            span,
        }))
    }

    /// Create a tuple expression.
    pub fn alloc_tuple(&self, elts: &'a [&'a Expr<'a>], span: TextRange) -> &'a Expr<'a> {
        self.arena.alloc(Expr::Tuple(TupleExpr {
            elts: self
                .arena
                .alloc_slice_iter(elts.iter().map(|e| (*e).clone())),
            span,
        }))
    }

    /// Create a dictionary expression.
    pub fn alloc_dict(
        &self,
        keys: &'a [Option<&'a Expr<'a>>],
        values: &'a [&'a Expr<'a>],
        span: TextRange,
    ) -> &'a Expr<'a> {
        let transformed_keys: SmallVec<[Option<Expr<'a>>; 8]> =
            keys.iter().map(|k| k.cloned()).collect();
        self.arena.alloc(Expr::Dict(DictExpr {
            keys: self.arena.alloc_slice_iter(transformed_keys),
            values: self
                .arena
                .alloc_slice_iter(values.iter().map(|v| (*v).clone())),
            span,
        }))
    }

    /// Desugar an augmented assignment into a regular assignment.
    ///
    /// Converts `target += value` to `target = target + value`.
    pub fn desugar_aug_assign(&self, aug_assign: &'a AugAssignStmt<'a>) -> &'a Stmt<'a> {
        let target_clone = self.clone_expr(&aug_assign.target);
        let value_clone = self.clone_expr(&aug_assign.value);
        let binop = self.alloc_binop(target_clone, aug_assign.op, value_clone, aug_assign.span);
        let targets = self.arena.alloc_slice(&[&aug_assign.target]);
        self.alloc_assign(targets, binop, aug_assign.span)
    }

    /// Attempt to fold a constant binary operation.
    ///
    /// Returns `Some(folded_expr)` if both operands are constants and can be folded,
    /// otherwise returns `None`.
    pub fn fold_constant_binop(&self, binop: &BinOpExpr<'a>) -> Option<&'a Expr<'a>> {
        match (binop.left, binop.right) {
            (Expr::Constant(left), Expr::Constant(right)) => {
                if let (Ok(left_num), Ok(right_num)) =
                    (left.value.parse::<i64>(), right.value.parse::<i64>())
                {
                    let result = match binop.op {
                        "+" => left_num + right_num,
                        "-" => left_num - right_num,
                        "*" => left_num * right_num,
                        "/" if right_num != 0 => left_num / right_num,
                        _ => return None,
                    };
                    Some(self.alloc_constant(&result.to_string(), binop.span))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Create a new module with transformed statements.
    pub fn transform_module<F>(&self, module: &Module<'a>, f: F) -> &'a Module<'a>
    where
        F: Fn(&Stmt<'a>) -> Stmt<'a>,
    {
        let transformed_body: SmallVec<[Stmt<'a>; 16]> = module.body.iter().map(f).collect();
        self.arena.alloc(Module {
            body: self.arena.alloc_slice_iter(transformed_body),
            span: module.span,
            docstring: module.docstring,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arena::Arena;

    #[test]
    fn test_transformer_creation() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);
        assert_eq!(transformer.arena().allocated_bytes(), 0);
    }

    #[test]
    fn test_constant_creation() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let constant = transformer.alloc_constant("42", TextRange::default());
        match constant {
            Expr::Constant(c) => assert_eq!(c.value, "42"),
            _ => panic!("Expected constant expression"),
        }
    }

    #[test]
    fn test_name_creation() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let name = transformer.alloc_name("x", TextRange::default());
        match name {
            Expr::Name(n) => assert_eq!(n.id, "x"),
            _ => panic!("Expected name expression"),
        }
    }

    #[test]
    fn test_binop_creation() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let left = transformer.alloc_constant("1", TextRange::default());
        let right = transformer.alloc_constant("2", TextRange::default());
        let binop = transformer.alloc_binop(left, "+", right, TextRange::default());

        match binop {
            Expr::BinOp(b) => {
                assert_eq!(b.op, "+");
                match (b.left, b.right) {
                    (Expr::Constant(l), Expr::Constant(r)) => {
                        assert_eq!(l.value, "1");
                        assert_eq!(r.value, "2");
                    }
                    _ => panic!("Expected constant operands"),
                }
            }
            _ => panic!("Expected binop expression"),
        }
    }

    #[test]
    fn test_assign_creation() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let target = transformer.alloc_name("x", TextRange::default());
        let value = transformer.alloc_constant("42", TextRange::default());
        let targets = arena.alloc_slice(&[target]);
        let assign = transformer.alloc_assign(targets, value, TextRange::default());

        match assign {
            Stmt::Assign(a) => {
                assert_eq!(a.targets.len(), 1);
                match (&a.targets[0], &a.value) {
                    (Expr::Name(n), Expr::Constant(c)) => {
                        assert_eq!(n.id, "x");
                        assert_eq!(c.value, "42");
                    }
                    _ => panic!("Expected name target and constant value"),
                }
            }
            _ => panic!("Expected assign statement"),
        }
    }

    #[test]
    fn test_aug_assign_desugaring() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let target_expr = transformer.alloc_name("x", TextRange::default());
        let value_expr = transformer.alloc_constant("1", TextRange::default());
        let aug_assign = AugAssignStmt {
            target: target_expr.clone(),
            op: "+",
            value: value_expr.clone(),
            span: TextRange::default(),
        };

        let desugared = transformer.desugar_aug_assign(&aug_assign);
        match desugared {
            Stmt::Assign(a) => {
                assert_eq!(a.targets.len(), 1);
                match &a.value {
                    Expr::BinOp(b) => {
                        assert_eq!(b.op, "+");

                        match (b.left, b.right) {
                            (Expr::Name(left_name), Expr::Constant(right_const)) => {
                                assert_eq!(left_name.id, "x");
                                assert_eq!(right_const.value, "1");
                            }
                            _ => panic!("Expected name + constant"),
                        }
                    }
                    _ => panic!("Expected binop in assignment"),
                }
            }
            _ => panic!("Expected assign statement"),
        }
    }

    #[test]
    fn test_constant_folding() {
        let arena = Arena::new();
        let transformer = Transformer::new(&arena);

        let left = transformer.alloc_constant("2", TextRange::default());
        let right = transformer.alloc_constant("3", TextRange::default());

        let binop_expr = BinOpExpr {
            left,
            op: "+",
            right,
            span: TextRange::default(),
        };

        let folded = transformer.fold_constant_binop(&binop_expr);
        assert!(folded.is_some());

        match folded.unwrap() {
            Expr::Constant(c) => assert_eq!(c.value, "5"),
            _ => panic!("Expected constant result"),
        }
    }
}
