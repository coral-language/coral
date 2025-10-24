//! Integration tests for visitor patterns and AST transformations.
//!
//! This test suite validates:
//! - Mutable visitor traversal correctness
//! - Transformation API functionality
//! - Arena allocation safety
//! - Common transformation patterns
//! - Edge cases and error conditions

use coral_parser::{
    Arena, Module,
    ast::*,
    visitor::{mut_visit::MutVisitor, transform::Transformer},
};
use text_size::TextRange;

/// Helper function to create a simple module for testing.
fn create_test_module<'a>(arena: &'a Arena) -> &'a Module<'a> {
    let pass_stmt = Stmt::Pass(TextRange::default());
    let body = arena.alloc_slice_iter([pass_stmt]);
    arena.alloc(Module {
        body,
        span: TextRange::default(),
        docstring: None,
    })
}

/// Helper function to create a constant expression.
fn create_constant<'a>(arena: &'a Arena, value: &str) -> &'a Expr<'a> {
    arena.alloc(Expr::Constant(ConstantExpr {
        value: arena.alloc_str(value),
        span: TextRange::default(),
    }))
}

#[test]
fn test_mut_visitor_basic_traversal() {
    let arena = Arena::new();
    let module = create_test_module(&arena);

    struct TestVisitor {
        stmt_count: usize,
        expr_count: usize,
    }

    impl<'a> MutVisitor<'a> for TestVisitor {
        fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
            self.stmt_count += 1;

            coral_parser::visitor::mut_walk::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr<'a>) {
            self.expr_count += 1;
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut visitor = TestVisitor {
        stmt_count: 0,
        expr_count: 0,
    };

    visitor.visit_module(module);

    assert_eq!(visitor.stmt_count, 1);
    assert_eq!(visitor.expr_count, 0);
}

#[test]
fn test_mut_visitor_expression_traversal() {
    let arena = Arena::new();

    let one = create_constant(&arena, "1");
    let two = create_constant(&arena, "2");
    let three = create_constant(&arena, "3");

    let mult = arena.alloc(Expr::BinOp(BinOpExpr {
        left: two,
        op: arena.alloc_str("*"),
        right: three,
        span: TextRange::default(),
    }));

    let add = arena.alloc(Expr::BinOp(BinOpExpr {
        left: one,
        op: arena.alloc_str("+"),
        right: mult,
        span: TextRange::default(),
    }));

    let expr_stmt = Stmt::Expr(ExprStmt {
        value: add.clone(),
        span: TextRange::default(),
    });

    let module = arena.alloc(Module {
        body: arena.alloc_slice_iter([expr_stmt]),
        span: TextRange::default(),
        docstring: None,
    });

    struct ExprCounter {
        constant_count: usize,
        binop_count: usize,
    }

    impl<'a> MutVisitor<'a> for ExprCounter {
        fn visit_expr(&mut self, expr: &Expr<'a>) {
            match expr {
                Expr::Constant(_) => self.constant_count += 1,
                Expr::BinOp(_) => self.binop_count += 1,
                _ => {}
            }
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut visitor = ExprCounter {
        constant_count: 0,
        binop_count: 0,
    };

    visitor.visit_module(module);

    assert_eq!(visitor.constant_count, 3); // 1, 2, 3
    assert_eq!(visitor.binop_count, 2); // +, *
}

#[test]
fn test_transformer_basic_functionality() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let constant = transformer.alloc_constant("42", TextRange::default());
    match constant {
        Expr::Constant(c) => assert_eq!(c.value, "42"),
        _ => panic!("Expected constant"),
    }

    let name = transformer.alloc_name("x", TextRange::default());
    match name {
        Expr::Name(n) => assert_eq!(n.id, "x"),
        _ => panic!("Expected name"),
    }

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
                _ => panic!("Expected constants"),
            }
        }
        _ => panic!("Expected binop"),
    }
}

#[test]
fn test_transformer_expression_transformation() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let orig_left = transformer.alloc_constant("2", TextRange::default());
    let orig_right = transformer.alloc_constant("3", TextRange::default());
    let orig_expr = transformer.alloc_binop(orig_left, "+", orig_right, TextRange::default());

    let transformed = transformer.transform_expr(orig_expr, |expr| match expr {
        Expr::BinOp(b) => {
            let new_left = transformer.alloc_constant("10", TextRange::default());
            Expr::BinOp(BinOpExpr {
                left: new_left,
                op: b.op,
                right: b.right,
                span: b.span,
            })
        }
        other => other.clone(),
    });

    match transformed {
        Expr::BinOp(b) => {
            match b.left {
                Expr::Constant(c) => assert_eq!(c.value, "10"),
                _ => panic!("Expected constant 10"),
            }
            match b.right {
                Expr::Constant(c) => assert_eq!(c.value, "3"),
                _ => panic!("Expected constant 3"),
            }
        }
        _ => panic!("Expected binop"),
    }
}

#[test]
fn test_constant_folding_transformation() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let left = transformer.alloc_constant("2", TextRange::default());
    let right = transformer.alloc_constant("3", TextRange::default());
    let expr = transformer.alloc_binop(left, "+", right, TextRange::default());

    let binop_expr = match expr {
        Expr::BinOp(b) => b,
        _ => panic!("Expected binop"),
    };

    let folded = transformer.fold_constant_binop(binop_expr);
    assert!(folded.is_some());

    match folded.unwrap() {
        Expr::Constant(c) => assert_eq!(c.value, "5"),
        _ => panic!("Expected constant 5"),
    }

    let x_name = transformer.alloc_name("x", TextRange::default());
    let non_foldable = transformer.alloc_binop(left, "+", x_name, TextRange::default());

    let binop_expr2 = match non_foldable {
        Expr::BinOp(b) => b,
        _ => panic!("Expected binop"),
    };

    let not_folded = transformer.fold_constant_binop(binop_expr2);
    assert!(not_folded.is_none());
}

#[test]
fn test_augmented_assignment_desugaring() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let target = transformer.alloc_name("x", TextRange::default());
    let value = transformer.alloc_constant("1", TextRange::default());

    let aug_assign = AugAssignStmt {
        target: target.clone(),
        op: "+",
        value: value.clone(),
        span: TextRange::default(),
    };

    let desugared = transformer.desugar_aug_assign(&aug_assign);

    match desugared {
        Stmt::Assign(a) => {
            assert_eq!(a.targets.len(), 1);
            match a.targets[0] {
                Expr::Name(ref n) => assert_eq!(n.id, "x"),
                _ => panic!("Expected x as target"),
            }
            match a.value {
                Expr::BinOp(ref b) => {
                    assert_eq!(b.op, "+");
                    match (b.left, b.right) {
                        (Expr::Name(left_name), Expr::Constant(right_const)) => {
                            assert_eq!(left_name.id, "x");
                            assert_eq!(right_const.value, "1");
                        }
                        _ => panic!("Expected x + 1"),
                    }
                }
                _ => panic!("Expected binop"),
            }
        }
        _ => panic!("Expected assign statement"),
    }
}

#[test]
fn test_statement_transformation() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let target = transformer.alloc_name("x", TextRange::default());
    let value = transformer.alloc_constant("42", TextRange::default());
    let targets = arena.alloc_slice(&[target]);
    let assign_stmt = transformer.alloc_assign(targets, value, TextRange::default());

    let transformed_stmt = transformer.transform_stmt(assign_stmt, |stmt| match stmt {
        Stmt::Assign(a) => {
            let new_value = transformer.alloc_constant("100", TextRange::default());
            Stmt::Assign(AssignStmt {
                targets: a.targets,
                value: new_value.clone(),
                span: a.span,
            })
        }
        other => other.clone(),
    });

    match transformed_stmt {
        Stmt::Assign(a) => match &a.value {
            Expr::Constant(c) => assert_eq!(c.value, "100"),
            _ => panic!("Expected constant 100"),
        },
        _ => panic!("Expected assign statement"),
    }
}

#[test]
fn test_module_transformation() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let x_target = transformer.alloc_name("x", TextRange::default());
    let x_value = transformer.alloc_constant("1", TextRange::default());
    let x_targets = arena.alloc_slice(&[x_target]);
    let x_assign = transformer.alloc_assign(x_targets, x_value, TextRange::default());

    let y_target = transformer.alloc_name("y", TextRange::default());
    let y_value = transformer.alloc_constant("2", TextRange::default());
    let y_targets = arena.alloc_slice(&[y_target]);
    let y_assign = transformer.alloc_assign(y_targets, y_value, TextRange::default());

    let original_module = arena.alloc(Module {
        body: arena.alloc_slice_iter([x_assign.clone(), y_assign.clone()]),
        span: TextRange::default(),
        docstring: None,
    });

    let transformed_module = transformer.transform_module(original_module, |stmt| match stmt {
        Stmt::Assign(a) => {
            let new_value = transformer.alloc_constant("99", TextRange::default());
            Stmt::Assign(AssignStmt {
                targets: a.targets,
                value: new_value.clone(),
                span: a.span,
            })
        }
        other => other.clone(),
    });

    assert_eq!(transformed_module.body.len(), 2);
    for stmt in transformed_module.body {
        match stmt {
            Stmt::Assign(a) => match &a.value {
                Expr::Constant(c) => assert_eq!(c.value, "99"),
                _ => panic!("Expected constant 99"),
            },
            _ => panic!("Expected assign statement"),
        }
    }
}

#[test]
fn test_expression_list_transformation() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let expr1 = transformer.alloc_constant("1", TextRange::default());
    let expr2 = transformer.alloc_constant("2", TextRange::default());
    let expr3 = transformer.alloc_constant("3", TextRange::default());

    let expr_array = [expr1, expr2, expr3];
    let original_list = transformer.alloc_list(&expr_array, TextRange::default());

    let transformed_list = transformer.transform_expr(original_list, |expr| match expr {
        Expr::List(l) => {
            let expr_refs: Vec<&Expr> = l.elts.iter().collect();
            let new_elts = transformer.map_exprs(&expr_refs, |elt| match elt {
                Expr::Constant(c) => {
                    if let Ok(num) = c.value.parse::<i32>() {
                        Expr::Constant(ConstantExpr {
                            value: transformer.arena().alloc_str(&(num * 2).to_string()),
                            span: c.span,
                        })
                    } else {
                        elt.clone()
                    }
                }
                other => other.clone(),
            });
            Expr::List(ListExpr {
                elts: new_elts,
                span: l.span,
            })
        }
        other => other.clone(),
    });

    match transformed_list {
        Expr::List(l) => {
            assert_eq!(l.elts.len(), 3);
            for (i, elt) in l.elts.iter().enumerate() {
                match elt {
                    Expr::Constant(c) => {
                        let expected = ((i + 1) * 2).to_string();
                        assert_eq!(c.value, expected);
                    }
                    _ => panic!("Expected constant"),
                }
            }
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_complex_ast_traversal() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let func_def = {
        let _x_param = transformer.alloc_name("x", TextRange::default());
        let _y_param = transformer.alloc_name("y", TextRange::default());
        let default_val = transformer.alloc_constant("1", TextRange::default());

        let args = Arguments {
            posonlyargs: arena.alloc_slice_iter([]),
            args: arena.alloc_slice_iter([
                Arg {
                    arg: arena.alloc_str("x"),
                    annotation: None,
                },
                Arg {
                    arg: arena.alloc_str("y"),
                    annotation: None,
                },
            ]),
            vararg: None,
            kwonlyargs: arena.alloc_slice_iter([]),
            kwarg: None,
            defaults: arena.alloc_slice_iter([]),
            kw_defaults: arena.alloc_slice_iter([Some(default_val.clone())]),
        };

        let x_ref = transformer.alloc_name("x", TextRange::default());
        let y_ref = transformer.alloc_name("y", TextRange::default());
        let two = transformer.alloc_constant("2", TextRange::default());
        let mult = transformer.alloc_binop(y_ref, "*", two, TextRange::default());
        let add = transformer.alloc_binop(x_ref, "+", mult, TextRange::default());

        let return_stmt = Stmt::Return(ReturnStmt {
            value: Some(add.clone()),
            span: TextRange::default(),
        });

        Stmt::FuncDef(FuncDefStmt {
            name: arena.alloc_str("foo"),
            type_params: arena.alloc_slice_iter([]),
            args,
            body: arena.alloc_slice_iter([return_stmt]),
            decorators: arena.alloc_slice_iter([]),
            returns: None,
            is_async: false,
            span: TextRange::default(),
            docstring: None,
        })
    };

    let func_def_ref = arena.alloc(func_def);
    let module = arena.alloc(Module {
        body: arena.alloc_slice_iter([(*func_def_ref).clone()]),
        span: TextRange::default(),
        docstring: None,
    });

    struct NodeTypeCounter {
        func_defs: usize,
        returns: usize,
        names: usize,
        constants: usize,
        binops: usize,
    }

    impl<'a> MutVisitor<'a> for NodeTypeCounter {
        fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
            match stmt {
                Stmt::FuncDef(_) => self.func_defs += 1,
                Stmt::Return(_) => self.returns += 1,
                _ => {}
            }
            coral_parser::visitor::mut_walk::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr<'a>) {
            match expr {
                Expr::Name(_) => self.names += 1,
                Expr::Constant(_) => self.constants += 1,
                Expr::BinOp(_) => self.binops += 1,
                _ => {}
            }
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut counter = NodeTypeCounter {
        func_defs: 0,
        returns: 0,
        names: 0,
        constants: 0,
        binops: 0,
    };

    counter.visit_module(module);

    assert_eq!(counter.func_defs, 1);
    assert_eq!(counter.returns, 1);
    assert_eq!(counter.names, 2); // x, y references in expression
    assert_eq!(counter.constants, 2); // 1 (default) + 2 (in expression)
    assert_eq!(counter.binops, 2); // +, *
}

#[test]
fn test_visitor_edge_cases() {
    let arena = Arena::new();

    let empty_module = arena.alloc(Module {
        body: arena.alloc_slice_iter([]),
        span: TextRange::default(),
        docstring: None,
    });

    struct EmptyModuleVisitor(usize);
    impl<'a> MutVisitor<'a> for EmptyModuleVisitor {
        fn visit_stmt(&mut self, _stmt: &Stmt<'a>) {
            self.0 += 1;
        }
    }

    let mut visitor = EmptyModuleVisitor(0);
    visitor.visit_module(empty_module);
    assert_eq!(visitor.0, 0); // No statements visited

    let transformer = Transformer::new(&arena);

    let innermost = transformer.alloc_constant("1", TextRange::default());
    let inner_elements = [innermost];
    let inner_tuple = transformer.alloc_tuple(&inner_elements, TextRange::default());
    let outer_elements = [inner_tuple];
    let current = transformer.alloc_tuple(&outer_elements, TextRange::default());

    struct DepthCounter(usize);
    impl<'a> MutVisitor<'a> for DepthCounter {
        fn visit_expr(&mut self, expr: &Expr<'a>) {
            self.0 += 1;
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut depth_visitor = DepthCounter(0);
    depth_visitor.visit_expr(current);
    assert_eq!(depth_visitor.0, 3); // 1 constant + 2 tuples
}

#[test]
fn test_transformation_memory_safety() {
    let arena = Arena::new();
    let transformer = Transformer::new(&arena);

    let mut exprs = Vec::new();
    for i in 0..1000 {
        exprs.push(transformer.alloc_constant(&i.to_string(), TextRange::default()));
    }

    let list = transformer.alloc_list(&exprs, TextRange::default());

    let transformed = transformer.transform_expr(list, |expr| match expr {
        Expr::List(l) => {
            let expr_refs: Vec<&Expr> = l.elts.iter().collect();
            let new_elts = transformer.map_exprs(&expr_refs, |elt| match elt {
                Expr::Constant(c) => {
                    if let Ok(num) = c.value.parse::<i32>() {
                        Expr::Constant(ConstantExpr {
                            value: transformer.arena().alloc_str(&(num * 2).to_string()),
                            span: c.span,
                        })
                    } else {
                        elt.clone()
                    }
                }
                other => other.clone(),
            });
            Expr::List(ListExpr {
                elts: new_elts,
                span: l.span,
            })
        }
        other => other.clone(),
    });

    match transformed {
        Expr::List(l) => {
            assert_eq!(l.elts.len(), 1000);
            for (i, elt) in l.elts.iter().enumerate() {
                match elt {
                    Expr::Constant(c) => {
                        let expected = (i * 2).to_string();
                        assert_eq!(c.value, expected);
                    }
                    _ => panic!("Expected constant"),
                }
            }
        }
        _ => panic!("Expected list"),
    }

    let stats = arena.memory_stats();
    assert!(stats.allocated_bytes > 0);
    assert!(stats.chunk_count >= 1);
}

#[test]
fn test_visitor_funcdef_returns_traversal() {
    let arena = Arena::new();

    let return_type_expr = arena.alloc(Expr::Name(NameExpr {
        id: arena.alloc_str("int"),
        span: TextRange::default(),
    }));

    let func_def = Stmt::FuncDef(FuncDefStmt {
        name: arena.alloc_str("test_func"),
        type_params: &[],
        args: Arguments {
            posonlyargs: &[],
            args: &[],
            vararg: None,
            kwonlyargs: &[],
            kw_defaults: &[],
            kwarg: None,
            defaults: &[],
        },
        body: &[],
        decorators: &[],
        returns: Some(Box::new(return_type_expr.clone())),
        is_async: false,
        span: TextRange::default(),
        docstring: None,
    });

    let module = arena.alloc(Module {
        body: arena.alloc_slice_iter([func_def]),
        span: TextRange::default(),
        docstring: None,
    });

    struct TestVisitor {
        stmt_count: usize,
        expr_count: usize,
        name_count: usize,
    }

    impl<'a> MutVisitor<'a> for TestVisitor {
        fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
            self.stmt_count += 1;
            coral_parser::visitor::mut_walk::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr<'a>) {
            self.expr_count += 1;
            if let Expr::Name(_) = expr {
                self.name_count += 1;
            }
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut visitor = TestVisitor {
        stmt_count: 0,
        expr_count: 0,
        name_count: 0,
    };

    visitor.visit_module(module);

    assert_eq!(visitor.stmt_count, 1);
    assert_eq!(visitor.expr_count, 1);
    assert_eq!(visitor.name_count, 1);
}

#[test]
fn test_visitor_formatted_value_format_spec_traversal() {
    let arena = Arena::new();

    let value_expr = create_constant(&arena, "3.14159");
    let format_spec_literal = arena.alloc(Expr::Constant(ConstantExpr {
        value: arena.alloc_str(".2f"),
        span: TextRange::default(),
    }));

    let format_spec = arena.alloc(JoinedStrExpr {
        values: arena.alloc_slice_iter([format_spec_literal.clone()]),
        span: TextRange::default(),
    });

    let formatted_value = arena.alloc(Expr::FormattedValue(FormattedValueExpr {
        value: value_expr,
        conversion: None,
        format_spec: Some(format_spec),
        span: TextRange::default(),
    }));

    let expr_stmt = Stmt::Expr(ExprStmt {
        value: formatted_value.clone(),
        span: TextRange::default(),
    });

    let module = arena.alloc(Module {
        body: arena.alloc_slice_iter([expr_stmt]),
        span: TextRange::default(),
        docstring: None,
    });

    struct TestVisitor {
        stmt_count: usize,
        expr_count: usize,
        constant_count: usize,
    }

    impl<'a> MutVisitor<'a> for TestVisitor {
        fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
            self.stmt_count += 1;
            coral_parser::visitor::mut_walk::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr<'a>) {
            self.expr_count += 1;
            if let Expr::Constant(_) = expr {
                self.constant_count += 1;
            }
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut visitor = TestVisitor {
        stmt_count: 0,
        expr_count: 0,
        constant_count: 0,
    };

    visitor.visit_module(module);

    assert_eq!(visitor.stmt_count, 1);
    assert_eq!(visitor.expr_count, 3);
    assert_eq!(visitor.constant_count, 2);
}

#[test]
fn test_visitor_formatted_value_no_format_spec() {
    let arena = Arena::new();

    let value_expr = create_constant(&arena, "hello");

    let formatted_value = arena.alloc(Expr::FormattedValue(FormattedValueExpr {
        value: value_expr,
        conversion: None,
        format_spec: None,
        span: TextRange::default(),
    }));

    let expr_stmt = Stmt::Expr(ExprStmt {
        value: formatted_value.clone(),
        span: TextRange::default(),
    });

    let module = arena.alloc(Module {
        body: arena.alloc_slice_iter([expr_stmt]),
        span: TextRange::default(),
        docstring: None,
    });

    struct TestVisitor {
        stmt_count: usize,
        expr_count: usize,
        constant_count: usize,
    }

    impl<'a> MutVisitor<'a> for TestVisitor {
        fn visit_stmt(&mut self, stmt: &Stmt<'a>) {
            self.stmt_count += 1;
            coral_parser::visitor::mut_walk::walk_stmt(self, stmt);
        }

        fn visit_expr(&mut self, expr: &Expr<'a>) {
            self.expr_count += 1;
            if let Expr::Constant(_) = expr {
                self.constant_count += 1;
            }
            coral_parser::visitor::mut_walk::walk_expr(self, expr);
        }
    }

    let mut visitor = TestVisitor {
        stmt_count: 0,
        expr_count: 0,
        constant_count: 0,
    };

    visitor.visit_module(module);

    assert_eq!(visitor.stmt_count, 1);
    assert_eq!(visitor.expr_count, 2);
    assert_eq!(visitor.constant_count, 1);
}
