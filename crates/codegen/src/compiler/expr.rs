//! Expression compilation to bytecode

use crate::bytecode::Instruction;
use crate::compiler::context::CompilationContext;
use crate::error::{CodegenError, CodegenResult};
use coral_parser::ast::Expr;

pub struct ExprCompiler;

impl ExprCompiler {
    pub fn compile(ctx: &mut CompilationContext, expr: &Expr) -> CodegenResult<u16> {
        match expr {
            Expr::Constant(e) => Self::compile_constant(ctx, e.value),
            Expr::Name(e) => Self::compile_name(ctx, e.id),
            Expr::BinOp(e) => Self::compile_binop(ctx, e.left, e.op, e.right),
            Expr::UnaryOp(e) => Self::compile_unaryop(ctx, e.op, e.operand),
            Expr::Compare(e) => Self::compile_compare(ctx, e),
            Expr::Call(e) => Self::compile_call(ctx, e),
            Expr::Attribute(e) => Self::compile_attribute(ctx, e.value, e.attr),
            Expr::Subscript(e) => Self::compile_subscript(ctx, e.value, e.slice),
            Expr::List(e) => Self::compile_list(ctx, e.elts),
            Expr::Tuple(e) => Self::compile_tuple(ctx, e.elts),
            Expr::Dict(e) => Self::compile_dict(ctx, e.keys, e.values),
            Expr::Set(e) => Self::compile_set(ctx, e.elts),
            Expr::BoolOp(e) => Self::compile_boolop(ctx, e.op, e.values),
            Expr::IfExp(e) => Self::compile_ifexp(ctx, e.test, e.body, e.orelse),
            Expr::Lambda(e) => Self::compile_lambda(ctx, e),
            Expr::ListComp(e) => Self::compile_listcomp(ctx, e),
            Expr::DictComp(e) => Self::compile_dictcomp(ctx, e),
            Expr::SetComp(e) => Self::compile_setcomp(ctx, e),
            Expr::GeneratorExp(e) => Self::compile_generatorexp(ctx, e),
            Expr::Await(e) => Self::compile_await(ctx, e.value),
            Expr::Yield(e) => Self::compile_yield(ctx, e.value),
            Expr::YieldFrom(e) => Self::compile_yieldfrom(ctx, e.value),
            Expr::JoinedStr(e) => Self::compile_joinedstr(ctx, e.values),
            Expr::FormattedValue(e) => Self::compile_formattedvalue(ctx, e),
            Expr::TString(e) => Self::compile_tstring(ctx, e.values),
            Expr::Starred(e) => Self::compile_starred(ctx, e.value),
            Expr::NamedExpr(e) => Self::compile_namedexpr(ctx, e.target, e.value),
            Expr::Slice(e) => Self::compile_slice(ctx, e),
            Expr::Complex(_) => Err(CodegenError::CompilationError(
                "Complex number support not yet implemented".to_string(),
            )),
            Expr::Bytes(_) => Err(CodegenError::CompilationError(
                "Bytes literal support not yet implemented".to_string(),
            )),
            Expr::ModuleIntrospection(_) => Err(CodegenError::CompilationError(
                "Module introspection support not yet implemented".to_string(),
            )),
            Expr::Error(_) => Err(CodegenError::CompilationError(
                "Error expression in AST".to_string(),
            )),
        }
    }

    fn compile_constant(ctx: &mut CompilationContext, value: &str) -> CodegenResult<u16> {
        let dst = ctx.allocate_register(0)?;

        let const_id = if let Ok(i) = value.parse::<i64>() {
            ctx.add_constant_integer(i)
        } else if let Ok(f) = value.parse::<f64>() {
            ctx.add_constant_float(f)
        } else if value == "True" {
            ctx.add_constant_bool(true)
        } else if value == "False" {
            ctx.add_constant_bool(false)
        } else if value == "None" {
            ctx.add_constant_none()
        } else {
            ctx.add_constant_string(value.to_string())
        };

        ctx.emit(Instruction::LoadConst { dst, const_id });

        Ok(dst)
    }

    fn compile_name(ctx: &mut CompilationContext, name: &str) -> CodegenResult<u16> {
        if let Some(reg_id) = ctx.resolve_local(name) {
            return Ok(reg_id);
        }

        if ctx.resolve_global(name).is_some() {
            let dst = ctx.allocate_register(0)?;
            let name_id = ctx.add_constant_string(name.to_string());
            ctx.emit(Instruction::LoadGlobal { dst, name_id });
            return Ok(dst);
        }

        Err(CodegenError::CompilationError(format!(
            "Undefined variable: {}",
            name
        )))
    }

    fn compile_binop(
        ctx: &mut CompilationContext,
        left: &Expr,
        op: &str,
        right: &Expr,
    ) -> CodegenResult<u16> {
        let lhs = Self::compile(ctx, left)?;
        let rhs = Self::compile(ctx, right)?;
        let dst = ctx.allocate_register(0)?;

        let lhs_type = ctx.register_allocator.get_type(lhs).unwrap_or(0);
        let rhs_type = ctx.register_allocator.get_type(rhs).unwrap_or(0);

        let instr = match (
            Self::classify_type(lhs_type),
            Self::classify_type(rhs_type),
            op,
        ) {
            (TypeClass::Int, TypeClass::Int, "+") => Instruction::AddInt { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Int, "-") => Instruction::SubInt { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Int, "*") => Instruction::MulInt { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Int, "/") => Instruction::DivInt { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Int, "%") => Instruction::ModInt { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Int, "**") => Instruction::PowInt {
                dst,
                base: lhs,
                exp: rhs,
            },
            (TypeClass::Float, TypeClass::Float, "+") => Instruction::AddFloat { dst, lhs, rhs },
            (TypeClass::Float, TypeClass::Float, "-") => Instruction::SubFloat { dst, lhs, rhs },
            (TypeClass::Float, TypeClass::Float, "*") => Instruction::MulFloat { dst, lhs, rhs },
            (TypeClass::Float, TypeClass::Float, "/") => Instruction::DivFloat { dst, lhs, rhs },
            (TypeClass::Float, TypeClass::Float, "%") => Instruction::ModFloat { dst, lhs, rhs },
            (TypeClass::Float, TypeClass::Float, "**") => Instruction::PowFloat {
                dst,
                base: lhs,
                exp: rhs,
            },
            (TypeClass::String, TypeClass::String, "+") => Instruction::ConcatStr { dst, lhs, rhs },
            (TypeClass::Int, TypeClass::Float, op) | (TypeClass::Float, TypeClass::Int, op) => {
                return Err(CodegenError::CompilationError(format!(
                    "Mixed numeric types in binary operation '{}': cannot mix int and float without explicit conversion",
                    op
                )));
            }
            _ => {
                return Err(CodegenError::CompilationError(format!(
                    "Unsupported operand types for binary operator '{}': {} and {}",
                    op,
                    Self::type_to_string(lhs_type),
                    Self::type_to_string(rhs_type)
                )));
            }
        };

        ctx.emit(instr);
        Ok(dst)
    }

    fn compile_unaryop(
        ctx: &mut CompilationContext,
        op: &str,
        operand: &Expr,
    ) -> CodegenResult<u16> {
        let operand_reg = Self::compile(ctx, operand)?;
        let dst = ctx.allocate_register(0)?;

        let instr = match op {
            "-" => Instruction::NegInt {
                dst,
                operand: operand_reg,
            },
            "+" => Instruction::Move {
                dst,
                src: operand_reg,
            },
            "~" => Instruction::Not {
                dst,
                operand: operand_reg,
            },
            "not" => Instruction::Not {
                dst,
                operand: operand_reg,
            },
            _ => {
                return Err(CodegenError::CompilationError(format!(
                    "Unknown unary operator: {}",
                    op
                )));
            }
        };

        ctx.emit(instr);
        Ok(dst)
    }

    fn compile_compare(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::CompareExpr,
    ) -> CodegenResult<u16> {
        let left = Self::compile(ctx, expr.left)?;
        let mut current_result = left;

        for (i, (op, right_expr)) in expr.ops.iter().zip(expr.comparators.iter()).enumerate() {
            let right = Self::compile(ctx, right_expr)?;
            let result = ctx.allocate_register(0)?;

            let instr = match *op {
                "==" => Instruction::EqInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                "!=" => Instruction::NotEqInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                "<" => Instruction::LtInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                "<=" => Instruction::LtEqInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                ">" => Instruction::GtInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                ">=" => Instruction::GtEqInt {
                    dst: result,
                    lhs: current_result,
                    rhs: right,
                },
                _ => {
                    return Err(CodegenError::CompilationError(format!(
                        "Unknown comparison operator: {}",
                        op
                    )));
                }
            };

            ctx.emit(instr);

            if i < expr.ops.len() - 1 {
                let and_result = ctx.allocate_register(0)?;
                ctx.emit(Instruction::And {
                    dst: and_result,
                    lhs: result,
                    rhs: right,
                });
                current_result = and_result;
            } else {
                current_result = result;
            }
        }

        Ok(current_result)
    }

    fn compile_call(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::CallExpr,
    ) -> CodegenResult<u16> {
        let func_reg = Self::compile(ctx, expr.func)?;
        let mut arg_regs = Vec::new();

        for arg in expr.args {
            let arg_reg = Self::compile(ctx, arg)?;
            arg_regs.push(arg_reg);
        }

        let result = ctx.allocate_register(0)?;
        let arg_base = if !arg_regs.is_empty() { arg_regs[0] } else { 0 };

        ctx.emit(Instruction::Call {
            result,
            func_reg,
            arg_base,
            arg_count: arg_regs.len() as u16,
        });

        Ok(result)
    }

    fn compile_attribute(
        ctx: &mut CompilationContext,
        obj_expr: &Expr,
        attr: &str,
    ) -> CodegenResult<u16> {
        let obj = Self::compile(ctx, obj_expr)?;
        let dst = ctx.allocate_register(0)?;
        let attr_id = ctx.add_constant_string(attr.to_string());

        ctx.emit(Instruction::GetAttr { dst, obj, attr_id });

        Ok(dst)
    }

    fn compile_subscript(
        ctx: &mut CompilationContext,
        obj_expr: &Expr,
        slice_expr: &Expr,
    ) -> CodegenResult<u16> {
        let obj = Self::compile(ctx, obj_expr)?;
        let key = Self::compile(ctx, slice_expr)?;
        let dst = ctx.allocate_register(0)?;

        ctx.emit(Instruction::GetItem { dst, obj, key });

        Ok(dst)
    }

    fn compile_list(ctx: &mut CompilationContext, elts: &[Expr]) -> CodegenResult<u16> {
        let dst = ctx.allocate_register(0)?;

        ctx.emit(Instruction::NewList {
            dst,
            size_hint: elts.len() as u32,
        });

        Ok(dst)
    }

    fn compile_tuple(ctx: &mut CompilationContext, elts: &[Expr]) -> CodegenResult<u16> {
        let dst = ctx.allocate_register(0)?;

        ctx.emit(Instruction::NewTuple {
            dst,
            elem_count: elts.len() as u16,
        });

        Ok(dst)
    }

    fn compile_dict(
        ctx: &mut CompilationContext,
        keys: &[Option<Expr>],
        _values: &[Expr],
    ) -> CodegenResult<u16> {
        let dst = ctx.allocate_register(0)?;

        ctx.emit(Instruction::NewDict {
            dst,
            size_hint: keys.len() as u32,
        });

        Ok(dst)
    }

    fn compile_set(ctx: &mut CompilationContext, elts: &[Expr]) -> CodegenResult<u16> {
        let dst = ctx.allocate_register(0)?;

        ctx.emit(Instruction::NewSet {
            dst,
            size_hint: elts.len() as u32,
        });

        Ok(dst)
    }

    fn compile_boolop(
        ctx: &mut CompilationContext,
        op: &str,
        values: &[Expr],
    ) -> CodegenResult<u16> {
        if values.is_empty() {
            return Err(CodegenError::CompilationError(
                "BoolOp with no values".to_string(),
            ));
        }

        let mut result = Self::compile(ctx, &values[0])?;

        for val_expr in &values[1..] {
            let val = Self::compile(ctx, val_expr)?;
            let new_result = ctx.allocate_register(0)?;

            let instr = match op {
                "and" => Instruction::And {
                    dst: new_result,
                    lhs: result,
                    rhs: val,
                },
                "or" => Instruction::Or {
                    dst: new_result,
                    lhs: result,
                    rhs: val,
                },
                _ => {
                    return Err(CodegenError::CompilationError(format!(
                        "Unknown boolean operator: {}",
                        op
                    )));
                }
            };

            ctx.emit(instr);
            result = new_result;
        }

        Ok(result)
    }

    fn compile_ifexp(
        ctx: &mut CompilationContext,
        test: &Expr,
        body: &Expr,
        orelse: &Expr,
    ) -> CodegenResult<u16> {
        let test_reg = Self::compile(ctx, test)?;
        let result = ctx.allocate_register(0)?;

        let else_offset = ctx.current_instruction_offset();
        ctx.emit(Instruction::JumpIfFalse {
            condition: test_reg,
            offset: 0,
        });

        let body_reg = Self::compile(ctx, body)?;
        ctx.emit(Instruction::Move {
            dst: result,
            src: body_reg,
        });

        let exit_offset = ctx.current_instruction_offset();
        ctx.emit(Instruction::Jump { offset: 0 });

        let orelse_start = ctx.current_instruction_offset();
        if let Instruction::JumpIfFalse { offset, .. } = &mut ctx.instructions[else_offset as usize]
        {
            *offset = orelse_start;
        }

        let orelse_reg = Self::compile(ctx, orelse)?;
        ctx.emit(Instruction::Move {
            dst: result,
            src: orelse_reg,
        });

        let end = ctx.current_instruction_offset();
        if let Instruction::Jump { offset } = &mut ctx.instructions[exit_offset as usize] {
            *offset = end;
        }

        Ok(result)
    }

    fn compile_lambda(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::LambdaExpr,
    ) -> CodegenResult<u16> {
        crate::compiler::function::FunctionCompiler::compile_lambda(ctx, expr)
    }

    fn compile_listcomp(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::ListCompExpr,
    ) -> CodegenResult<u16> {
        let list_reg = ctx.allocate_register(0)?;
        ctx.emit(Instruction::NewList {
            dst: list_reg,
            size_hint: 0,
        });

        for generator in expr.generators {
            let _iter_reg = ExprCompiler::compile(ctx, &generator.iter)?;

            for if_cond in generator.ifs {
                let cond_reg = ExprCompiler::compile(ctx, if_cond)?;
                let loop_exit = ctx.current_instruction_offset();
                ctx.emit(Instruction::JumpIfFalse {
                    condition: cond_reg,
                    offset: 0,
                });

                let _elt_reg = ExprCompiler::compile(ctx, expr.elt)?;

                let new_offset = ctx.current_instruction_offset();
                if let Instruction::JumpIfFalse { offset, .. } =
                    &mut ctx.instructions[loop_exit as usize]
                {
                    *offset = new_offset;
                }
            }
        }

        Ok(list_reg)
    }

    fn compile_dictcomp(
        _ctx: &mut CompilationContext,
        _expr: &coral_parser::ast::DictCompExpr,
    ) -> CodegenResult<u16> {
        let dict_reg = _ctx.allocate_register(0)?;
        _ctx.emit(Instruction::NewDict {
            dst: dict_reg,
            size_hint: 0,
        });

        Ok(dict_reg)
    }

    fn compile_setcomp(
        _ctx: &mut CompilationContext,
        _expr: &coral_parser::ast::SetCompExpr,
    ) -> CodegenResult<u16> {
        let set_reg = _ctx.allocate_register(0)?;
        _ctx.emit(Instruction::NewSet {
            dst: set_reg,
            size_hint: 0,
        });

        Ok(set_reg)
    }

    fn compile_generatorexp(
        ctx: &mut CompilationContext,
        _expr: &coral_parser::ast::GeneratorExpExpr,
    ) -> CodegenResult<u16> {
        let gen_reg = ctx.allocate_register(0)?;
        let function_id = ctx.allocate_generator_id();
        ctx.emit(Instruction::NewGenerator {
            dst: gen_reg,
            function_id,
        });

        Ok(gen_reg)
    }

    fn compile_await(ctx: &mut CompilationContext, value: &Expr) -> CodegenResult<u16> {
        let future_reg = ExprCompiler::compile(ctx, value)?;
        let result_reg = ctx.allocate_register(0)?;

        ctx.emit(Instruction::Await {
            result: result_reg,
            future: future_reg,
            state_id: 0,
        });

        Ok(result_reg)
    }

    fn compile_yield(ctx: &mut CompilationContext, value: Option<&Expr>) -> CodegenResult<u16> {
        let value_reg = if let Some(expr) = value {
            ExprCompiler::compile(ctx, expr)?
        } else {
            let none_id = ctx.add_constant_none();
            let reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: reg,
                const_id: none_id,
            });
            reg
        };

        ctx.emit(Instruction::Yield { value: value_reg });

        Ok(value_reg)
    }

    fn compile_yieldfrom(ctx: &mut CompilationContext, value: &Expr) -> CodegenResult<u16> {
        let iter_reg = ExprCompiler::compile(ctx, value)?;
        let result_reg = ctx.allocate_register(0)?;

        ctx.emit(Instruction::YieldFrom { value: iter_reg });

        Ok(result_reg)
    }

    fn compile_joinedstr(_ctx: &mut CompilationContext, _values: &[Expr]) -> CodegenResult<u16> {
        Err(CodegenError::CompilationError(
            "F-string expressions are not yet supported in this implementation".to_string(),
        ))
    }

    fn compile_formattedvalue(
        _ctx: &mut CompilationContext,
        _expr: &coral_parser::ast::FormattedValueExpr,
    ) -> CodegenResult<u16> {
        Err(CodegenError::CompilationError(
            "Formatted value expressions in f-strings are not yet supported".to_string(),
        ))
    }

    fn compile_tstring(_ctx: &mut CompilationContext, _values: &[Expr]) -> CodegenResult<u16> {
        Err(CodegenError::CompilationError(
            "Template string expressions are not yet supported in this implementation".to_string(),
        ))
    }

    fn compile_starred(_ctx: &mut CompilationContext, _value: &Expr) -> CodegenResult<u16> {
        Err(CodegenError::CompilationError(
            "Starred expressions (*args) are not yet supported in this context".to_string(),
        ))
    }

    fn compile_namedexpr(
        _ctx: &mut CompilationContext,
        _target: &Expr,
        _value: &Expr,
    ) -> CodegenResult<u16> {
        Err(CodegenError::CompilationError(
            "Named expressions (walrus operator :=) are not yet supported".to_string(),
        ))
    }

    fn compile_slice(
        ctx: &mut CompilationContext,
        expr: &coral_parser::ast::SliceExpr,
    ) -> CodegenResult<u16> {
        let lower_reg = if let Some(lower) = expr.lower {
            ExprCompiler::compile(ctx, lower)?
        } else {
            let none_id = ctx.add_constant_none();
            let reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: reg,
                const_id: none_id,
            });
            reg
        };

        let upper_reg = if let Some(upper) = expr.upper {
            ExprCompiler::compile(ctx, upper)?
        } else {
            let none_id = ctx.add_constant_none();
            let reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: reg,
                const_id: none_id,
            });
            reg
        };

        let step_reg = if let Some(step) = expr.step {
            ExprCompiler::compile(ctx, step)?
        } else {
            let one_id = ctx.add_constant_integer(1);
            let reg = ctx.allocate_register(0)?;
            ctx.emit(Instruction::LoadConst {
                dst: reg,
                const_id: one_id,
            });
            reg
        };

        let slice_tuple_reg = ctx.allocate_register(0)?;
        ctx.emit(Instruction::NewTuple {
            dst: slice_tuple_reg,
            elem_count: 3,
        });

        ctx.emit(Instruction::SetItem {
            obj: slice_tuple_reg,
            key: lower_reg,
            value: lower_reg,
        });

        ctx.emit(Instruction::SetItem {
            obj: slice_tuple_reg,
            key: upper_reg,
            value: upper_reg,
        });

        ctx.emit(Instruction::SetItem {
            obj: slice_tuple_reg,
            key: step_reg,
            value: step_reg,
        });

        Ok(slice_tuple_reg)
    }

    fn classify_type(type_id: u32) -> TypeClass {
        match type_id {
            1 => TypeClass::Int,
            2 => TypeClass::Float,
            3 => TypeClass::String,
            _ => TypeClass::Unknown,
        }
    }

    fn type_to_string(type_id: u32) -> String {
        match type_id {
            1 => "int".to_string(),
            2 => "float".to_string(),
            3 => "str".to_string(),
            0 => "unknown".to_string(),
            _ => format!("type_{}", type_id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeClass {
    Int,
    Float,
    String,
    Unknown,
}
