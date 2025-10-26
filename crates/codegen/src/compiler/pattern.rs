//! Pattern matching compilation to bytecode

use crate::bytecode::Instruction;
use crate::compiler::context::CompilationContext;
use crate::compiler::expr::ExprCompiler;
use crate::error::CodegenResult;
use coral_parser::ast::{MatchStmt, Pattern};

pub struct PatternCompiler;

impl PatternCompiler {
    pub fn compile_match(ctx: &mut CompilationContext, stmt: &MatchStmt) -> CodegenResult<()> {
        let subject_reg = ExprCompiler::compile(ctx, &stmt.subject)?;

        let mut case_offsets = Vec::new();
        let mut jump_offsets = Vec::new();

        for case in stmt.cases {
            let case_start = ctx.current_instruction_offset();
            case_offsets.push(case_start);

            match Self::compile_pattern(ctx, &case.pattern, subject_reg)? {
                (_pattern_result_reg, Some(_destructured_regs)) => {
                    if let Some(guard_expr) = &case.guard {
                        let guard_reg = ExprCompiler::compile(ctx, guard_expr)?;
                        let exit_offset = ctx.current_instruction_offset();
                        ctx.emit(Instruction::JumpIfFalse {
                            condition: guard_reg,
                            offset: 0,
                        });

                        for stmt in case.body {
                            crate::compiler::stmt::StmtCompiler::compile(ctx, stmt)?;
                        }

                        let next_case_offset = ctx.current_instruction_offset();
                        if let Instruction::JumpIfFalse { offset, .. } =
                            &mut ctx.instructions[exit_offset as usize]
                        {
                            *offset = next_case_offset;
                        }
                    } else {
                        for stmt in case.body {
                            crate::compiler::stmt::StmtCompiler::compile(ctx, stmt)?;
                        }
                    }

                    jump_offsets.push(ctx.current_instruction_offset());
                    ctx.emit(Instruction::Jump { offset: 0 });
                }
                (_pattern_result_reg, None) => {
                    for stmt in case.body {
                        crate::compiler::stmt::StmtCompiler::compile(ctx, stmt)?;
                    }

                    jump_offsets.push(ctx.current_instruction_offset());
                    ctx.emit(Instruction::Jump { offset: 0 });
                }
            }
        }

        let match_end = ctx.current_instruction_offset();
        for jump_offset in jump_offsets {
            if let Instruction::Jump { offset } = &mut ctx.instructions[jump_offset as usize] {
                *offset = match_end;
            }
        }

        Ok(())
    }

    pub fn compile_pattern(
        ctx: &mut CompilationContext,
        pattern: &Pattern,
        subject_reg: u16,
    ) -> CodegenResult<(u16, Option<Vec<u16>>)> {
        match pattern {
            Pattern::MatchAs(p) => {
                if let Some(name) = p.name {
                    ctx.define_local(name.to_string(), subject_reg)?;
                }
                Ok((subject_reg, None))
            }

            Pattern::MatchValue(p) => {
                let const_reg = ExprCompiler::compile(ctx, &p.value)?;
                let result_reg = ctx.allocate_register(0)?;

                ctx.emit(Instruction::EqInt {
                    dst: result_reg,
                    lhs: subject_reg,
                    rhs: const_reg,
                });

                Ok((result_reg, None))
            }

            Pattern::MatchOr(p) => {
                let mut result_reg = ctx.allocate_register(0)?;
                let (first_result, _) = Self::compile_pattern(ctx, &p.patterns[0], subject_reg)?;
                ctx.emit(Instruction::Move {
                    dst: result_reg,
                    src: first_result,
                });

                for pattern_item in &p.patterns[1..] {
                    let (pattern_result, _) =
                        Self::compile_pattern(ctx, pattern_item, subject_reg)?;
                    let new_result = ctx.allocate_register(0)?;
                    ctx.emit(Instruction::Or {
                        dst: new_result,
                        lhs: result_reg,
                        rhs: pattern_result,
                    });
                    result_reg = new_result;
                }

                Ok((result_reg, None))
            }

            Pattern::MatchSequence(p) => {
                let mut destructured = Vec::new();

                for _ in p.patterns.iter() {
                    let reg = ctx.allocate_register(0)?;
                    destructured.push(reg);
                }

                ctx.emit(Instruction::DestructureSeq {
                    base_reg: destructured[0],
                    seq: subject_reg,
                    count: p.patterns.len() as u16,
                });

                Ok((subject_reg, Some(destructured)))
            }

            Pattern::MatchMapping(p) => {
                let mut destructured = Vec::new();

                for _ in p.keys {
                    let reg = ctx.allocate_register(0)?;
                    destructured.push(reg);
                }

                let keys: Vec<u32> = p
                    .keys
                    .iter()
                    .map(|key_expr| {
                        if let coral_parser::ast::Expr::Constant(c) = key_expr {
                            ctx.add_constant_string(c.value.to_string())
                        } else {
                            ctx.add_constant_string("key".to_string())
                        }
                    })
                    .collect();

                ctx.emit(Instruction::DestructureDict {
                    base_reg: destructured[0],
                    dict: subject_reg,
                    keys,
                });

                Ok((subject_reg, Some(destructured)))
            }

            Pattern::MatchClass(p) => {
                let _class_reg = ExprCompiler::compile(ctx, &p.cls)?;
                let result_reg = ctx.allocate_register(0)?;

                ctx.emit(Instruction::MatchType {
                    dst: result_reg,
                    value: subject_reg,
                    type_id: 0,
                });

                Ok((result_reg, None))
            }

            Pattern::MatchSingleton(p) => {
                let const_id = match p.value {
                    coral_parser::ast::patterns::MatchSingleton::True => {
                        ctx.add_constant_bool(true)
                    }
                    coral_parser::ast::patterns::MatchSingleton::False => {
                        ctx.add_constant_bool(false)
                    }
                    coral_parser::ast::patterns::MatchSingleton::None => ctx.add_constant_none(),
                };

                let result_reg = ctx.allocate_register(0)?;

                ctx.emit(Instruction::MatchValue {
                    dst: result_reg,
                    value: subject_reg,
                    const_id,
                });

                Ok((result_reg, None))
            }
        }
    }
}
