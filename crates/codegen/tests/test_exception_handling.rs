//! Exception Handling Tests
//!
//! Tests for error handling and exception mechanisms in bytecode including:
//! - Exception handlers
//! - Try/Catch blocks
//! - Error propagation
//! - Raise instructions

use coral_codegen::bytecode::Instruction;
use coral_codegen::bytecode::function::ExceptionHandler;
use coral_codegen::compiler::context::CompilationContext;

#[test]
fn test_exception_handler_creation() {
    let handler = ExceptionHandler {
        start_offset: 0,
        end_offset: 10,
        handler_offset: 20,
        exception_type: 1,
    };

    assert_eq!(handler.start_offset, 0);
    assert_eq!(handler.end_offset, 10);
    assert_eq!(handler.handler_offset, 20);
    assert_eq!(handler.exception_type, 1);
}

#[test]
fn test_exception_handler_range_calculation() {
    let handler = ExceptionHandler {
        start_offset: 5,
        end_offset: 15,
        handler_offset: 25,
        exception_type: 2,
    };

    assert_eq!(handler.end_offset - handler.start_offset, 10);
}

#[test]
fn test_multiple_exception_handlers() {
    let handlers = [
        ExceptionHandler {
            start_offset: 0,
            end_offset: 10,
            handler_offset: 20,
            exception_type: 1,
        },
        ExceptionHandler {
            start_offset: 10,
            end_offset: 20,
            handler_offset: 30,
            exception_type: 2,
        },
        ExceptionHandler {
            start_offset: 20,
            end_offset: 30,
            handler_offset: 40,
            exception_type: 3,
        },
    ];

    assert_eq!(handlers.len(), 3);
    assert_eq!(handlers[0].start_offset, 0);
    assert_eq!(handlers[1].handler_offset, 30);
    assert_eq!(handlers[2].exception_type, 3);
}

#[test]
fn test_nested_exception_handlers() {
    let handlers = [
        ExceptionHandler {
            start_offset: 0,
            end_offset: 20,
            handler_offset: 50,
            exception_type: 1,
        },
        ExceptionHandler {
            start_offset: 5,
            end_offset: 15,
            handler_offset: 60,
            exception_type: 2,
        },
        ExceptionHandler {
            start_offset: 20,
            end_offset: 30,
            handler_offset: 70,
            exception_type: 3,
        },
    ];

    assert_eq!(handlers.len(), 3);

    let outer_handler = &handlers[0];
    let inner_handler = &handlers[1];

    assert!(inner_handler.start_offset >= outer_handler.start_offset);
    assert!(inner_handler.end_offset <= outer_handler.end_offset);
}

#[test]
fn test_begin_try_end_try_block() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::BeginTry {
        handler_offset: 100,
    });

    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: 1,
    });

    ctx.emit(Instruction::EndTry);

    assert_eq!(ctx.instructions.len(), 3);
    assert!(matches!(ctx.instructions[0], Instruction::BeginTry { .. }));
    assert!(matches!(ctx.instructions[2], Instruction::EndTry));
}

#[test]
fn test_raise_instruction_emission() {
    let mut ctx = CompilationContext::new(256);

    let exc_id = ctx.add_constant_string("RuntimeError".to_string());
    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: exc_id,
    });

    ctx.emit(Instruction::Raise { exception: 0 });

    assert_eq!(ctx.instructions.len(), 2);
    assert!(matches!(
        ctx.instructions[1],
        Instruction::Raise { exception: 0 }
    ));
}

#[test]
fn test_propagate_error_instruction() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: 1,
    });

    ctx.emit(Instruction::PropagateError { result: 0 });

    assert_eq!(ctx.instructions.len(), 2);
    assert!(matches!(
        ctx.instructions[1],
        Instruction::PropagateError { result: 0 }
    ));
}

#[test]
fn test_nested_try_blocks() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::BeginTry { handler_offset: 50 });
    ctx.emit(Instruction::BeginTry {
        handler_offset: 100,
    });

    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: 1,
    });

    ctx.emit(Instruction::EndTry);
    ctx.emit(Instruction::EndTry);

    assert_eq!(ctx.instructions.len(), 5);
}

#[test]
fn test_exception_type_specificity() {
    let handlers = [
        ExceptionHandler {
            start_offset: 0,
            end_offset: 50,
            handler_offset: 100,
            exception_type: 1, // Generic exception
        },
        ExceptionHandler {
            start_offset: 0,
            end_offset: 50,
            handler_offset: 200,
            exception_type: 2, // More specific exception
        },
    ];

    assert_eq!(handlers[0].exception_type, 1);
    assert_eq!(handlers[1].exception_type, 2);
    assert_ne!(handlers[0].exception_type, handlers[1].exception_type);
}

#[test]
fn test_exception_handler_with_code_generation() {
    let mut ctx = CompilationContext::new(256);

    ctx.emit(Instruction::BeginTry { handler_offset: 50 });

    let val1 = ctx.add_constant_integer(10);
    let val2 = ctx.add_constant_integer(20);

    ctx.emit(Instruction::LoadConst {
        dst: 0,
        const_id: val1,
    });
    ctx.emit(Instruction::LoadConst {
        dst: 1,
        const_id: val2,
    });

    ctx.emit(Instruction::AddInt {
        dst: 2,
        lhs: 0,
        rhs: 1,
    });

    ctx.emit(Instruction::EndTry);

    assert!(ctx.instructions.len() > 2);
    assert!(
        ctx.instructions
            .iter()
            .any(|i| matches!(i, Instruction::BeginTry { .. }))
    );
    assert!(
        ctx.instructions
            .iter()
            .any(|i| matches!(i, Instruction::EndTry))
    );
}
