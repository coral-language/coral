//! Bytecode instruction definitions
//!
//! This module defines the Instruction enum that represents all executable bytecode.
//! Instructions are designed to be compact and register-based for efficiency.

use super::opcode::Opcode;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Instruction {
    // Register Operations
    Move {
        dst: u16,
        src: u16,
    },
    LoadConst {
        dst: u16,
        const_id: u32,
    },
    LoadGlobal {
        dst: u16,
        name_id: u32,
    },
    StoreGlobal {
        src: u16,
        name_id: u32,
    },
    LoadImport {
        dst: u16,
        module_id: u16,
        symbol_id: u32,
    },
    LoadLocal {
        dst: u16,
        local_id: u16,
    },
    StoreLocal {
        src: u16,
        local_id: u16,
    },

    // Arithmetic - Integers
    AddInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    SubInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    MulInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    DivInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    ModInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    NegInt {
        dst: u16,
        operand: u16,
    },
    PowInt {
        dst: u16,
        base: u16,
        exp: u16,
    },

    // Arithmetic - Floats
    AddFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    SubFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    MulFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    DivFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    ModFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    NegFloat {
        dst: u16,
        operand: u16,
    },
    PowFloat {
        dst: u16,
        base: u16,
        exp: u16,
    },

    // Comparisons - Integers
    EqInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    NotEqInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    LtInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    LtEqInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    GtInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    GtEqInt {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },

    // Comparisons - Floats
    EqFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    LtFloat {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },

    // Boolean Operations
    And {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    Or {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    Not {
        dst: u16,
        operand: u16,
    },
    IsNone {
        dst: u16,
        operand: u16,
    },

    // Control Flow
    Jump {
        offset: u32,
    },
    JumpIfTrue {
        condition: u16,
        offset: u32,
    },
    JumpIfFalse {
        condition: u16,
        offset: u32,
    },
    Call {
        result: u16,
        func_reg: u16,
        arg_base: u16,
        arg_count: u16,
    },
    CallNative {
        result: u16,
        module_id: u16,
        func_id: u32,
        arg_base: u16,
        arg_count: u16,
    },
    Return {
        value: u16,
    },
    Nop,

    // Collections
    NewList {
        dst: u16,
        size_hint: u32,
    },
    NewDict {
        dst: u16,
        size_hint: u32,
    },
    NewSet {
        dst: u16,
        size_hint: u32,
    },
    NewTuple {
        dst: u16,
        elem_count: u16,
    },
    GetItem {
        dst: u16,
        obj: u16,
        key: u16,
    },
    SetItem {
        obj: u16,
        key: u16,
        value: u16,
    },
    GetAttr {
        dst: u16,
        obj: u16,
        attr_id: u32,
    },
    SetAttr {
        obj: u16,
        attr_id: u32,
        value: u16,
    },
    Len {
        dst: u16,
        obj: u16,
    },

    // Pattern Matching
    MatchType {
        dst: u16,
        value: u16,
        type_id: u32,
    },
    MatchValue {
        dst: u16,
        value: u16,
        const_id: u32,
    },
    DestructureSeq {
        base_reg: u16,
        seq: u16,
        count: u16,
    },
    DestructureDict {
        base_reg: u16,
        dict: u16,
        keys: Vec<u32>,
    },

    // Async Operations
    Await {
        result: u16,
        future: u16,
        state_id: u32,
    },
    Resume {
        state_id: u32,
    },
    Yield {
        value: u16,
    },

    // Error Handling
    PropagateError {
        result: u16,
    },
    Raise {
        exception: u16,
    },
    BeginTry {
        handler_offset: u32,
    },
    EndTry,

    // String Operations
    ConcatStr {
        dst: u16,
        lhs: u16,
        rhs: u16,
    },
    FormatStr {
        dst: u16,
        template_id: u32,
        args_base: u16,
        args_count: u16,
    },
    StrLen {
        dst: u16,
        str_reg: u16,
    },

    // Type Operations
    TypeOf {
        dst: u16,
        value: u16,
    },
    IsInstance {
        dst: u16,
        value: u16,
        type_id: u32,
    },
    Cast {
        dst: u16,
        value: u16,
        target_type_id: u32,
    },

    // Debug Operations
    PrintDebug {
        value: u16,
    },
    Breakpoint,
}

impl Instruction {
    pub fn opcode(&self) -> Opcode {
        match self {
            Instruction::Move { .. } => Opcode::Move,
            Instruction::LoadConst { .. } => Opcode::LoadConst,
            Instruction::LoadGlobal { .. } => Opcode::LoadGlobal,
            Instruction::StoreGlobal { .. } => Opcode::StoreGlobal,
            Instruction::LoadImport { .. } => Opcode::LoadImport,
            Instruction::LoadLocal { .. } => Opcode::LoadLocal,
            Instruction::StoreLocal { .. } => Opcode::StoreLocal,
            Instruction::AddInt { .. } => Opcode::AddInt,
            Instruction::SubInt { .. } => Opcode::SubInt,
            Instruction::MulInt { .. } => Opcode::MulInt,
            Instruction::DivInt { .. } => Opcode::DivInt,
            Instruction::ModInt { .. } => Opcode::ModInt,
            Instruction::NegInt { .. } => Opcode::NegInt,
            Instruction::PowInt { .. } => Opcode::PowInt,
            Instruction::AddFloat { .. } => Opcode::AddFloat,
            Instruction::SubFloat { .. } => Opcode::SubFloat,
            Instruction::MulFloat { .. } => Opcode::MulFloat,
            Instruction::DivFloat { .. } => Opcode::DivFloat,
            Instruction::ModFloat { .. } => Opcode::ModFloat,
            Instruction::NegFloat { .. } => Opcode::NegFloat,
            Instruction::PowFloat { .. } => Opcode::PowFloat,
            Instruction::EqInt { .. } => Opcode::EqInt,
            Instruction::NotEqInt { .. } => Opcode::NotEqInt,
            Instruction::LtInt { .. } => Opcode::LtInt,
            Instruction::LtEqInt { .. } => Opcode::LtEqInt,
            Instruction::GtInt { .. } => Opcode::GtInt,
            Instruction::GtEqInt { .. } => Opcode::GtEqInt,
            Instruction::EqFloat { .. } => Opcode::EqFloat,
            Instruction::LtFloat { .. } => Opcode::LtFloat,
            Instruction::And { .. } => Opcode::And,
            Instruction::Or { .. } => Opcode::Or,
            Instruction::Not { .. } => Opcode::Not,
            Instruction::IsNone { .. } => Opcode::IsNone,
            Instruction::Jump { .. } => Opcode::Jump,
            Instruction::JumpIfTrue { .. } => Opcode::JumpIfTrue,
            Instruction::JumpIfFalse { .. } => Opcode::JumpIfFalse,
            Instruction::Call { .. } => Opcode::Call,
            Instruction::CallNative { .. } => Opcode::CallNative,
            Instruction::Return { .. } => Opcode::Return,
            Instruction::Nop => Opcode::Nop,
            Instruction::NewList { .. } => Opcode::NewList,
            Instruction::NewDict { .. } => Opcode::NewDict,
            Instruction::NewSet { .. } => Opcode::NewSet,
            Instruction::NewTuple { .. } => Opcode::NewTuple,
            Instruction::GetItem { .. } => Opcode::GetItem,
            Instruction::SetItem { .. } => Opcode::SetItem,
            Instruction::GetAttr { .. } => Opcode::GetAttr,
            Instruction::SetAttr { .. } => Opcode::SetAttr,
            Instruction::Len { .. } => Opcode::Len,
            Instruction::MatchType { .. } => Opcode::MatchType,
            Instruction::MatchValue { .. } => Opcode::MatchValue,
            Instruction::DestructureSeq { .. } => Opcode::DestructureSeq,
            Instruction::DestructureDict { .. } => Opcode::DestructureDict,
            Instruction::Await { .. } => Opcode::Await,
            Instruction::Resume { .. } => Opcode::Resume,
            Instruction::Yield { .. } => Opcode::Yield,
            Instruction::PropagateError { .. } => Opcode::PropagateError,
            Instruction::Raise { .. } => Opcode::Raise,
            Instruction::BeginTry { .. } => Opcode::BeginTry,
            Instruction::EndTry => Opcode::EndTry,
            Instruction::ConcatStr { .. } => Opcode::ConcatStr,
            Instruction::FormatStr { .. } => Opcode::FormatStr,
            Instruction::StrLen { .. } => Opcode::StrLen,
            Instruction::TypeOf { .. } => Opcode::TypeOf,
            Instruction::IsInstance { .. } => Opcode::IsInstance,
            Instruction::Cast { .. } => Opcode::Cast,
            Instruction::PrintDebug { .. } => Opcode::PrintDebug,
            Instruction::Breakpoint => Opcode::Breakpoint,
        }
    }
}
