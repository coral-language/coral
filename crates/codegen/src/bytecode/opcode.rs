//! Opcode definitions for the bytecode instruction set
//!
//! This module defines all ~60 opcodes organized by category:
//! - Register operations (move, load, store)
//! - Arithmetic operations (type-specialized)
//! - Control flow (jump, call, return)
//! - Async operations (await, resume, yield)
//! - Memory operations (list, dict, attributes)
//! - Pattern matching and error handling

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
#[repr(u8)]
pub enum Opcode {
    // Register Operations (0-10)
    Move = 0,
    LoadConst = 1,
    LoadGlobal = 2,
    StoreGlobal = 3,
    LoadImport = 4,
    LoadLocal = 5,
    StoreLocal = 6,

    // Arithmetic - Integers (11-20)
    AddInt = 11,
    SubInt = 12,
    MulInt = 13,
    DivInt = 14,
    ModInt = 15,
    NegInt = 16,
    PowInt = 17,

    // Arithmetic - Floats (21-30)
    AddFloat = 21,
    SubFloat = 22,
    MulFloat = 23,
    DivFloat = 24,
    ModFloat = 25,
    NegFloat = 26,
    PowFloat = 27,

    // Comparisons (31-38)
    EqInt = 31,
    NotEqInt = 32,
    LtInt = 33,
    LtEqInt = 34,
    GtInt = 35,
    GtEqInt = 36,
    EqFloat = 37,
    LtFloat = 38,

    // Boolean Operations (39-42)
    And = 39,
    Or = 40,
    Not = 41,
    IsNone = 42,

    // Control Flow (43-50)
    Jump = 43,
    JumpIfTrue = 44,
    JumpIfFalse = 45,
    Call = 46,
    CallNative = 47,
    Return = 48,
    Nop = 49,

    // Collections (51-60)
    NewList = 51,
    NewDict = 52,
    NewSet = 53,
    NewTuple = 54,
    GetItem = 55,
    SetItem = 56,
    GetAttr = 57,
    SetAttr = 58,
    Len = 59,

    // Pattern Matching (61-65)
    MatchType = 61,
    MatchValue = 62,
    DestructureSeq = 63,
    DestructureDict = 64,

    // Async Operations (66-70)
    Await = 66,
    Resume = 67,
    Yield = 68,
    YieldFrom = 69,
    NewGenerator = 70,

    // Error Handling (71-75)
    PropagateError = 71,
    Raise = 72,
    BeginTry = 73,
    EndTry = 74,

    // String Operations (76-80)
    ConcatStr = 76,
    FormatStr = 77,
    StrLen = 78,

    // Type Operations (81-85)
    TypeOf = 81,
    IsInstance = 82,
    Cast = 83,

    // Special Operations (86-90)
    PrintDebug = 86,
    Breakpoint = 87,
}

impl Opcode {
    pub fn name(&self) -> &'static str {
        match self {
            Opcode::Move => "Move",
            Opcode::LoadConst => "LoadConst",
            Opcode::LoadGlobal => "LoadGlobal",
            Opcode::StoreGlobal => "StoreGlobal",
            Opcode::LoadImport => "LoadImport",
            Opcode::LoadLocal => "LoadLocal",
            Opcode::StoreLocal => "StoreLocal",
            Opcode::AddInt => "AddInt",
            Opcode::SubInt => "SubInt",
            Opcode::MulInt => "MulInt",
            Opcode::DivInt => "DivInt",
            Opcode::ModInt => "ModInt",
            Opcode::NegInt => "NegInt",
            Opcode::PowInt => "PowInt",
            Opcode::AddFloat => "AddFloat",
            Opcode::SubFloat => "SubFloat",
            Opcode::MulFloat => "MulFloat",
            Opcode::DivFloat => "DivFloat",
            Opcode::ModFloat => "ModFloat",
            Opcode::NegFloat => "NegFloat",
            Opcode::PowFloat => "PowFloat",
            Opcode::EqInt => "EqInt",
            Opcode::NotEqInt => "NotEqInt",
            Opcode::LtInt => "LtInt",
            Opcode::LtEqInt => "LtEqInt",
            Opcode::GtInt => "GtInt",
            Opcode::GtEqInt => "GtEqInt",
            Opcode::EqFloat => "EqFloat",
            Opcode::LtFloat => "LtFloat",
            Opcode::And => "And",
            Opcode::Or => "Or",
            Opcode::Not => "Not",
            Opcode::IsNone => "IsNone",
            Opcode::Jump => "Jump",
            Opcode::JumpIfTrue => "JumpIfTrue",
            Opcode::JumpIfFalse => "JumpIfFalse",
            Opcode::Call => "Call",
            Opcode::CallNative => "CallNative",
            Opcode::Return => "Return",
            Opcode::Nop => "Nop",
            Opcode::NewList => "NewList",
            Opcode::NewDict => "NewDict",
            Opcode::NewSet => "NewSet",
            Opcode::NewTuple => "NewTuple",
            Opcode::GetItem => "GetItem",
            Opcode::SetItem => "SetItem",
            Opcode::GetAttr => "GetAttr",
            Opcode::SetAttr => "SetAttr",
            Opcode::Len => "Len",
            Opcode::MatchType => "MatchType",
            Opcode::MatchValue => "MatchValue",
            Opcode::DestructureSeq => "DestructureSeq",
            Opcode::DestructureDict => "DestructureDict",
            Opcode::Await => "Await",
            Opcode::Resume => "Resume",
            Opcode::Yield => "Yield",
            Opcode::YieldFrom => "YieldFrom",
            Opcode::NewGenerator => "NewGenerator",
            Opcode::PropagateError => "PropagateError",
            Opcode::Raise => "Raise",
            Opcode::BeginTry => "BeginTry",
            Opcode::EndTry => "EndTry",
            Opcode::ConcatStr => "ConcatStr",
            Opcode::FormatStr => "FormatStr",
            Opcode::StrLen => "StrLen",
            Opcode::TypeOf => "TypeOf",
            Opcode::IsInstance => "IsInstance",
            Opcode::Cast => "Cast",
            Opcode::PrintDebug => "PrintDebug",
            Opcode::Breakpoint => "Breakpoint",
        }
    }

    pub fn from_u8(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Opcode::Move),
            1 => Some(Opcode::LoadConst),
            2 => Some(Opcode::LoadGlobal),
            3 => Some(Opcode::StoreGlobal),
            4 => Some(Opcode::LoadImport),
            5 => Some(Opcode::LoadLocal),
            6 => Some(Opcode::StoreLocal),
            11 => Some(Opcode::AddInt),
            12 => Some(Opcode::SubInt),
            13 => Some(Opcode::MulInt),
            14 => Some(Opcode::DivInt),
            15 => Some(Opcode::ModInt),
            16 => Some(Opcode::NegInt),
            17 => Some(Opcode::PowInt),
            21 => Some(Opcode::AddFloat),
            22 => Some(Opcode::SubFloat),
            23 => Some(Opcode::MulFloat),
            24 => Some(Opcode::DivFloat),
            25 => Some(Opcode::ModFloat),
            26 => Some(Opcode::NegFloat),
            27 => Some(Opcode::PowFloat),
            31 => Some(Opcode::EqInt),
            32 => Some(Opcode::NotEqInt),
            33 => Some(Opcode::LtInt),
            34 => Some(Opcode::LtEqInt),
            35 => Some(Opcode::GtInt),
            36 => Some(Opcode::GtEqInt),
            37 => Some(Opcode::EqFloat),
            38 => Some(Opcode::LtFloat),
            39 => Some(Opcode::And),
            40 => Some(Opcode::Or),
            41 => Some(Opcode::Not),
            42 => Some(Opcode::IsNone),
            43 => Some(Opcode::Jump),
            44 => Some(Opcode::JumpIfTrue),
            45 => Some(Opcode::JumpIfFalse),
            46 => Some(Opcode::Call),
            47 => Some(Opcode::CallNative),
            48 => Some(Opcode::Return),
            49 => Some(Opcode::Nop),
            51 => Some(Opcode::NewList),
            52 => Some(Opcode::NewDict),
            53 => Some(Opcode::NewSet),
            54 => Some(Opcode::NewTuple),
            55 => Some(Opcode::GetItem),
            56 => Some(Opcode::SetItem),
            57 => Some(Opcode::GetAttr),
            58 => Some(Opcode::SetAttr),
            59 => Some(Opcode::Len),
            61 => Some(Opcode::MatchType),
            62 => Some(Opcode::MatchValue),
            63 => Some(Opcode::DestructureSeq),
            64 => Some(Opcode::DestructureDict),
            66 => Some(Opcode::Await),
            67 => Some(Opcode::Resume),
            68 => Some(Opcode::Yield),
            69 => Some(Opcode::YieldFrom),
            70 => Some(Opcode::NewGenerator),
            71 => Some(Opcode::PropagateError),
            72 => Some(Opcode::Raise),
            73 => Some(Opcode::BeginTry),
            74 => Some(Opcode::EndTry),
            76 => Some(Opcode::ConcatStr),
            77 => Some(Opcode::FormatStr),
            78 => Some(Opcode::StrLen),
            81 => Some(Opcode::TypeOf),
            82 => Some(Opcode::IsInstance),
            83 => Some(Opcode::Cast),
            86 => Some(Opcode::PrintDebug),
            87 => Some(Opcode::Breakpoint),
            _ => None,
        }
    }
}
