//! High-level Intermediate Representation (HIR)
//! A typed, lowered representation of the AST

pub mod class_analysis;
pub mod context;
pub mod lower;
pub mod owned;
pub mod typed_expr;
pub mod typed_item;
pub mod typed_pattern;
pub mod typed_stmt;
pub mod validate;

pub use lower::HirLowerer;
pub use owned::{
    OwnedAnnAssignStmt, OwnedArg, OwnedAssignStmt, OwnedAttributeExpr, OwnedAugAssignStmt,
    OwnedAwaitExpr, OwnedBinOpExpr, OwnedBoolOpExpr, OwnedCallExpr, OwnedClassDefStmt,
    OwnedCompareExpr, OwnedConstantExpr, OwnedDictExpr, OwnedExceptHandler, OwnedExprStmt,
    OwnedForStmt, OwnedFormattedValueExpr, OwnedFuncDefStmt, OwnedIfExpExpr, OwnedIfStmt,
    OwnedJoinedStrExpr, OwnedLambdaExpr, OwnedListExpr, OwnedMatchCase, OwnedMatchStmt,
    OwnedNameExpr, OwnedNamedExpr, OwnedRaiseStmt, OwnedReturnStmt, OwnedSetExpr, OwnedSliceExpr,
    OwnedStarredExpr, OwnedSubscriptExpr, OwnedTStringExpr, OwnedTryStmt, OwnedTupleExpr,
    OwnedTypedExpr, OwnedTypedModule, OwnedTypedStmt, OwnedUnaryOpExpr, OwnedWhileStmt,
    OwnedYieldExpr, OwnedYieldFromExpr, convert_to_owned,
};
pub use typed_expr::TypedExpr;
pub use typed_item::TypedItem;
pub use typed_pattern::TypedPattern;
pub use typed_stmt::TypedStmt;
