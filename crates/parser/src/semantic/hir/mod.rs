//! High-level Intermediate Representation (HIR)
//! A typed, lowered representation of the AST

pub mod class_analysis;
pub mod context;
pub mod lower;
pub mod typed_expr;
pub mod typed_item;
pub mod typed_pattern;
pub mod typed_stmt;
pub mod validate;

pub use lower::HirLowerer;
pub use typed_expr::TypedExpr;
pub use typed_item::TypedItem;
pub use typed_pattern::TypedPattern;
pub use typed_stmt::TypedStmt;
