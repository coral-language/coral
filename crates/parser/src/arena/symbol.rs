//! Symbol table for interned strings.
//!
//! Provides efficient string deduplication and comparison via symbol IDs.

use std::fmt;

/// A unique identifier for an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u32);

impl Symbol {
    /// Create a new symbol from a u32 ID.
    pub fn new(id: u32) -> Self {
        Symbol(id)
    }

    /// Get the underlying ID.
    pub fn as_u32(self) -> u32 {
        self.0
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({})", self.0)
    }
}
