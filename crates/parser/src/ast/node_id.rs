//! Node identification for AST traversal and manipulation.

use std::sync::atomic::{AtomicU32, Ordering};

/// Unique identifier for AST nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(u32);

impl NodeId {
    /// Create a new node ID from a u32.
    pub fn new(id: u32) -> Self {
        NodeId(id)
    }

    /// Get the underlying ID value.
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Special ID for the root node.
    pub const ROOT: NodeId = NodeId(0);

    /// Special ID for invalid/missing nodes.
    pub const INVALID: NodeId = NodeId(u32::MAX);
}

impl std::fmt::Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Node({})", self.0)
    }
}

/// Generator for unique node IDs.
pub struct NodeIdGenerator {
    next_id: AtomicU32,
}

impl NodeIdGenerator {
    /// Create a new node ID generator.
    pub fn new() -> Self {
        NodeIdGenerator {
            next_id: AtomicU32::new(1), // Start at 1, reserve 0 for ROOT
        }
    }

    /// Generate a new unique node ID.
    pub fn next(&self) -> NodeId {
        let id = self.next_id.fetch_add(1, Ordering::SeqCst);
        NodeId(id)
    }

    /// Reset the generator (useful for testing).
    pub fn reset(&self) {
        self.next_id.store(1, Ordering::SeqCst);
    }
}

impl Default for NodeIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}
