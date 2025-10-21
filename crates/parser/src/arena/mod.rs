//! Arena allocator for AST nodes.
//!
//! Uses `bumpalo` for efficient bump allocation of immutable AST nodes.

use bumpalo::Bump;
use std::cell::UnsafeCell;

pub mod interner;
pub mod symbol;

pub use interner::Interner;
pub use symbol::Symbol;

/// Thread-safe arena allocator for AST nodes.
///
/// SAFETY: This is single-threaded by design (UnsafeCell). The parser
/// is designed to be used on a single thread per Arena. For multi-threaded
/// access, wrap the Arena in Arc<Mutex<Arena>>.
pub struct Arena {
    inner: UnsafeCell<Bump>,
}

/// Allocation strategy for the arena.
#[derive(Debug, Clone, Copy)]
pub enum AllocationStrategy {
    /// Default bumpalo allocation strategy.
    Default,
    /// Optimized for small AST nodes (expressions, identifiers).
    SmallNodes,
    /// Optimized for large AST nodes (function bodies, modules).
    LargeNodes,
    /// Custom chunk size in bytes.
    Custom(usize),
}

impl Arena {
    /// Create a new arena with the default allocation strategy.
    pub fn new() -> Self {
        Arena {
            inner: UnsafeCell::new(Bump::new()),
        }
    }

    /// Create a new arena with a specific allocation strategy.
    pub fn with_strategy(strategy: AllocationStrategy) -> Self {
        let bump = match strategy {
            AllocationStrategy::Default => Bump::new(),
            AllocationStrategy::SmallNodes => Bump::with_capacity(4096), // 4KB chunks
            AllocationStrategy::LargeNodes => Bump::with_capacity(65536), // 64KB chunks
            AllocationStrategy::Custom(size) => Bump::with_capacity(size),
        };
        Arena {
            inner: UnsafeCell::new(bump),
        }
    }

    /// Allocate a value in the arena.
    pub fn alloc<T>(&self, value: T) -> &T {
        unsafe { (*self.inner.get()).alloc(value) }
    }

    /// Allocate a slice in the arena.
    pub fn alloc_slice<T: Copy>(&self, values: &[T]) -> &[T] {
        unsafe { (*self.inner.get()).alloc_slice_copy(values) }
    }

    /// Allocate a string in the arena.
    pub fn alloc_str(&self, s: &str) -> &str {
        unsafe { (*self.inner.get()).alloc_str(s) }
    }

    /// Allocate a vector as a slice in the arena (for Copy types).
    pub fn alloc_vec<T: Copy>(&self, vec: Vec<T>) -> &[T] {
        let bump = unsafe { &mut *self.inner.get() };
        bump.alloc_slice_copy(&vec)
    }

    /// Allocate a vector of non-Copy items as a slice.
    pub fn alloc_slice_vec<T>(&self, vec: Vec<T>) -> &[T] {
        let bump = unsafe { &mut *self.inner.get() };
        bump.alloc_slice_fill_iter(vec)
    }

    /// Allocate any iterator of items as a slice (works with SmallVec, Vec, etc).
    pub fn alloc_slice_iter<T, I>(&self, iter: I) -> &[T]
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        let bump = unsafe { &mut *self.inner.get() };
        bump.alloc_slice_fill_iter(iter)
    }

    /// Get the arena's current allocation size (for diagnostics).
    pub fn allocated_bytes(&self) -> usize {
        unsafe { (*self.inner.get()).allocated_bytes() }
    }

    /// Get the number of memory chunks allocated.
    pub fn chunk_count(&self) -> usize {
        unsafe { (*self.inner.get()).chunk_capacity() }
    }

    /// Get detailed memory statistics for the arena.
    pub fn memory_stats(&self) -> ArenaStats {
        let bump = unsafe { &*self.inner.get() };
        ArenaStats {
            allocated_bytes: bump.allocated_bytes(),
            chunk_count: bump.chunk_capacity(),
        }
    }

    /// Reset the arena, freeing all allocated memory.
    /// This is useful for REPL or incremental parsing scenarios.
    ///
    /// The arena can be reused after calling this method.
    pub fn reset(&mut self) {
        unsafe {
            (*self.inner.get()).reset();
        }
    }

    /// Prepare the arena for reuse by resetting it.
    /// This is an alias for `reset()` that emphasizes reuse semantics.
    pub fn prepare_for_reuse(&mut self) {
        self.reset();
    }

    /// Check if the arena is empty (no allocations).
    pub fn is_empty(&self) -> bool {
        self.allocated_bytes() == 0
    }
}

/// Memory statistics for an arena.
#[derive(Debug, Clone, Copy)]
pub struct ArenaStats {
    /// Total bytes allocated in the arena.
    pub allocated_bytes: usize,
    /// Number of memory chunks.
    pub chunk_count: usize,
}

impl ArenaStats {
    /// Format the statistics as a human-readable string.
    pub fn format_human_readable(&self) -> String {
        let bytes = self.allocated_bytes;
        let size_str = if bytes < 1024 {
            format!("{} B", bytes)
        } else if bytes < 1024 * 1024 {
            format!("{:.2} KB", bytes as f64 / 1024.0)
        } else {
            format!("{:.2} MB", bytes as f64 / (1024.0 * 1024.0))
        };

        format!(
            "Arena Stats: {} allocated across {} chunk(s)",
            size_str, self.chunk_count
        )
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_alloc() {
        let arena = Arena::new();
        let val = arena.alloc(42);
        assert_eq!(*val, 42);
    }
}
