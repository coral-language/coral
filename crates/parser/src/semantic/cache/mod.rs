//! Persistent caching system for semantic analysis results.
//!
//! This module provides disk-based caching to persist analysis results across
//! process restarts, improving startup performance for large projects.

pub mod mmap;
pub mod persistent;

pub use mmap::{MmapCacheStats, MmapPersistentCache};
pub use persistent::{CacheConfig, CacheError, PersistentCache};
