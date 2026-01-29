//! Shard: Deterministic, zero-copy parallel execution
//!
//! This crate provides the core primitives for the shard parallel execution runtime.
//! It enables efficient parallel processing of large datasets with explicit memory
//! management and deterministic behavior.
//!
//! # Core concepts
//!
//! - **Shared segments** - Memory regions that can be accessed by multiple workers
//!   without copying. Use [`shared::SharedSegment`] to create shared data.
//!
//! - **Shards** - Index descriptors for dividing work across workers.
//!   Use [`shards()`] to create shard descriptors.
//!
//! # Accessing shared data: fetch() vs materialize()
//!
//! The [`shared`] module provides two ways to access data:
//!
//! - **`fetch()`** - Returns a view without copying. Use for read-only access.
//! - **`materialize()`** - Copies data to owned form. Use when you need ownership.
//!
//! See the [`shared`] module documentation for detailed guidance.

pub mod shards;
pub mod shared;

// Re-export main types at crate root
pub use shards::{
    shards, AutotuneConfig, BlockSize, Shard, ShardDescriptor, ShardIterator, ShardStrategy,
};

pub use shared::{
    Fetchable, Materializable, SharedDiagnostics, SharedError, SharedSegment, SharedView,
};
