//! Shard: Deterministic, zero-copy parallel execution
//!
//! This crate provides the core primitives for the shard parallel execution runtime.
//! It enables efficient parallel processing of large datasets with explicit memory
//! management and deterministic behavior.

pub mod shards;

// Re-export main types at crate root
pub use shards::{
    shards, AutotuneConfig, BlockSize, Shard, ShardDescriptor, ShardIterator, ShardStrategy,
};
