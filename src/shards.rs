//! Shard descriptor creation with autotuning
//!
//! Produces shard descriptors (index ranges or interleaved indices) for use
//! with parallel execution. Supports both contiguous and strided strategies.

use thiserror::Error;

/// Errors that can occur during shard creation
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ShardError {
    #[error("n must be greater than 0, got {0}")]
    InvalidN(usize),

    #[error("block_size must be greater than 0, got {0}")]
    InvalidBlockSize(usize),

    #[error("worker_count must be greater than 0 for autotuning")]
    NoWorkers,
}

/// Strategy for assigning indices to shards
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ShardStrategy {
    /// Consecutive indices per shard (e.g., [0-99], [100-199], ...)
    /// Good for cache locality when accessing contiguous memory regions
    #[default]
    Contiguous,

    /// Interleaved indices across shards (e.g., [0,4,8,...], [1,5,9,...], ...)
    /// Good for load balancing when work varies by position
    Strided,
}

/// Block size specification for shard creation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BlockSize {
    /// Automatically determine block size based on worker count and heuristics
    #[default]
    Auto,

    /// Explicit block size (number of items per shard)
    Explicit(usize),
}

/// Configuration for autotuning block size
#[derive(Debug, Clone, Copy)]
pub struct AutotuneConfig {
    /// Number of worker processes available
    pub worker_count: usize,

    /// Target minimum shards per worker (for load balancing)
    /// Default: 4 (allows stealing/balancing)
    pub min_shards_per_worker: usize,

    /// Target maximum shards per worker (to limit dispatch overhead)
    /// Default: 64
    pub max_shards_per_worker: usize,

    /// Expected scratch memory per item in bytes (for memory budgeting)
    /// Default: 0 (no memory constraint)
    pub scratch_bytes_per_item: usize,

    /// Total memory budget for scratch space in bytes
    /// Default: 0 (no budget constraint)
    pub scratch_budget_bytes: usize,
}

impl Default for AutotuneConfig {
    fn default() -> Self {
        Self {
            worker_count: 1,
            min_shards_per_worker: 4,
            max_shards_per_worker: 64,
            scratch_bytes_per_item: 0,
            scratch_budget_bytes: 0,
        }
    }
}

impl AutotuneConfig {
    /// Create a new autotuning configuration with the given worker count
    pub fn with_workers(worker_count: usize) -> Self {
        Self {
            worker_count,
            ..Default::default()
        }
    }
}

/// A single shard descriptor
///
/// Shards can be either:
/// - A contiguous range of indices
/// - A strided set of indices
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Shard {
    /// Contiguous range [start, end)
    Range { start: usize, end: usize },

    /// Strided indices: start, start+stride, start+2*stride, ... up to limit
    Strided {
        start: usize,
        stride: usize,
        count: usize,
    },
}

impl Shard {
    /// Returns the number of indices in this shard
    pub fn len(&self) -> usize {
        match self {
            Shard::Range { start, end } => end - start,
            Shard::Strided { count, .. } => *count,
        }
    }

    /// Returns true if this shard has no indices
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator over the indices in this shard
    pub fn indices(&self) -> ShardIndices {
        match self {
            Shard::Range { start, end } => ShardIndices::Range(*start..*end),
            Shard::Strided {
                start,
                stride,
                count,
            } => ShardIndices::Strided {
                current: *start,
                stride: *stride,
                remaining: *count,
            },
        }
    }

    /// Returns the indices as a vector (useful for R interop)
    pub fn to_vec(&self) -> Vec<usize> {
        self.indices().collect()
    }

    /// Returns the indices as 1-based (R convention)
    pub fn to_vec_1based(&self) -> Vec<usize> {
        self.indices().map(|i| i + 1).collect()
    }
}

/// Iterator over shard indices
pub enum ShardIndices {
    Range(std::ops::Range<usize>),
    Strided {
        current: usize,
        stride: usize,
        remaining: usize,
    },
}

impl Iterator for ShardIndices {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ShardIndices::Range(r) => r.next(),
            ShardIndices::Strided {
                current,
                stride,
                remaining,
            } => {
                if *remaining == 0 {
                    None
                } else {
                    let val = *current;
                    *current += *stride;
                    *remaining -= 1;
                    Some(val)
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = match self {
            ShardIndices::Range(r) => r.len(),
            ShardIndices::Strided { remaining, .. } => *remaining,
        };
        (len, Some(len))
    }
}

impl ExactSizeIterator for ShardIndices {}

/// Collection of shard descriptors
#[derive(Debug, Clone)]
pub struct ShardDescriptor {
    /// Total number of items being sharded
    pub n: usize,

    /// Strategy used for sharding
    pub strategy: ShardStrategy,

    /// Computed block size
    pub block_size: usize,

    /// The individual shards
    pub shards: Vec<Shard>,
}

impl ShardDescriptor {
    /// Returns the number of shards
    pub fn num_shards(&self) -> usize {
        self.shards.len()
    }

    /// Returns an iterator over the shards
    pub fn iter(&self) -> ShardIterator<'_> {
        ShardIterator {
            inner: self.shards.iter(),
        }
    }

    /// Verify that all indices from 0 to n-1 are covered exactly once
    #[cfg(test)]
    fn verify_coverage(&self) -> bool {
        let mut seen = vec![false; self.n];
        for shard in &self.shards {
            for idx in shard.indices() {
                if idx >= self.n || seen[idx] {
                    return false;
                }
                seen[idx] = true;
            }
        }
        seen.iter().all(|&b| b)
    }
}

impl<'a> IntoIterator for &'a ShardDescriptor {
    type Item = &'a Shard;
    type IntoIter = ShardIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterator over shards in a descriptor
pub struct ShardIterator<'a> {
    inner: std::slice::Iter<'a, Shard>,
}

impl<'a> Iterator for ShardIterator<'a> {
    type Item = &'a Shard;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a> ExactSizeIterator for ShardIterator<'a> {}

/// Compute optimal block size using autotuning heuristics
///
/// The autotuning considers:
/// 1. Worker count: more workers = smaller blocks for parallelism
/// 2. Load balancing: at least min_shards_per_worker to allow work stealing
/// 3. Dispatch overhead: at most max_shards_per_worker to limit overhead
/// 4. Memory budget: if scratch_bytes_per_item is set, constrain block size
fn autotune_block_size(n: usize, config: &AutotuneConfig) -> usize {
    if config.worker_count == 0 {
        // Fallback: single block
        return n;
    }

    // Target number of shards based on worker count
    let min_shards = config.worker_count * config.min_shards_per_worker;
    let max_shards = config.worker_count * config.max_shards_per_worker;

    // Compute block size from target shard counts
    // More shards = smaller blocks
    let block_from_min = n.div_ceil(min_shards);
    let block_from_max = n / max_shards.max(1);

    // Start with block size that gives us good parallelism
    let mut block_size = block_from_min.max(1);

    // Apply memory budget constraint if specified
    if config.scratch_bytes_per_item > 0 && config.scratch_budget_bytes > 0 {
        let max_items_per_budget =
            config.scratch_budget_bytes / config.scratch_bytes_per_item.max(1);
        let memory_constrained_block = max_items_per_budget / config.worker_count.max(1);
        if memory_constrained_block > 0 && memory_constrained_block < block_size {
            block_size = memory_constrained_block;
        }
    }

    // Ensure we don't have too few shards (would hurt load balancing)
    // and not too many (would increase dispatch overhead)
    let num_shards = n.div_ceil(block_size);
    if num_shards < min_shards && block_from_min > 1 {
        block_size = block_from_min;
    }
    if num_shards > max_shards && block_from_max > 0 {
        block_size = block_from_max.max(block_size);
    }

    // At minimum, one item per shard
    block_size.max(1)
}

/// Create contiguous shards covering [0, n)
fn create_contiguous_shards(n: usize, block_size: usize) -> Vec<Shard> {
    let num_shards = n.div_ceil(block_size);
    let mut shards = Vec::with_capacity(num_shards);

    let mut start = 0;
    while start < n {
        let end = (start + block_size).min(n);
        shards.push(Shard::Range { start, end });
        start = end;
    }

    shards
}

/// Create strided shards covering [0, n)
///
/// With k shards, shard i gets indices: i, i+k, i+2k, ...
fn create_strided_shards(n: usize, num_shards: usize) -> Vec<Shard> {
    let num_shards = num_shards.max(1).min(n);
    let mut shards = Vec::with_capacity(num_shards);

    for start in 0..num_shards {
        // Count how many indices this shard gets
        let count = (n - start).div_ceil(num_shards);
        shards.push(Shard::Strided {
            start,
            stride: num_shards,
            count,
        });
    }

    shards
}

/// Create shard descriptors for parallel execution
///
/// # Arguments
///
/// * `n` - Total number of items to shard (must be > 0)
/// * `block_size` - Items per shard, or Auto for autotuning
/// * `strategy` - How to assign indices to shards
///
/// # Returns
///
/// A `ShardDescriptor` containing the shards, or an error if parameters are invalid.
///
/// # Example
///
/// ```
/// use shard::{shards, BlockSize, ShardStrategy, AutotuneConfig};
///
/// // Auto-tuned block size for 8 workers
/// let config = AutotuneConfig::with_workers(8);
/// let desc = shards(1000, BlockSize::Auto, ShardStrategy::Contiguous, &config).unwrap();
///
/// // Each shard covers a contiguous range
/// for shard in &desc {
///     println!("Shard: {:?}", shard);
/// }
/// ```
pub fn shards(
    n: usize,
    block_size: BlockSize,
    strategy: ShardStrategy,
    config: &AutotuneConfig,
) -> Result<ShardDescriptor, ShardError> {
    if n == 0 {
        return Err(ShardError::InvalidN(0));
    }

    let computed_block_size = match block_size {
        BlockSize::Auto => {
            if config.worker_count == 0 {
                return Err(ShardError::NoWorkers);
            }
            autotune_block_size(n, config)
        }
        BlockSize::Explicit(size) => {
            if size == 0 {
                return Err(ShardError::InvalidBlockSize(0));
            }
            size
        }
    };

    let shards = match strategy {
        ShardStrategy::Contiguous => create_contiguous_shards(n, computed_block_size),
        ShardStrategy::Strided => {
            // For strided, we use block_size to determine number of shards
            let num_shards = n.div_ceil(computed_block_size);
            create_strided_shards(n, num_shards)
        }
    };

    Ok(ShardDescriptor {
        n,
        strategy,
        block_size: computed_block_size,
        shards,
    })
}

/// Convenience function with default autotuning config
///
/// Uses a single worker by default. For proper autotuning, use the full
/// `shards()` function with an `AutotuneConfig`.
pub fn shards_simple(
    n: usize,
    block_size: BlockSize,
    strategy: ShardStrategy,
) -> Result<ShardDescriptor, ShardError> {
    // For simple API, use explicit block size or default to n (single shard)
    let config = AutotuneConfig::with_workers(1);
    let effective_block_size = match block_size {
        BlockSize::Auto => BlockSize::Explicit(n), // Single shard if no workers specified
        other => other,
    };
    shards(n, effective_block_size, strategy, &config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_contiguous_simple() {
        let config = AutotuneConfig::with_workers(1);
        let desc = shards(10, BlockSize::Explicit(3), ShardStrategy::Contiguous, &config).unwrap();

        assert_eq!(desc.num_shards(), 4);
        assert!(desc.verify_coverage());

        // Check ranges
        let shards: Vec<_> = desc.shards.clone();
        assert_eq!(shards[0], Shard::Range { start: 0, end: 3 });
        assert_eq!(shards[1], Shard::Range { start: 3, end: 6 });
        assert_eq!(shards[2], Shard::Range { start: 6, end: 9 });
        assert_eq!(shards[3], Shard::Range { start: 9, end: 10 });
    }

    #[test]
    fn test_strided_simple() {
        let config = AutotuneConfig::with_workers(1);
        let desc = shards(10, BlockSize::Explicit(3), ShardStrategy::Strided, &config).unwrap();

        assert!(desc.verify_coverage());

        // With block_size=3, we get ceil(10/3)=4 shards
        assert_eq!(desc.num_shards(), 4);

        // Verify strided pattern: shard 0 gets [0,4,8], shard 1 gets [1,5,9], etc.
        let s0: Vec<_> = desc.shards[0].indices().collect();
        let s1: Vec<_> = desc.shards[1].indices().collect();
        let s2: Vec<_> = desc.shards[2].indices().collect();
        let s3: Vec<_> = desc.shards[3].indices().collect();

        assert_eq!(s0, vec![0, 4, 8]);
        assert_eq!(s1, vec![1, 5, 9]);
        assert_eq!(s2, vec![2, 6]);
        assert_eq!(s3, vec![3, 7]);
    }

    #[test]
    fn test_autotuning() {
        // 8 workers, default settings (4-64 shards per worker)
        let config = AutotuneConfig::with_workers(8);
        let desc = shards(1000, BlockSize::Auto, ShardStrategy::Contiguous, &config).unwrap();

        // Should have between 32 (8*4) and 512 (8*64) shards
        let num_shards = desc.num_shards();
        assert!(num_shards >= 32, "Too few shards: {}", num_shards);
        assert!(num_shards <= 512, "Too many shards: {}", num_shards);
        assert!(desc.verify_coverage());
    }

    #[test]
    fn test_autotuning_with_memory() {
        let config = AutotuneConfig {
            worker_count: 8,
            min_shards_per_worker: 4,
            max_shards_per_worker: 64,
            scratch_bytes_per_item: 1024, // 1KB per item
            scratch_budget_bytes: 1024 * 1024, // 1MB total budget
        };
        let desc = shards(10000, BlockSize::Auto, ShardStrategy::Contiguous, &config).unwrap();

        // Memory constraint: 1MB budget / 8 workers / 1KB per item = 128 items max per shard
        // So block_size should be constrained
        assert!(desc.block_size <= 128, "Block size too large for memory budget");
        assert!(desc.verify_coverage());
    }

    #[test]
    fn test_coverage_exhaustive() {
        // Test various n and block_size combinations
        for n in [1, 2, 7, 10, 100, 1000] {
            for block_size in [1, 2, 3, 7, 10, 100] {
                let config = AutotuneConfig::with_workers(1);
                let desc_contig =
                    shards(n, BlockSize::Explicit(block_size), ShardStrategy::Contiguous, &config)
                        .unwrap();
                let desc_strided =
                    shards(n, BlockSize::Explicit(block_size), ShardStrategy::Strided, &config)
                        .unwrap();

                assert!(
                    desc_contig.verify_coverage(),
                    "Contiguous coverage failed for n={}, block_size={}",
                    n,
                    block_size
                );
                assert!(
                    desc_strided.verify_coverage(),
                    "Strided coverage failed for n={}, block_size={}",
                    n,
                    block_size
                );
            }
        }
    }

    #[test]
    fn test_shard_to_vec() {
        let shard = Shard::Range { start: 5, end: 10 };
        assert_eq!(shard.to_vec(), vec![5, 6, 7, 8, 9]);
        assert_eq!(shard.to_vec_1based(), vec![6, 7, 8, 9, 10]);

        let shard = Shard::Strided {
            start: 0,
            stride: 3,
            count: 4,
        };
        assert_eq!(shard.to_vec(), vec![0, 3, 6, 9]);
        assert_eq!(shard.to_vec_1based(), vec![1, 4, 7, 10]);
    }

    #[test]
    fn test_error_cases() {
        let config = AutotuneConfig::with_workers(1);

        // n = 0
        assert!(matches!(
            shards(0, BlockSize::Explicit(1), ShardStrategy::Contiguous, &config),
            Err(ShardError::InvalidN(0))
        ));

        // block_size = 0
        assert!(matches!(
            shards(10, BlockSize::Explicit(0), ShardStrategy::Contiguous, &config),
            Err(ShardError::InvalidBlockSize(0))
        ));

        // Auto with 0 workers
        let zero_config = AutotuneConfig::with_workers(0);
        assert!(matches!(
            shards(10, BlockSize::Auto, ShardStrategy::Contiguous, &zero_config),
            Err(ShardError::NoWorkers)
        ));
    }

    #[test]
    fn test_single_item() {
        let config = AutotuneConfig::with_workers(1);
        let desc = shards(1, BlockSize::Explicit(1), ShardStrategy::Contiguous, &config).unwrap();

        assert_eq!(desc.num_shards(), 1);
        assert_eq!(desc.shards[0].len(), 1);
        assert!(desc.verify_coverage());
    }

    #[test]
    fn test_large_block_size() {
        // Block size larger than n
        let config = AutotuneConfig::with_workers(1);
        let desc =
            shards(10, BlockSize::Explicit(100), ShardStrategy::Contiguous, &config).unwrap();

        assert_eq!(desc.num_shards(), 1);
        assert_eq!(desc.shards[0].len(), 10);
        assert!(desc.verify_coverage());
    }

    #[test]
    fn test_iterator_len() {
        let shard = Shard::Range { start: 0, end: 100 };
        let indices = shard.indices();
        assert_eq!(indices.len(), 100);

        let shard = Shard::Strided {
            start: 0,
            stride: 5,
            count: 20,
        };
        let indices = shard.indices();
        assert_eq!(indices.len(), 20);
    }
}
