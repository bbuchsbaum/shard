# Getting Started with shard

``` r
library(shard)
```

R’s parallel tools make it easy to fan out work, but they leave you to
manage the hard parts yourself: duplicated memory, runaway workers,
invisible copy-on-write. shard handles all of that so you can focus on
the computation.

The core idea is simple: **share inputs once, write outputs to a buffer,
let shard supervise the workers.**

## A first example

Suppose you have a large matrix and want to compute column means in
parallel. With shard, you share the matrix, allocate an output buffer,
and map over column indices:

``` r
set.seed(42)
X <- matrix(rnorm(5000), nrow = 100, ncol = 50)

# Share the matrix (zero-copy for workers)
X_shared <- share(X)

# Allocate an output buffer
out <- buffer("double", dim = ncol(X))

# Define column shards and run
blocks <- shards(ncol(X), workers = 2)
run <- shard_map(
  blocks,
  borrow = list(X = X_shared),
  out    = list(out = out),
  workers = 2,
  fun = function(shard, X, out) {
    for (j in shard$idx) {
      out[j] <- mean(X[, j])
    }
  }
)

# Read results from the buffer
result <- out[]
head(result)
#> [1]  0.03251482 -0.08748371 -0.01036817  0.03293646 -0.11783051  0.00236351
```

No serialization of the full matrix per worker. No list of return values
to reassemble. The workers wrote directly into `out`.

## The three core objects

shard’s workflow revolves around three things:

| Object           | Constructor                                                          | Purpose                                          |
|:-----------------|:---------------------------------------------------------------------|:-------------------------------------------------|
| Shared input     | [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md)   | Immutable, zero-copy data visible to all workers |
| Output buffer    | [`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md) | Writable shared memory that workers fill in      |
| Shard descriptor | [`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md) | Index ranges that partition the work             |

### Sharing inputs

[`share()`](https://bbuchsbaum.github.io/shard/reference/share.md)
places an R object into shared memory. Workers attach to the same
segment instead of receiving a copy:

``` r
X_shared <- share(X)
is_shared(X_shared)
#> [1] TRUE
shared_info(X_shared)
#> $path
#> [1] "/tmp/shard_10665_1774894493_1774894495"
#> 
#> $backing
#> [1] "mmap"
#> 
#> $size
#> [1] 40000
#> 
#> $readonly
#> [1] TRUE
#> 
#> $class_info
#> $class_info$type
#> [1] "matrix"
#> 
#> $class_info$dim
#> [1] 100  50
#> 
#> $class_info$mode
#> [1] "double"
#> 
#> 
#> $segment_info
#> $segment_info$size
#> [1] 40000
#> 
#> $segment_info$backing
#> [1] "mmap"
#> 
#> $segment_info$path
#> [1] "/tmp/shard_10665_1774894493_1774894495"
#> 
#> $segment_info$readonly
#> [1] TRUE
#> 
#> $segment_info$owns
#> [1] TRUE
```

Shared objects are **read-only**. Any attempt to modify them in a worker
raises an error, which prevents silent copy-on-write bugs.

### Output buffers

[`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md)
creates typed shared memory that workers write to using standard R
indexing:

``` r
buf <- buffer("double", dim = c(10, 5))
buf[1:5, 1] <- rnorm(5)
buf[6:10, 1] <- rnorm(5)
buf[, 1]
#>  [1]  0.07122244  0.97029003  0.31003525 -0.13954856 -0.32631113 -0.11880951
#>  [7]  0.89355889  0.21111451 -0.48877979 -0.22037193
```

Buffers support `"double"`, `"integer"`, `"logical"`, and `"raw"` types.
For matrices and arrays, pass a `dim` vector:

``` r
int_buf <- buffer("integer", dim = 100)
mat_buf <- buffer("double", dim = c(50, 20))
```

### Shard descriptors

[`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md)
partitions a range of indices into chunks for parallel execution. It
auto-tunes the block size based on the number of workers:

``` r
blocks <- shards(1000, workers = 4)
blocks
#> shard descriptor
#>   Items: 1,000 
#>   Block size: 63 
#>   Strategy: contiguous 
#>   Shards: 16 
#>   Shard sizes: 55 - 63
```

Each shard carries an `idx` field with its assigned indices:

``` r
blocks[[1]]$idx[1:10]  # first 10 indices of shard 1
#>  [1]  1  2  3  4  5  6  7  8  9 10
```

## Running shard_map()

[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
is the engine. It dispatches shards to a supervised worker pool, passes
shared inputs, and collects diagnostics:

``` r
set.seed(1)
X <- matrix(rnorm(2000), nrow = 100, ncol = 20)
X_shared <- share(X)
col_sds <- buffer("double", dim = ncol(X))

blocks <- shards(ncol(X), workers = 2)
run <- shard_map(
  blocks,
  borrow  = list(X = X_shared),
  out     = list(col_sds = col_sds),
  workers = 2,
  fun = function(shard, X, col_sds) {
    for (j in shard$idx) {
      col_sds[j] <- sd(X[, j])
    }
  }
)

# Results are already in the buffer
sd_values <- col_sds[]

# Verify against base R
all.equal(sd_values, apply(X, 2, sd))
#> [1] TRUE
```

### What if workers return values?

If your function returns a value (instead of writing to a buffer), shard
gathers the results:

``` r
blocks <- shards(10, workers = 2)
run <- shard_map(
  blocks,
  workers = 2,
  fun = function(shard) {
    sum(shard$idx)
  }
)
results(run)
#> $`1`
#> [1] 3
#> 
#> $`2`
#> [1] 7
#> 
#> $`3`
#> [1] 11
#> 
#> $`4`
#> [1] 15
#> 
#> $`5`
#> [1] 19
```

Buffers are preferred for large outputs because they avoid serializing
results back to the main process. Use return values for small summaries.

## Convenience wrappers

For common patterns, shard provides wrappers that handle sharing,
sharding, and buffering automatically.

### Column-wise apply

[`shard_apply_matrix()`](https://bbuchsbaum.github.io/shard/reference/shard_apply_matrix.md)
applies a scalar function over each column of a matrix:

``` r
set.seed(1)
X <- matrix(rnorm(2000), nrow = 100, ncol = 20)
y <- rnorm(100)

# Correlate each column of X with y
cors <- shard_apply_matrix(
  X,
  MARGIN = 2,
  FUN = function(v, y) cor(v, y),
  VARS = list(y = y),
  workers = 2
)
head(cors)
#> [1]  0.09122115  0.14024598  0.14152571  0.07212827  0.01476771 -0.02163338
```

The matrix is auto-shared, columns are dispatched as shards, and results
are collected into a vector.

### List lapply

[`shard_lapply_shared()`](https://bbuchsbaum.github.io/shard/reference/shard_lapply_shared.md)
is a parallel `lapply` with automatic sharing of large list elements:

``` r
chunks <- lapply(1:10, function(i) rnorm(100))

means <- shard_lapply_shared(
  chunks,
  FUN = mean,
  workers = 2
)
unlist(means)
#>  [1]  0.05500125 -0.03817609  0.11888573  0.05156112 -0.17846699  0.07432353
#>  [7]  0.07494846  0.06421283  0.07259335  0.14194300
```

## Diagnostics

Every
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
call records timing, memory, and worker statistics. Use
[`report()`](https://bbuchsbaum.github.io/shard/reference/report.md) to
inspect them:

``` r
report(result = run)
#> shard_report (summary)
#> Generated: 2026-03-30 18:14:54 
#> 
#> Pool:
#>   Workers: 2 
#>   RSS limit: 2.0 GB 
#>   Drift threshold: 50% 
#>   Stats: 32 tasks, 0 recycles, 0 deaths
#> 
#> Memory:
#>   Total RSS: 158.6 MB 
#>   Peak RSS: 79.5 MB 
#>   Mean RSS: 79.3 MB
```

For focused views:

- `mem_report(run)` – peak and baseline RSS per worker
- `copy_report(run)` – bytes transferred through buffers
- `task_report(run)` – per-chunk execution times and retry counts

## Worker pool management

By default,
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
creates a worker pool on first use and reuses it. You can also manage
the pool explicitly:

``` r
# Create a pool with 4 workers and a 1GB memory cap
pool_create(n = 4, rss_limit = "1GB")

# Check pool health
pool_status()

# Run multiple shard_map() calls (reuses the same pool)
run1 <- shard_map(shards(1000), workers = 4, fun = function(s) sum(s$idx))
run2 <- shard_map(shards(500),  workers = 4, fun = function(s) mean(s$idx))

# Shut down workers when done
pool_stop()
```

Workers are supervised: if a worker’s memory usage drifts beyond the
threshold, shard recycles it automatically.

## Copy-on-write protection

Shared inputs are immutable by default (`cow = "deny"`). This prevents a
common class of parallel bugs where a worker accidentally modifies
shared data, triggering a silent copy:

``` r
shard_map(
  shards(10),
  borrow = list(X = share(matrix(1:100, 10, 10))),
  workers = 2,
  cow = "deny",
  fun = function(shard, X) {
    X[1, 1] <- 999  # Error: mutation denied
  }
)
```

You can relax this with `cow = "audit"` (detect and report mutations) or
`cow = "allow"` (permit copy-on-write with tracking). See
[`?shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
for details.

## Clean up

When you are done, stop the pool to release worker processes:

``` r
pool_stop()
```

## Next steps

- [`?shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  – full reference for the parallel engine
- [`?share`](https://bbuchsbaum.github.io/shard/reference/share.md) –
  sharing options and backing types
- [`?buffer`](https://bbuchsbaum.github.io/shard/reference/buffer.md) –
  buffer types and matrix/array support
- [`?report`](https://bbuchsbaum.github.io/shard/reference/report.md) –
  diagnostic reports and recommendations
- [`?shard_apply_matrix`](https://bbuchsbaum.github.io/shard/reference/shard_apply_matrix.md)
  – column-wise parallel apply
- [`?pool_create`](https://bbuchsbaum.github.io/shard/reference/pool_create.md)
  – pool configuration and memory limits
