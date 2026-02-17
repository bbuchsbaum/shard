# shard

**Deterministic, zero-copy parallel execution for R.**

`shard` is a parallel runtime for workloads that look like: - “run the
same numeric kernel over many slices of big data” - “thousands of
independent tasks over a shared dataset” - “parallel GLM / simulation /
bootstrap / feature screening”

It focuses on three things that are often painful in R parallelism: 1)
**Shared immutable inputs** (avoid duplicating large objects across
workers) 2) **Explicit output buffers** (avoid huge result-gather lists)
3) **Deterministic cleanup** (supervise workers and recycle on memory
drift)

------------------------------------------------------------------------

## Installation

From CRAN (once released):

``` r
install.packages("shard")
```

Development version:

``` r
# install.packages("pak")
pak::pak("your-org/shard")
```

------------------------------------------------------------------------

## Core concepts

### 1) Share large inputs (read-only by default)

``` r
X <- shard::share(X)   # matrix/array/vector
Y <- shard::share(Y)
```

Shared objects are designed for zero-copy parallel reads (where the OS
allows) and are treated as immutable by default inside parallel tasks.

### 2) Allocate explicit output buffers

Instead of returning giant objects from each worker, write to a
preallocated buffer:

``` r
out <- shard::buffer("double", dim = c(1e6))   # example: 1M outputs
```

### 3) Run shard_map() over shards

``` r
blocks <- shard::shards(1e6, block_size = "auto")

run <- shard::shard_map(
  blocks,
  borrow = list(X = X, Y = Y),
  out = list(out = out),
  workers = 8,
  fun = function(block, X, Y, out) {
    # block contains indices
    idx <- block$idx
    out[idx] <- colMeans(Y[, idx, drop = FALSE])
  }
)

shard::report(run)
```

------------------------------------------------------------------------

## Safety defaults (and why they matter)

### Borrowed inputs are immutable

By default, trying to mutate borrowed/shared inputs is treated as a
bug: - `cow = "deny"` (default): mutation triggers an error -
`cow = "audit"`: detect and flag (best-effort; platform dependent) -
`cow = "allow"`: allow copy-on-write, track it, and enforce budgets

**Why default is deny:** - Prevents silent memory blowups from
accidental wide writes - Prevents subtle correctness bugs (changes are
private to a worker) - Keeps behavior predictable across platforms

------------------------------------------------------------------------

## Deterministic cleanup via worker supervision

R’s GC and allocator behavior can lead to memory drift in long-running
workers.

`shard` monitors per-worker memory usage and can recycle workers when
drift exceeds thresholds, keeping end-of-run memory close to baseline.

------------------------------------------------------------------------

## Diagnostics

After a run, `shard` can report: - total and per-worker peak RSS - end
RSS vs baseline - materialized bytes (hidden copies) - recycling events,
retries, timing

``` r
rep <- shard::report(run)
print(rep)

shard::mem_report(run)
shard::copy_report(run)
```

------------------------------------------------------------------------

## Apply-style wrappers (memshare-like ergonomics)

If your workload is basically “apply a function over columns” or “lapply
over a list”, `shard` provides thin convenience wrappers that still run
through the supervised runtime.

### Column-wise apply (scalar return)

``` r
X <- matrix(rnorm(1e6), nrow = 1000)

scores <- shard::shard_apply_matrix(
  X,
  MARGIN = 2,
  FUN = function(v, y) cor(v, y),
  VARS = list(y = rnorm(nrow(X))),
  workers = 8
)
```

### List lapply (guarded gather)

``` r
xs <- lapply(1:1000, function(i) rnorm(100))

out <- shard::shard_lapply_shared(
  xs,
  FUN = function(el) mean(el),
  workers = 8
)
```

For large outputs (big vectors/data.frames per element), prefer
[`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md),
[`table_sink()`](https://bbuchsbaum.github.io/shard/reference/table_sink.md),
or
[`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
instead of gathering everything to the master.

------------------------------------------------------------------------

## Integration with foreach (optional)

`shard` can be used directly via
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).
For compatibility with existing `foreach` code, a companion backend
package can register a `%dopar%` backend.

(See the `doShard` package in the project ecosystem.)

------------------------------------------------------------------------

## Vignettes

Vignettes are planned but not yet included in the package source.

------------------------------------------------------------------------

## License

MIT
