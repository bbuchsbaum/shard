# Parallel crossprod() using shard views + output buffers

Computes `crossprod(X, Y)` (i.e. `t(X) %*% Y`) using:

- shared/mmap-backed inputs (one copy),

- block views (no slice materialization),

- BLAS-3 dgemm in each tile,

- an explicit shared output buffer (no gather/bind spikes).

## Usage

``` r
shard_crossprod(
  X,
  Y,
  workers = NULL,
  block_x = "auto",
  block_y = "auto",
  backing = c("mmap", "shm"),
  materialize = c("auto", "never", "always"),
  materialize_max_bytes = 512 * 1024^2,
  diagnostics = TRUE
)
```

## Arguments

- X, Y:

  Double matrices with the same number of rows.

- workers:

  Number of worker processes.

- block_x, block_y:

  Tile sizes over `ncol(X)` and `ncol(Y)`. Use `"auto"` (default) to
  autotune on the current machine.

- backing:

  Backing for shared inputs and output buffer (`"mmap"` or `"shm"`).

- materialize:

  Whether to return the result as a standard R matrix: `"never"` (return
  buffer handle), `"always"`, or `"auto"` (materialize if estimated
  output size is below `materialize_max_bytes`).

- materialize_max_bytes:

  Threshold for `"auto"` materialization.

- diagnostics:

  Whether to collect shard_map diagnostics.

## Value

A list with:

- `buffer`: shard_buffer for the result (p x v)

- `value`: materialized matrix if requested, otherwise NULL

- `run`: the underlying shard_result from shard_map

- `tile`: chosen tile sizes

## Details

This is intended as an ergonomic entry point for the "wow" path: users
shouldn't have to manually call
[`share()`](https://bbuchsbaum.github.io/shard/reference/share.md),
[`view_block()`](https://bbuchsbaum.github.io/shard/reference/view_block.md),
[`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md),
`tiles2d()`, and
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
for common patterns.
