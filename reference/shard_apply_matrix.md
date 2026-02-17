# Apply a Function Over Matrix Columns with Shared Inputs

A convenience wrapper for the common "per-column apply" pattern. The
matrix is shared once and each worker receives a zero-copy column view
when possible.

## Usage

``` r
shard_apply_matrix(
  X,
  MARGIN = 2,
  FUN,
  VARS = NULL,
  workers = NULL,
  ...,
  policy = shard_apply_policy()
)
```

## Arguments

- X:

  A numeric/integer/logical matrix (or a shared matrix created by
  [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md)).

- MARGIN:

  Must be 2 (columns).

- FUN:

  Function of the form `function(v, ...)` returning a scalar atomic.

- VARS:

  Optional named list of extra variables. Large atomic VARS are
  auto-shared based on `policy$auto_share_min_bytes`.

- workers:

  Number of workers (passed to
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- ...:

  Additional arguments forwarded to `FUN`.

- policy:

  A
  [`shard_apply_policy()`](https://bbuchsbaum.github.io/shard/reference/shard_apply_policy.md)
  object.

## Value

An atomic vector of length `ncol(X)` with the results.

## Details

Current limitation: `MARGIN` must be 2 (columns). Row-wise apply would
require strided/gather slicing and is intentionally explicit in shard
via views/kernels.
