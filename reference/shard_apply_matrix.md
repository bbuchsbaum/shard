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

## Examples

``` r
# \donttest{
X <- matrix(rnorm(400), 20, 20)
shard_apply_matrix(X, MARGIN = 2, FUN = mean)
#>  [1]  0.01543774 -0.19104418  0.12401901 -0.19082450 -0.22187576 -0.06540110
#>  [7]  0.03941378 -0.28938217  0.01727253  0.29918314 -0.49148688  0.02911144
#> [13]  0.09123634 -0.26772574  0.27751757 -0.59333675 -0.10850564 -0.20181439
#> [19] -0.07470518  0.10584459
pool_stop()
# }
```
