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
#>  [1] -0.001318534 -0.137691473  0.167734132 -0.313173042 -0.077425565
#>  [6] -0.181145755 -0.051592701 -0.186890507  0.231881906 -0.135645149
#> [11] -0.037327701  0.029086775 -0.045347449 -0.308523295 -0.014213638
#> [16] -0.347121746 -0.167135456 -0.280361143  0.138963790  0.282064603
pool_stop()
# }
```
