# Create a contiguous block view

Create a contiguous block view

## Usage

``` r
view_block(x, rows = NULL, cols = NULL)
```

## Arguments

- x:

  A shared (share()d) atomic matrix.

- rows:

  NULL or idx_range().

- cols:

  NULL or idx_range().

## Value

A `shard_view_block` object representing the contiguous block slice.

## Examples

``` r
# \donttest{
m <- share(matrix(1:20, nrow = 4))
v <- view_block(m, cols = idx_range(1, 2))
# }
```
