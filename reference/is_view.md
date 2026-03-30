# View Predicates

View Predicates

## Usage

``` r
is_view(x)

is_block_view(x)
```

## Arguments

- x:

  An object.

## Value

Logical. `TRUE` if `x` is a shard view (or block view).

## Examples

``` r
# \donttest{
m <- share(matrix(1:20, nrow = 4))
v <- view_block(m, cols = idx_range(1, 2))
is_view(v)
#> [1] TRUE
is_block_view(v)
#> [1] TRUE
# }
```
