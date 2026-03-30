# Introspection for a view

Returns metadata about a view without forcing materialization.

## Usage

``` r
view_info(v)
```

## Arguments

- v:

  A shard view.

## Value

A named list with fields: `dtype`, `dim`, `slice_dim`, `rows`, `cols`,
`layout`, `fast_path`, `nbytes_est`, and `base_is_shared`.

## Examples

``` r
# \donttest{
m <- share(matrix(1:20, nrow = 4))
v <- view_block(m, cols = idx_range(1, 2))
view_info(v)
#> $dtype
#> [1] "integer"
#> 
#> $dim
#> [1] 4 5
#> 
#> $slice_dim
#> [1] 4 2
#> 
#> $rows
#> NULL
#> 
#> $cols
#> idx_range [1:2] 
#> 
#> $rows_print
#> [1] "(all)"
#> 
#> $cols_print
#> [1] "[1:2]"
#> 
#> $layout
#> [1] "col_block_contiguous"
#> 
#> $fast_path
#> [1] FALSE
#> 
#> $nbytes_est
#> [1] 32
#> 
#> $base_is_shared
#> [1] TRUE
#> 
# }
```
