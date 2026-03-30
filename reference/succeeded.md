# Check if shard_map Succeeded

Check if shard_map Succeeded

## Usage

``` r
succeeded(x)
```

## Arguments

- x:

  A shard_result object.

## Value

Logical. TRUE if no failures.

## Examples

``` r
# \donttest{
result <- shard_map(4L, function(shard) shard$idx[[1L]], workers = 2)
pool_stop()
succeeded(result)
#> [1] TRUE
# }
```
