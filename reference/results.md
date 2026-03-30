# Extract Results from shard_map

Extract Results from shard_map

## Usage

``` r
results(x, flatten = TRUE)
```

## Arguments

- x:

  A shard_result object.

- flatten:

  Logical. Flatten nested results?

## Value

List or vector of results.

## Examples

``` r
# \donttest{
result <- shard_map(4L, function(shard) shard$idx[[1L]], workers = 2)
pool_stop()
results(result)
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 2
#> 
#> $`3`
#> [1] 3
#> 
#> $`4`
#> [1] 4
#> 
# }
```
