# Print a shard_report Object

Print a shard_report Object

## Usage

``` r
# S3 method for class 'shard_report'
print(x, ...)
```

## Arguments

- x:

  A `shard_report` object.

- ...:

  Ignored.

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
res <- shard_map(shards(100, workers = 2), function(s) sum(s$idx), workers = 2)
pool_stop()
rpt <- report(result = res)
print(rpt)
#> shard_report (summary)
#> Generated: 2026-03-30 15:57:21 
#> 
#> Pool: (not active)
#> 
#> Memory:
#>   (no pool active)
# }
```
