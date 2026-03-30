# Performance Recommendations

Uses run telemetry (copy/materialization stats, packing volume,
buffer/table writes, scratch pool stats) to produce actionable
recommendations.

## Usage

``` r
recommendations(result)
```

## Arguments

- result:

  A `shard_result` from
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).

## Value

A character vector of recommendations (possibly empty).

## Examples

``` r
# \donttest{
res <- shard_map(shards(100, workers = 2), function(s) sum(s$idx), workers = 2)
pool_stop()
recommendations(res)
#> character(0)
# }
```
