# Copy-on-Write Policy Report

Generates a report of copy-on-write behavior for borrowed inputs.

## Usage

``` r
cow_report(result = NULL)
```

## Arguments

- result:

  Optional. A `shard_result` object to extract COW stats from.

## Value

An S3 object of class `shard_report` with type `"cow"` containing:

- `type`: "cow"

- `timestamp`: When the report was generated

- `policy`: The COW policy used ("deny", "audit", "allow")

- `violations`: Count of COW violations detected (audit mode)

- `copies_triggered`: Estimated copies triggered by mutations

## Examples

``` r
# \donttest{
res <- shard_map(shards(100, workers = 2), function(s) sum(s$idx), workers = 2)
pool_stop()
cow_report(res)
#> shard copy-on-write report
#> Generated: 2026-03-30 18:13:59 
#> 
#> Policy: deny 
#> Violations: 0 
#> Copies triggered: 0 
# }
```
