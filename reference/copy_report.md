# Data Copy Report

Generates a report of data transfer and copy statistics during parallel
execution.

## Usage

``` r
copy_report(result = NULL)
```

## Arguments

- result:

  Optional. A `shard_result` object to extract copy stats from.

## Value

An S3 object of class `shard_report` with type `"copy"` containing:

- `type`: "copy"

- `timestamp`: When the report was generated

- `borrow_exports`: Number of borrowed input exports

- `borrow_bytes`: Total bytes in borrowed inputs

- `result_imports`: Number of result imports

- `result_bytes`: Estimated bytes in results

- `buffer_writes`: Number of buffer write operations

- `buffer_bytes`: Total bytes written to buffers

## Examples

``` r
# \donttest{
res <- shard_map(shards(100, workers = 2), function(s) sum(s$idx), workers = 2)
pool_stop()
copy_report(res)
#> shard data copy report
#> Generated: 2026-03-30 18:13:56 
#> 
#> Borrowed inputs:
#>   Exports: 0 
#>   Bytes: 0 B 
#> 
#> Results:
#>   Imports: 8 
#>   Bytes: 1.6 KB 
#> 
#> Buffers:
#>   Writes: 0 
#>   Bytes: 0 B 
#> 
#> Tables:
#>   Writes: 0 
#>   Rows: 0 
#>   Bytes: 0 B 
#> 
#> Views:
#>   Created: 0 
#>   Materialized: 0 
#>   Materialized bytes: 0 B 
#>   Packed: 0 
#>   Packed bytes: 0 B 
# }
```
