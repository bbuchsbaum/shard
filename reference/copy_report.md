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
if (FALSE) { # \dontrun{
X <- matrix(rnorm(1e4), 100, 100)
result <- shard_map(shards(10), function(s, X) sum(X[, s$idx]),
                    borrow = list(X = X))
copy_report(result)
} # }
```
