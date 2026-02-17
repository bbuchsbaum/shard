# Task Execution Report

Generates a report of task/chunk execution statistics from a shard_map
result.

## Usage

``` r
task_report(result = NULL)
```

## Arguments

- result:

  A `shard_result` object from
  [`shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).

## Value

An S3 object of class `shard_report` with type `"task"` containing:

- `type`: "task"

- `timestamp`: When the report was generated

- `shards_total`: Total number of shards

- `shards_processed`: Number of shards successfully processed

- `shards_failed`: Number of permanently failed shards

- `chunks_dispatched`: Number of chunk batches dispatched

- `total_retries`: Total number of retry attempts

- `duration`: Total execution duration (seconds)

- `throughput`: Shards processed per second

- `queue_status`: Final queue status

## Examples

``` r
if (FALSE) { # \dontrun{
result <- shard_map(shards(100), function(s) sum(s$idx))
task_report(result)
} # }
```
