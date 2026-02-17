# Generate Shard Runtime Report

Primary entry point for shard diagnostics. Generates a comprehensive
report of the current runtime state including pool status, memory usage,
and execution statistics.

## Usage

``` r
report(level = c("summary", "workers", "tasks", "segments"), result = NULL)
```

## Arguments

- level:

  Character. Detail level for the report:

  - `"summary"`: High-level overview (default)

  - `"workers"`: Include per-worker details

  - `"tasks"`: Include task execution history

  - `"segments"`: Include shared memory segment details

- result:

  Optional. A `shard_result` object from
  [`shard_map`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  to include execution diagnostics from.

## Value

An S3 object of class `shard_report` containing:

- `level`: The requested detail level

- `timestamp`: When the report was generated

- `pool`: Pool status information (if pool exists)

- `memory`: Memory usage summary

- `workers`: Per-worker details (if level includes workers)

- `tasks`: Task execution details (if level includes tasks)

- `segments`: Segment details (if level includes segments)

- `result_diagnostics`: Diagnostics from shard_result (if provided)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic summary
report()

# Include worker details
report("workers")

# Full report with task and segment details
report("segments")

# Include diagnostics from a shard_map result
result <- shard_map(shards(100), function(s) sum(s$idx))
report("tasks", result = result)
} # }
```
