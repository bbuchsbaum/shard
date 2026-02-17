# Shared Memory Segment Report

Generates a report of active shared memory segments in the current
session.

## Usage

``` r
segment_report()
```

## Value

An S3 object of class `shard_report` with type `"segment"` containing:

- `type`: "segment"

- `timestamp`: When the report was generated

- `n_segments`: Number of tracked segments

- `total_bytes`: Total bytes across all segments

- `segments`: List of segment details

## Details

This function reports on segments that are currently accessible. Note
that segments are automatically cleaned up when their R objects are
garbage collected, so this only shows segments with live references.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create some segments
seg1 <- segment_create(1024)
seg2 <- segment_create(2048)
shared <- share(1:1000)

segment_report()
} # }
```
