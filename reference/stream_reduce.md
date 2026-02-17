# Stream over row-groups/datasets and reduce

Applies `f()` to each partition (row-group) and combines results with
`combine()` into a single accumulator. This keeps peak memory bounded by
the largest single partition (plus your accumulator).

## Usage

``` r
stream_reduce(x, f, init, combine, ...)
```

## Arguments

- x:

  A `shard_row_groups` or `shard_dataset` handle.

- f:

  Function `(chunk, ...) -> value` producing a per-partition value.

- init:

  Initial accumulator value.

- combine:

  Function `(acc, value) -> acc` to update the accumulator.

- ...:

  Passed to `f()`.

## Value

The final accumulator.
