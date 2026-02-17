# Contiguous index range

Creates a compact, serializable range descriptor for contiguous indices.
This avoids allocating an explicit index vector for large slices.

## Usage

``` r
idx_range(start, end)
```

## Arguments

- start:

  Integer. Start index (1-based, inclusive).

- end:

  Integer. End index (1-based, inclusive).

## Value

An object of class `shard_idx_range`.
