# Create Shards from an Explicit Index List

Constructs a `shard_descriptor` from a user-supplied list of index
vectors. This is useful for non-contiguous workloads like
searchlights/feature sets where each shard operates on an arbitrary
subset.

## Usage

``` r
shards_list(idxs)
```

## Arguments

- idxs:

  List of integer vectors (1-based indices). Each element becomes one
  shard with fields `id`, `idx`, and `len`.

## Value

A `shard_descriptor`.
