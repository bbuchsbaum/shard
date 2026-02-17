# Autotune Block Size

Determines optimal block size based on worker count and constraints.

## Usage

``` r
autotune_block_size(
  n,
  workers,
  min_shards_per_worker = 4L,
  max_shards_per_worker = 64L,
  scratch_bytes_per_item = 0,
  scratch_budget = 0
)
```

## Arguments

- n:

  Total items.

- workers:

  Number of workers.

- min_shards_per_worker:

  Minimum shards per worker.

- max_shards_per_worker:

  Maximum shards per worker.

- scratch_bytes_per_item:

  Scratch bytes per item.

- scratch_budget:

  Total scratch budget.

## Value

Integer block size.
