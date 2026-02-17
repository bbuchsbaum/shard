# Shard Descriptor Creation

Create shard descriptors for parallel execution with autotuning.

Produces shard descriptors (index ranges) for use with
[`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).
Supports autotuning based on worker count and memory constraints.

## Usage

``` r
shards(
  n,
  block_size = "auto",
  workers = NULL,
  strategy = c("contiguous", "strided"),
  min_shards_per_worker = 4L,
  max_shards_per_worker = 64L,
  scratch_bytes_per_item = 0,
  scratch_budget = 0
)
```

## Arguments

- n:

  Integer. Total number of items to shard.

- block_size:

  Block size specification. Can be:

  - `"auto"` (default): Autotune based on worker count

  - Integer: Explicit number of items per shard

  - Character: Human-readable like `"1K"`, `"10K"`

- workers:

  Integer. Number of workers for autotuning (default: pool size or
  detectCores - 1).

- strategy:

  Sharding strategy: `"contiguous"` (default) or `"strided"`.

- min_shards_per_worker:

  Integer. Minimum shards per worker for load balancing (default 4).

- max_shards_per_worker:

  Integer. Maximum shards per worker to limit overhead (default 64).

- scratch_bytes_per_item:

  Numeric. Expected scratch memory per item for memory budgeting.

- scratch_budget:

  Character or numeric. Total scratch memory budget (e.g., "1GB").

## Value

A `shard_descriptor` object containing:

- `n`: Total items

- `block_size`: Computed block size

- `strategy`: Strategy used

- `shards`: List of shard descriptors with `id`, `start`, `end`, `idx`
  fields

## Examples

``` r
# Autotune for 8 workers
blocks <- shards(1e6, workers = 8)
length(blocks$shards)  # Number of shards
#> [1] 32

# Explicit block size
blocks <- shards(1000, block_size = 100)

# Access shard indices
blocks$shards[[1]]$idx  # Indices for first shard
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100
```
