# Create a gather (indexed) view over a shared matrix

Gather views describe non-contiguous column (or row) subsets without
allocating a slice-sized matrix. shard-aware kernels can then choose to
pack the requested indices into scratch explicitly (bounded and
reportable) or run gather-aware compute paths.

## Usage

``` r
view_gather(x, rows = NULL, cols)
```

## Arguments

- x:

  A shared (share()d) atomic matrix (double/integer/logical/raw).

- rows:

  Row selector. NULL (all rows) or idx_range().

- cols:

  Integer vector of column indices (1-based).

## Value

A view object of class `shard_view_gather`.

## Details

v1 note: only column-gather views are implemented (rows may be NULL or
idx_range()).
