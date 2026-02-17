# Allocate a fixed-row table buffer

Allocates a columnar table output: one typed buffer per column, each of
length `nrow`. Intended for lock-free disjoint row-range writes in
shard_map.

## Usage

``` r
table_buffer(schema, nrow, backing = c("auto", "mmap", "shm"))
```

## Arguments

- schema:

  A `shard_schema`.

- nrow:

  Total number of rows in the final table.

- backing:

  Backing type for buffers (`"auto"`, `"mmap"`, `"shm"`).

## Value

A `shard_table_buffer`.
