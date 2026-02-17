# Finalize a table buffer or sink

For a `shard_table_buffer`, this returns a lightweight in-memory handle
(or a materialized data.frame/tibble, depending on `materialize`).

## Usage

``` r
table_finalize(
  target,
  materialize = c("never", "auto", "always"),
  max_bytes = 256 * 1024^2,
  ...
)
```

## Arguments

- target:

  A `shard_table_buffer` or `shard_table_sink`.

- materialize:

  `"never"`, `"auto"`, or `"always"`.

- max_bytes:

  For `"auto"`, materialize only if estimated bytes \<= max_bytes.

- ...:

  Reserved for future extensions.

## Details

For a `shard_table_sink`, this returns a row-group handle referencing
the written partitions (or materializes them if requested).
