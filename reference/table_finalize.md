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

## Value

A `shard_table_handle`, `shard_row_groups`, or materialized
data.frame/tibble depending on `target` type and `materialize`.

## Details

For a `shard_table_sink`, this returns a row-group handle referencing
the written partitions (or materializes them if requested).

## Examples

``` r
# \donttest{
s <- schema(x = float64(), y = int32())
tb <- table_buffer(s, nrow = 5L)
table_write(tb, idx_range(1, 5), data.frame(x = rnorm(5), y = 1:5))
handle <- table_finalize(tb)
# }
```
