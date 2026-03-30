# Collect a shard table into memory

`collect()` is a convenience alias for
[`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md)
for shard table outputs.

## Usage

``` r
collect(x, ...)
```

## Arguments

- x:

  A shard table handle (`shard_row_groups`, `shard_dataset`, or
  `shard_table_handle`).

- ...:

  Passed to
  [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md).

## Value

A data.frame (or tibble if the `tibble` package is installed).

## Examples

``` r
# \donttest{
s <- schema(x = float64(), y = int32())
tb <- table_buffer(s, nrow = 5L)
table_write(tb, idx_range(1, 5), data.frame(x = rnorm(5), y = 1:5))
handle <- table_finalize(tb)
df <- collect(handle)
# }
```
