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

  A shard table handle.

- ...:

  Passed to
  [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md).
