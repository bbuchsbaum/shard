# Materialize a shard table handle as a data.frame/tibble

Materialize a shard table handle as a data.frame/tibble

## Usage

``` r
as_tibble(x, max_bytes = 256 * 1024^2, ...)
```

## Arguments

- x:

  A shard table object.

- max_bytes:

  Warn if estimated payload exceeds this threshold.

- ...:

  Reserved for future extensions.

## Value

A data.frame (or tibble if the `tibble` package is installed).

## Examples

``` r
# \donttest{
s <- schema(x = float64(), y = int32())
tb <- table_buffer(s, nrow = 5L)
table_write(tb, idx_range(1, 5), data.frame(x = rnorm(5), y = 1:5))
df <- as_tibble(tb)
# }
```
