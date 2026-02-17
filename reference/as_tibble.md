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
