# Finalize a table buffer

Finalize a table buffer

## Usage

``` r
# S3 method for class 'shard_table_buffer'
table_finalize(
  target,
  materialize = c("never", "auto", "always"),
  max_bytes = 256 * 1024^2,
  ...
)
```

## Arguments

- target:

  A `shard_table_buffer`.

- materialize:

  `"never"`, `"auto"`, or `"always"`.

- max_bytes:

  For `"auto"`, materialize only if estimated bytes \<= max_bytes.

- ...:

  Reserved for future extensions.

## Value

A `shard_table_handle` or a materialized data.frame/tibble.
