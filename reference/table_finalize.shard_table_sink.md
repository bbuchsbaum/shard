# Finalize a sink

Finalize a sink

## Usage

``` r
# S3 method for class 'shard_table_sink'
table_finalize(
  target,
  materialize = c("never", "auto", "always"),
  max_bytes = 256 * 1024^2,
  ...
)
```

## Arguments

- target:

  A `shard_table_sink`.

- materialize:

  `"never"`, `"auto"`, or `"always"`.

- max_bytes:

  For `"auto"`, materialize only if estimated bytes \<= max_bytes.

- ...:

  Reserved for future extensions.

## Value

A `shard_row_groups` handle (or a materialized data.frame/tibble).
