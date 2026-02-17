# Define a table schema

A schema is a named set of columns with explicit types. It is used to
allocate table buffers and validate writes.

## Usage

``` r
schema(...)
```

## Arguments

- ...:

  Named columns with type specs (e.g.,
  [`int32()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md),
  [`float64()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)).

## Value

A `shard_schema`.
