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

A `shard_schema` object.

## Examples

``` r
s <- schema(x = float64(), y = int32(), label = string_col())
s
#> $columns
#> $columns$x
#> $kind
#> [1] "float64"
#> 
#> attr(,"class")
#> [1] "shard_coltype"
#> 
#> $columns$y
#> $kind
#> [1] "int32"
#> 
#> attr(,"class")
#> [1] "shard_coltype"
#> 
#> $columns$label
#> $kind
#> [1] "string"
#> 
#> attr(,"class")
#> [1] "shard_coltype"
#> 
#> 
#> attr(,"class")
#> [1] "shard_schema"
```
