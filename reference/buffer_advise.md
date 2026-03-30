# Advise access pattern for a buffer

Advise access pattern for a buffer

## Usage

``` r
buffer_advise(
  x,
  advice = c("normal", "sequential", "random", "willneed", "dontneed")
)
```

## Arguments

- x:

  A shard_buffer.

- advice:

  See
  [`segment_advise()`](https://bbuchsbaum.github.io/shard/reference/segment_advise.md).

## Value

A logical scalar; `TRUE` if the OS accepted the hint.

## Examples

``` r
# \donttest{
buf <- buffer("double", dim = 10L)
buffer_advise(buf, "sequential")
#> [1] TRUE
# }
```
