# Advise access pattern for a shared input vector/matrix

Advise access pattern for a shared input vector/matrix

## Usage

``` r
shared_advise(
  x,
  advice = c("normal", "sequential", "random", "willneed", "dontneed")
)
```

## Arguments

- x:

  A shard shared vector (from
  [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md)).

- advice:

  See
  [`segment_advise()`](https://bbuchsbaum.github.io/shard/reference/segment_advise.md).

## Value

A logical scalar; `TRUE` if the OS accepted the hint.

## Examples

``` r
# \donttest{
x <- as_shared(1:100)
shared_advise(x, "sequential")
#> [1] TRUE
# }
```
