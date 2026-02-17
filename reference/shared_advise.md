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

Logical.
