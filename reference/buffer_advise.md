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

Logical.
