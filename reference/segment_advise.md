# Advise OS about expected access pattern for a segment

This calls `madvise()` on the segment mapping when available.

## Usage

``` r
segment_advise(
  seg,
  advice = c("normal", "sequential", "random", "willneed", "dontneed")
)
```

## Arguments

- seg:

  A shard_segment.

- advice:

  One of "normal", "sequential", "random", "willneed", "dontneed".

## Value

A logical scalar; `TRUE` if the OS accepted the hint.

## Examples

``` r
# \donttest{
seg <- segment_create(1024)
segment_advise(seg, "sequential")
#> [1] TRUE
# }
```
