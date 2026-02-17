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

Logical; TRUE if the OS accepted the hint.
