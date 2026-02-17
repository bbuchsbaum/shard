# Fetch Data from a Shared Object

Retrieves the R object from shared memory by deserializing it. This is
the primary way to access shared data in workers.

## Usage

``` r
fetch(x, ...)
```

## Arguments

- x:

  A `shard_shared` object.

- ...:

  Ignored.

## Value

The original R object that was shared.

## Details

When called in the main process, this reads from the existing segment.
When called in a worker process, this opens the segment by path and
deserializes the data.

The `fetch()` function is the primary way to access shared data. It can
also be called as
[`materialize()`](https://bbuchsbaum.github.io/shard/reference/materialize.md)
for compatibility.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- 1:1000
shared <- share(x)
recovered <- fetch(shared)
identical(x, recovered)  # TRUE
} # }
```
