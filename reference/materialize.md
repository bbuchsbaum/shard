# Materialize Shared Object

Alias for
[`fetch()`](https://bbuchsbaum.github.io/shard/reference/fetch.md).
Retrieves the R object from shared memory.

## Usage

``` r
materialize(x)

# S3 method for class 'shard_shared'
materialize(x)

# Default S3 method
materialize(x)
```

## Arguments

- x:

  A `shard_shared` object.

## Value

The original R object.

## Examples

``` r
# \donttest{
shared <- share(1:100)
data <- materialize(shared)
close(shared)
# }
```
