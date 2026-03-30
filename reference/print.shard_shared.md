# Print a Shared Object

Print a Shared Object

## Usage

``` r
# S3 method for class 'shard_shared'
print(x, ...)
```

## Arguments

- x:

  A `shard_shared` object.

- ...:

  Ignored.

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
shared <- share(1:10)
print(shared)
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"shard_cow")
#> [1] "deny"
#> attr(,"shard_readonly")
#> [1] TRUE
close(shared)
# }
```
