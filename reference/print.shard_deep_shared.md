# Print a Deep-Shared Object

Print a Deep-Shared Object

## Usage

``` r
# S3 method for class 'shard_deep_shared'
print(x, ...)
```

## Arguments

- x:

  A `shard_deep_shared` object.

- ...:

  Ignored.

## Value

The input `x`, invisibly.

## Examples

``` r
# \donttest{
lst <- list(a = 1:10, b = 11:20)
shared <- share(lst, deep = TRUE, min_bytes = 1)
print(shared)
#> <shard_deep_shared>
#>   Type: list 
#>   Backing: auto 
#>   Read-only: TRUE 
#> 
#>   Summary:
#>     Shared segments: 2 
#>     Aliased references: 0 
#>     Kept in-place: 0 
#>     Total shared bytes: 80 
close(shared)
# }
```
