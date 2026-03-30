# Check if Object is Shared

Check if Object is Shared

## Usage

``` r
is_shared(x)
```

## Arguments

- x:

  An object to check.

## Value

A logical scalar: `TRUE` if `x` is a shared object, `FALSE` otherwise.

## Examples

``` r
is_shared(1:10)
#> [1] FALSE
# \donttest{
shared <- share(1:10)
is_shared(shared)
#> [1] TRUE
close(shared)
# }
```
