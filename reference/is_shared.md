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

TRUE if x is a `shard_shared` or `shard_deep_shared` object, FALSE
otherwise.

## Examples

``` r
is_shared(share(1:10))  # TRUE
#> [1] TRUE
is_shared(1:10)         # FALSE
#> [1] FALSE
```
