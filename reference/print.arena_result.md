# Print an arena_result object

Print an arena_result object

## Usage

``` r
# S3 method for class 'arena_result'
print(x, ...)
```

## Arguments

- x:

  An `arena_result` object.

- ...:

  Additional arguments passed to `print`.

## Value

Returns `x` invisibly.

## Examples

``` r
# \donttest{
info <- arena({ sum(1:10) }, diagnostics = TRUE)
print(info)
#> <arena_result>
#>   Arena ID: arena_1_11171313.013 
#>   Strict mode: FALSE 
#>   RSS before: 199,761,920 bytes
#>   RSS after: 199,761,920 bytes
#>   RSS delta: 0 bytes
#>   Result size: 56 bytes
#> 
#> Result:
#> [1] 55
# }
```
