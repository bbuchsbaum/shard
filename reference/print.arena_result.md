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
#>   Arena ID: arena_1_20452020.675 
#>   Strict mode: FALSE 
#>   RSS before: 205,422,592 bytes
#>   RSS after: 205,422,592 bytes
#>   RSS delta: 0 bytes
#>   Result size: 56 bytes
#> 
#> Result:
#> [1] 55
# }
```
