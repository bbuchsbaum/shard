# Check if Currently Inside an Arena

Returns TRUE if the current execution context is within an arena()
scope.

## Usage

``` r
in_arena()
```

## Value

Logical indicating whether we are in an arena scope.

## Examples

``` r
in_arena()
#> [1] FALSE
arena({
  in_arena()
})
#> [1] TRUE
```
