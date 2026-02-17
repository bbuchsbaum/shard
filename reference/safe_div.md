# Safe Divide

Division that returns NA instead of Inf/NaN for zero divisors.

## Usage

``` r
safe_div(x, y)
```

## Arguments

- x:

  Numerator.

- y:

  Denominator.

## Value

x/y or NA if y is zero.
