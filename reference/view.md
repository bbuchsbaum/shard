# Create a view over a shared matrix

Create a view over a shared matrix

## Usage

``` r
view(x, rows = NULL, cols = NULL, type = c("auto", "block", "gather"))
```

## Arguments

- x:

  A shared (share()d) atomic matrix (double/integer/logical/raw).

- rows:

  Row selector. NULL (all rows) or idx_range().

- cols:

  Column selector. NULL (all cols) or idx_range().

- type:

  View type. `"block"` or `"gather"` (or `"auto"`).

## Value

A view object.
