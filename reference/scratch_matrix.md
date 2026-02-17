# Get a scratch matrix

Allocates (or reuses) a double matrix in the worker scratch pool.

## Usage

``` r
scratch_matrix(nrow, ncol, key = NULL)
```

## Arguments

- nrow, ncol:

  Dimensions.

- key:

  Optional key to control reuse. Defaults to a shape-derived key.
