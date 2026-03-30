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

## Value

A double matrix of dimensions `nrow` by `ncol`.

## Examples

``` r
m <- scratch_matrix(10, 5)
dim(m)
#> [1] 10  5
```
