# Set dim on a Shared Vector

Raises an error if the copy-on-write policy is `"deny"`.

## Usage

``` r
# S3 method for class 'shard_shared_vector'
dim(x) <- value
```

## Arguments

- x:

  A `shard_shared_vector`.

- value:

  Integer vector of dimensions.

## Value

The modified object `x`.
