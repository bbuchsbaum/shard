# Set dimnames on a Shared Vector

Raises an error if the copy-on-write policy is `"deny"`.

## Usage

``` r
# S3 method for class 'shard_shared_vector'
dimnames(x) <- value
```

## Arguments

- x:

  A `shard_shared_vector`.

- value:

  List of dimnames.

## Value

The modified object `x`.
