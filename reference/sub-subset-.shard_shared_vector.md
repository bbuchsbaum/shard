# Double-bracket Subset-assign a Shared Vector

Replacement method for `shard_shared_vector`. Raises an error if the
copy-on-write policy is `"deny"`.

## Usage

``` r
# S3 method for class 'shard_shared_vector'
x[[...]] <- value
```

## Arguments

- x:

  A `shard_shared_vector`.

- ...:

  Indices.

- value:

  Replacement value.

## Value

The modified object `x`.
