# Set an Attribute on a Shared Vector

Raises an error if the copy-on-write policy is `"deny"`.

## Usage

``` r
# S3 method for class 'shard_shared_vector'
attr(x, which) <- value
```

## Arguments

- x:

  A `shard_shared_vector`.

- which:

  Attribute name.

- value:

  Attribute value.

## Value

The modified object `x`.
