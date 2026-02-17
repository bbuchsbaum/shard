# Configure scratch pool limits

Configure scratch pool limits

## Usage

``` r
scratch_pool_config(max_bytes = Inf)
```

## Arguments

- max_bytes:

  Maximum scratch pool bytes allowed in a worker. If exceeded, the
  worker is flagged for recycle at the next safe point.
