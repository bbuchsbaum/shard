# Reset diagnostic counters for a shared vector

Reset diagnostic counters for a shared vector

## Usage

``` r
shared_reset_diagnostics(x)
```

## Arguments

- x:

  A shard ALTREP vector

## Value

x (invisibly)

## Examples

``` r
if (FALSE) { # \dontrun{
seg <- segment_create(400)
segment_write(seg, 1:100, offset = 0)
x <- shared_vector(seg, "integer", length = 100)

sum(x)
shared_diagnostics(x)$dataptr_calls  # > 0

shared_reset_diagnostics(x)
shared_diagnostics(x)$dataptr_calls  # 0
} # }
```
