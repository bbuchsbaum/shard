# Create a view (subset) of a shared vector

Create a view (subset) of a shared vector

## Usage

``` r
shared_view(x, start, length)
```

## Arguments

- x:

  A shard ALTREP vector

- start:

  Start index (1-based, like R)

- length:

  Number of elements

## Value

An ALTREP view into the same shared memory

## Examples

``` r
if (FALSE) { # \dontrun{
seg <- segment_create(800)
segment_write(seg, 1:100, offset = 0)
x <- shared_vector(seg, "integer", length = 100)

# Create a view of elements 10-20
y <- shared_view(x, start = 10, length = 11)
y[1]  # Same as x[10]
} # }
```
