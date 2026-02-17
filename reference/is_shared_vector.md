# Check if an object is a shared vector

Check if an object is a shared vector

## Usage

``` r
is_shared_vector(x)
```

## Arguments

- x:

  Any R object

## Value

TRUE if x is a shard ALTREP vector, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
seg <- segment_create(400)
segment_write(seg, 1:100, offset = 0)
x <- shared_vector(seg, "integer", length = 100)

is_shared_vector(x)    # TRUE
is_shared_vector(1:10) # FALSE
} # }
```
