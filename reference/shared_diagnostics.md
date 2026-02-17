# Get diagnostics for a shared vector

Get diagnostics for a shared vector

## Usage

``` r
shared_diagnostics(x)
```

## Arguments

- x:

  A shard ALTREP vector

## Value

A list with diagnostic information:

- dataptr_calls:

  Number of times DATAPTR was accessed

- materialize_calls:

  Number of times vector was copied to standard R vector

- length:

  Number of elements

- offset:

  Byte offset into underlying segment

- readonly:

  Whether write access is prevented

- type:

  R type of the vector

## Examples

``` r
if (FALSE) { # \dontrun{
seg <- segment_create(400)
segment_write(seg, 1:100, offset = 0)
x <- shared_vector(seg, "integer", length = 100)

# Access data
sum(x)

# Check diagnostics
shared_diagnostics(x)
} # }
```
