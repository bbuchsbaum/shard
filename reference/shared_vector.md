# Create a shared vector from a segment

Create a shared vector from a segment

## Usage

``` r
shared_vector(
  segment,
  type = c("double", "integer", "logical", "raw"),
  offset = 0,
  length = NULL,
  readonly = TRUE,
  cow = NULL
)
```

## Arguments

- segment:

  A shard_segment object

- type:

  Vector type: "integer", "double"/"numeric", "logical", or "raw"

- offset:

  Byte offset into segment (default: 0)

- length:

  Number of elements. If NULL, calculated from segment size.

- readonly:

  If TRUE, prevent write access via DATAPTR (default: TRUE)

- cow:

  Copy-on-write policy for mutation attempts. One of `"deny"`,
  `"audit"`, or `"allow"`. If NULL, defaults to `"deny"` when
  `readonly=TRUE` and `"allow"` otherwise.

## Value

An ALTREP vector backed by shared memory

## Examples

``` r
# \donttest{
seg <- segment_create(400)
segment_write(seg, 1:100, offset = 0)

x <- shared_vector(seg, "integer", length = 100)
x[1:10]
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> attr(,"shard_cow")
#> [1] "deny"
#> attr(,"shard_readonly")
#> [1] TRUE

shared_diagnostics(x)
#> $dataptr_calls
#> [1] 0
#> 
#> $materialize_calls
#> [1] 0
#> 
#> $length
#> [1] 100
#> 
#> $offset
#> [1] 0
#> 
#> $readonly
#> [1] TRUE
#> 
#> $type
#> [1] "integer"
#> 
# }
```
