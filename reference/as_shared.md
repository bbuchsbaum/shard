# Create a shared vector from an existing R vector

Convenience function that creates a segment, writes the data, and
returns an ALTREP view.

## Usage

``` r
as_shared(x, readonly = TRUE, backing = "auto", cow = NULL)
```

## Arguments

- x:

  An atomic vector (integer, double, logical, or raw)

- readonly:

  If TRUE, prevent write access (default: TRUE)

- backing:

  Backing type for the segment: "auto", "mmap", or "shm"

- cow:

  Copy-on-write policy for the resulting shared vector. One of `"deny"`,
  `"audit"`, or `"allow"`. If NULL, defaults based on `readonly`.

## Value

An ALTREP vector backed by shared memory

## Examples

``` r
if (FALSE) { # \dontrun{
# Convert existing vector to shared
x <- as_shared(1:100)
is_shared_vector(x)  # TRUE

# Subsetting returns views
y <- x[1:10]
is_shared_vector(y)  # TRUE for contiguous subsets
} # }
```
