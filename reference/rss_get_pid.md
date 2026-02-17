# Get RSS for a Process ID

Retrieves the Resident Set Size (RSS) for a process using the best
available method for the current platform.

## Usage

``` r
rss_get_pid(pid)
```

## Arguments

- pid:

  Integer. Process ID to query.

## Value

Numeric. RSS in bytes, or NA if unavailable.
