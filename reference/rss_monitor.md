# Monitor RSS Over Time

Creates a closure that records RSS samples with timestamps.

## Usage

``` r
rss_monitor(pid)
```

## Arguments

- pid:

  Process ID to monitor.

## Value

A list with [`sample()`](https://rdrr.io/r/base/sample.html) and
[`history()`](https://rdrr.io/r/utils/savehistory.html) functions.
