# Apply Wrapper Policy

Centralizes safe defaults and guardrails for apply/lapply convenience
wrappers.

## Usage

``` r
shard_apply_policy(
  auto_share_min_bytes = "1MB",
  max_gather_bytes = "256MB",
  cow = c("deny", "audit", "allow"),
  profile = c("default", "memory", "speed"),
  block_size = "auto",
  backing = c("auto", "mmap", "shm")
)
```

## Arguments

- auto_share_min_bytes:

  Minimum object size for auto-sharing (default "1MB").

- max_gather_bytes:

  Maximum estimated gathered result bytes before refusing to run
  (default "256MB").

- cow:

  Copy-on-write policy for borrowed inputs. One of `"deny"`, `"audit"`,
  or `"allow"`. Default `"deny"`.

- profile:

  Execution profile passed through to
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md).
  One of `"default"`, `"memory"`, or `"speed"`. Default `"default"`.

- block_size:

  Shard block size for apply-style workloads. Default `"auto"`.

- backing:

  Backing type used when auto-sharing (`"auto"`, `"mmap"`, `"shm"`).

## Value

An object of class `shard_apply_policy`.
