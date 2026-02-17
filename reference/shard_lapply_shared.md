# Apply a Function Over a List with Optional Auto-Sharing

A convenience wrapper for list workloads that need supervision and
shared inputs. Large atomic list elements are auto-shared based on
policy.

## Usage

``` r
shard_lapply_shared(
  x,
  FUN,
  VARS = NULL,
  workers = NULL,
  ...,
  policy = shard_apply_policy()
)
```

## Arguments

- x:

  A list.

- FUN:

  Function of the form `function(el, ...)`.

- VARS:

  Optional named list of extra variables (auto-shared when large).

- workers:

  Number of workers (passed to
  [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)).

- ...:

  Additional arguments forwarded to `FUN`.

- policy:

  A
  [`shard_apply_policy()`](https://bbuchsbaum.github.io/shard/reference/shard_apply_policy.md)
  object.

## Value

A list of results of length `length(x)`.

## Details

This wrapper enforces guardrails to avoid accidental huge gathers: it
estimates the total gathered result size from a probe call and refuses
to run if it exceeds `policy$max_gather_bytes`.
