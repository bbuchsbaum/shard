# Why shard is different

`shard` is a parallel execution runtime for R designed for
**deterministic memory behavior** and **zero-copy parallel reads**.

This document is benchmark-first: it focuses on measurable outcomes
(time, peak memory, memory returning, hidden copies), not slogans.

------------------------------------------------------------------------

## The pain `shard` fixes (and why existing tools struggle)

Most R parallel backends suffer from at least one of:

1.  **Input duplication**

- Big inputs get serialized and duplicated into every worker (PSOCK) or
  accidentally copied on mutation (fork/COW edge cases).

2.  **Hidden copies**

- A “small” change triggers copy-on-modify of a huge object.
- Native code sometimes writes into inputs unexpectedly.
- You find out only when RSS explodes.

3.  **Non-deterministic memory lifetime**

- GC timing + allocator fragmentation makes memory *not come back*.
- Long runs drift upward and eventually hit OOM.

4.  **Worker heap drift**

- Even if each task is “small”, workers accumulate fragmentation /
  cached allocations / package-level caches.

------------------------------------------------------------------------

## What `shard` changes (the contract)

`shard` introduces a stricter, more reliable contract:

### 1) Shared inputs are immutable (by default)

Large inputs are stored once and *attached* by workers. - No per-task
serialization. - No accidental in-place modification of shared inputs. -
If you try to mutate borrowed inputs: **error** (default).

### 2) Outputs are explicit buffers

Instead of returning massive objects from each worker, tasks write into
typed buffers. - Eliminates huge result-gather lists. - Enables
idempotent retries (safe re-run of a shard overwrites its own slice).

### 3) Deterministic cleanup via supervision + recycling

`shard` does not rely on GC behavior to be “nice”. - It monitors memory
drift. - It **recycles workers** when drift exceeds thresholds. - If a
worker crashes/OOMs, it restarts and requeues work safely.

### 4) Diagnostics you can prove

`shard` reports: - peak RSS (total + per-worker) - end RSS (did memory
come back?) - materialized bytes (hidden copies) - recycling events and
retries - (optional) COW auditing signals

------------------------------------------------------------------------

## Results snapshot (fill with your measured numbers)

> **Note:** The table below is a template. Replace placeholders with
> values from `inst/bench/` runs.

**Hardware/Software** - CPU: - RAM: - OS: - R version: - BLAS/LAPACK: -
Dataset sizes: T = **, P =** , V = \_\_

### Benchmark A: Many regressions (QR-once, chunked `QᵀY`)

| Backend                     | Workers | Time (s) ↓ | Speedup ↑ | Peak RSS (GB) ↓ | End RSS (GB) ↓ | Hidden copies (MB) ↓ | Recycles |
|-----------------------------|--------:|-----------:|----------:|----------------:|---------------:|---------------------:|---------:|
| Serial baseline             |       1 |       \_\_ |      1.0× |            \_\_ |           \_\_ |                 \_\_ |        0 |
| parallel::mclapply          |       8 |       \_\_ |     \_\_× |            \_\_ |           \_\_ |                 \_\_ |        0 |
| future.apply (multisession) |       8 |       \_\_ |     \_\_× |            \_\_ |           \_\_ |                 \_\_ |        0 |
| foreach + doParallel        |       8 |       \_\_ |     \_\_× |            \_\_ |           \_\_ |                 \_\_ |        0 |
| **shard**                   |       8 |       \_\_ |     \_\_× |            \_\_ |           \_\_ |                 \_\_ |     \_\_ |

### Benchmark B: 10k-task simulation sweep (memory drift stress test)

| Backend              | Workers | Time (s) ↓ | Peak RSS (GB) ↓ | End RSS (GB) ↓ | RSS drift (GB) ↓ | Failures | Recycles |
|----------------------|--------:|-----------:|----------------:|---------------:|-----------------:|---------:|---------:|
| foreach + doParallel |       8 |       \_\_ |            \_\_ |           \_\_ |             \_\_ |     \_\_ |        0 |
| future.apply         |       8 |       \_\_ |            \_\_ |           \_\_ |             \_\_ |     \_\_ |        0 |
| **shard**            |       8 |       \_\_ |            \_\_ |           \_\_ |             \_\_ |     \_\_ |     \_\_ |

**What “good” looks like** - Peak RSS: substantially lower than
PSOCK-based approaches for big shared inputs - End RSS: returns close to
baseline (or does so after recycling) - Hidden copies: near zero for
shared, immutable inputs in the hot path - Drift: flat RSS over long
runs

------------------------------------------------------------------------

## Why this matters across the R ecosystem

If your workload looks like “do the same numeric work over many slices
of big data”, `shard` is designed to: - scale cores without multiplying
memory - remain stable over long runs - surface memory/copy problems as
diagnostics instead of surprises

------------------------------------------------------------------------

## Reproducing benchmarks

Bench scripts live in `inst/bench/`. Recommended run pattern: -
warm-up - fixed seeds - multiple repetitions - report median time + max
peak RSS

Outputs: - `results.csv` - `shard_report.json` (optional) - plots (time
vs workers, peak RSS vs workers, RSS drift over task index)
