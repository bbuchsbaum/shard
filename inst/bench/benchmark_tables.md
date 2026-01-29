# Benchmark Table Templates

Blog-post-ready benchmark table templates for shard performance documentation.

---

## Template A: Scaling (time vs workers)

```markdown
### Scaling benchmark: {suite_name}

**Dataset:** {dataset_id}
**Dims:** T={T}, P={P}, V={V}
**Machine:** {cpu}, {ram}GB, {os}, R {r_ver}, BLAS: {blas}

| Backend | Workers | Time (s) ↓ | Speedup ↑ | Peak RSS (GB) ↓ | End RSS (GB) ↓ | RSS drift (GB) ↓ | Hidden copies (MB) ↓ | Recycles |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| serial | 1 | __ | 1.00× | __ | __ | __ | __ | 0 |
| foreach + doParallel | 2 | __ | __× | __ | __ | __ | __ | 0 |
| foreach + doParallel | 4 | __ | __× | __ | __ | __ | __ | 0 |
| foreach + doParallel | 8 | __ | __× | __ | __ | __ | __ | 0 |
| shard | 2 | __ | __× | __ | __ | __ | __ | __ |
| shard | 4 | __ | __× | __ | __ | __ | __ | __ |
| shard | 8 | __ | __× | __ | __ | __ | __ | __ |
```

---

## Template B: Long-run stability (drift stress test)

```markdown
### Memory stability benchmark: {suite_name}

**Workload:** {task_count} tasks, {workers} workers
**Goal:** demonstrate flat RSS + deterministic cleanup

| Backend | Workers | Tasks | Peak RSS (GB) ↓ | End RSS (GB) ↓ | Drift (GB) ↓ | Failures | Recycles |
|---|---:|---:|---:|---:|---:|---:|---:|
| foreach + doParallel | __ | __ | __ | __ | __ | __ | 0 |
| future.apply | __ | __ | __ | __ | __ | __ | 0 |
| shard | __ | __ | __ | __ | __ | __ | __ |
```

---

## Suggested "first blog post" outline

If you want the post to basically write itself from the tables:

```markdown
# shard: Deterministic Parallelism in R Without Memory Surprises

## The problem (with one painful chart)
- peak RSS grows over time with long-running workers
- hidden copies multiply inputs

## shard's contract (3 bullets)
- shared immutable inputs
- explicit output buffers
- supervised workers with recycling

## Benchmarks
### 1) Many regressions (QR-once)
[Template A table]
- time scaling plot
- peak RSS plot

### 2) 10k-task drift test
[Template B table]
- RSS drift over task index plot

## Diagnostics: proving "no hidden copies"
- materialized_mb
- recycle events

## When to use shard (and when not to)
## Next: doShard + distributed v2
```
