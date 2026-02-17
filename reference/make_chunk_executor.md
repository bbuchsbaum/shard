# Create Chunk Executor Function

Creates a self-contained function that can execute a chunk in a worker.
This function is passed to dispatch_chunks and runs entirely within the
worker process.

## Usage

``` r
make_chunk_executor(auto_table = FALSE)
```

## Value

A function that executes chunks.
