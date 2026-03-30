# Package index

## All functions

- [`adapter`](https://bbuchsbaum.github.io/shard/reference/adapter.md) :
  Adapter Registry for Class-Specific Deep Sharing
- [`affinity`](https://bbuchsbaum.github.io/shard/reference/affinity.md)
  : CPU Affinity + mmap Advice (Advanced)
- [`affinity_supported()`](https://bbuchsbaum.github.io/shard/reference/affinity_supported.md)
  : Check whether CPU affinity is supported
- [`altrep`](https://bbuchsbaum.github.io/shard/reference/altrep.md) :
  ALTREP Shared Vectors
- [`arena()`](https://bbuchsbaum.github.io/shard/reference/arena.md) :
  Arena Semantic Scope
- [`arena_depth()`](https://bbuchsbaum.github.io/shard/reference/arena_depth.md)
  : Get Current Arena Depth
- [`as.array(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.array.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Array
- [`as.double(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.double.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Double
- [`as.integer(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.integer.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Integer
- [`as.logical(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.logical.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Logical
- [`as.matrix(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.matrix.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Matrix
- [`as.raw(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.raw.shard_buffer.md)
  : Coerce a Shared Memory Buffer to Raw
- [`as.vector(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as.vector.shard_buffer.md)
  : Coerce a Shared Memory Buffer to a Vector
- [`as_shared()`](https://bbuchsbaum.github.io/shard/reference/as_shared.md)
  : Create a shared vector from an existing R vector
- [`as_tibble()`](https://bbuchsbaum.github.io/shard/reference/as_tibble.md)
  : Materialize a shard table handle as a data.frame/tibble
- [`as_tibble(`*`<shard_dataset>`*`)`](https://bbuchsbaum.github.io/shard/reference/as_tibble.shard_dataset.md)
  : Materialize a dataset handle into a data.frame/tibble
- [`as_tibble(`*`<shard_row_groups>`*`)`](https://bbuchsbaum.github.io/shard/reference/as_tibble.shard_row_groups.md)
  : Materialize a row-groups handle into a data.frame/tibble
- [`as_tibble(`*`<shard_table_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/as_tibble.shard_table_buffer.md)
  : Materialize a fixed table handle or buffer
- [`as_tibble(`*`<shard_table_handle>`*`)`](https://bbuchsbaum.github.io/shard/reference/as_tibble.shard_table_handle.md)
  : Materialize a table handle into a data.frame/tibble
- [`` `attr<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/attr-set-.shard_shared_vector.md)
  : Set an Attribute on a Shared Vector
- [`` `attributes<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/attributes-set-.shard_shared_vector.md)
  : Set Attributes on a Shared Vector
- [`available_backings()`](https://bbuchsbaum.github.io/shard/reference/available_backings.md)
  : Get available shared memory backing types
- [`buffer()`](https://bbuchsbaum.github.io/shard/reference/buffer.md) :
  Shared Memory Buffers
- [`buffer_advise()`](https://bbuchsbaum.github.io/shard/reference/buffer_advise.md)
  : Advise access pattern for a buffer
- [`buffer_close()`](https://bbuchsbaum.github.io/shard/reference/buffer_close.md)
  : Close a Buffer
- [`buffer_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/buffer_diagnostics.md)
  : Buffer Diagnostics
- [`buffer_info()`](https://bbuchsbaum.github.io/shard/reference/buffer_info.md)
  : Get Buffer Info
- [`buffer_open()`](https://bbuchsbaum.github.io/shard/reference/buffer_open.md)
  : Open an Existing Buffer
- [`buffer_path()`](https://bbuchsbaum.github.io/shard/reference/buffer_path.md)
  : Get Buffer Path
- [`close(`*`<shard_shared>`*`)`](https://bbuchsbaum.github.io/shard/reference/close.shard_shared.md)
  [`close(`*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/close.shard_shared.md)
  [`close(`*`<shard_deep_shared>`*`)`](https://bbuchsbaum.github.io/shard/reference/close.shard_shared.md)
  : Close a Shared Object
- [`collect()`](https://bbuchsbaum.github.io/shard/reference/collect.md)
  : Collect a shard table into memory
- [`collect(`*`<shard_dataset>`*`)`](https://bbuchsbaum.github.io/shard/reference/collect.shard_dataset.md)
  : Collect a dataset handle into memory
- [`collect(`*`<shard_row_groups>`*`)`](https://bbuchsbaum.github.io/shard/reference/collect.shard_row_groups.md)
  : Collect a row-groups handle into memory
- [`collect(`*`<shard_table_handle>`*`)`](https://bbuchsbaum.github.io/shard/reference/collect.shard_table_handle.md)
  : Collect a table handle into memory
- [`int32()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)
  [`float64()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)
  [`bool()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)
  [`raw_col()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)
  [`string_col()`](https://bbuchsbaum.github.io/shard/reference/coltypes.md)
  : Column Types
- [`copy_report()`](https://bbuchsbaum.github.io/shard/reference/copy_report.md)
  : Data Copy Report
- [`cow_report()`](https://bbuchsbaum.github.io/shard/reference/cow_report.md)
  : Copy-on-Write Policy Report
- [`diagnostics`](https://bbuchsbaum.github.io/shard/reference/diagnostics.md)
  : Diagnostics API
- [`` `dim<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/dim-set-.shard_shared_vector.md)
  : Set dim on a Shared Vector
- [`dim(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/dim.shard_buffer.md)
  : Dimensions of a Shared Memory Buffer
- [`` `dimnames<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/dimnames-set-.shard_shared_vector.md)
  : Set dimnames on a Shared Vector
- [`dispatch`](https://bbuchsbaum.github.io/shard/reference/dispatch.md)
  : Task Dispatch Engine
- [`dispatch_chunks()`](https://bbuchsbaum.github.io/shard/reference/dispatch_chunks.md)
  : Dispatch Chunks to Worker Pool
- [`ergonomics`](https://bbuchsbaum.github.io/shard/reference/ergonomics.md)
  : Ergonomic Apply/Lapply Wrappers
- [`factor_col()`](https://bbuchsbaum.github.io/shard/reference/factor_col.md)
  : Categorical column type
- [`fetch()`](https://bbuchsbaum.github.io/shard/reference/fetch.md) :
  Fetch Data from a Shared Object
- [`idx_range()`](https://bbuchsbaum.github.io/shard/reference/idx_range.md)
  : Contiguous index range
- [`in_arena()`](https://bbuchsbaum.github.io/shard/reference/in_arena.md)
  : Check if Currently Inside an Arena
- [`is_shared()`](https://bbuchsbaum.github.io/shard/reference/is_shared.md)
  : Check if Object is Shared
- [`is_shared_vector()`](https://bbuchsbaum.github.io/shard/reference/is_shared_vector.md)
  : Check if an object is a shared vector
- [`is_view()`](https://bbuchsbaum.github.io/shard/reference/is_view.md)
  [`is_block_view()`](https://bbuchsbaum.github.io/shard/reference/is_view.md)
  : View Predicates
- [`is_windows()`](https://bbuchsbaum.github.io/shard/reference/is_windows.md)
  : Check if running on Windows
- [`iterate_row_groups()`](https://bbuchsbaum.github.io/shard/reference/iterate_row_groups.md)
  : Iterate row groups
- [`length(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/length.shard_buffer.md)
  : Length of a Shared Memory Buffer
- [`length(`*`<shard_descriptor>`*`)`](https://bbuchsbaum.github.io/shard/reference/length.shard_descriptor.md)
  : Length of a shard_descriptor Object
- [`length(`*`<shard_descriptor_lazy>`*`)`](https://bbuchsbaum.github.io/shard/reference/length.shard_descriptor_lazy.md)
  : Length of a shard_descriptor_lazy Object
- [`list_kernels()`](https://bbuchsbaum.github.io/shard/reference/list_kernels.md)
  : List registered kernels
- [`materialize()`](https://bbuchsbaum.github.io/shard/reference/materialize.md)
  : Materialize Shared Object
- [`materialize(`*`<shard_view_block>`*`)`](https://bbuchsbaum.github.io/shard/reference/materialize.shard_view_block.md)
  : Materialize a block view into an R matrix
- [`materialize(`*`<shard_view_gather>`*`)`](https://bbuchsbaum.github.io/shard/reference/materialize.shard_view_gather.md)
  : Materialize a gather view into an R matrix
- [`mem_report()`](https://bbuchsbaum.github.io/shard/reference/mem_report.md)
  : Memory Usage Report
- [`` `names<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/names-set-.shard_shared_vector.md)
  : Set Names on a Shared Vector
- [`pin_workers()`](https://bbuchsbaum.github.io/shard/reference/pin_workers.md)
  : Pin shard workers to CPU cores
- [`pool`](https://bbuchsbaum.github.io/shard/reference/pool.md) :
  Worker Pool Management
- [`pool_create()`](https://bbuchsbaum.github.io/shard/reference/pool_create.md)
  : Create a Worker Pool
- [`pool_dispatch()`](https://bbuchsbaum.github.io/shard/reference/pool_dispatch.md)
  : Dispatch Task to Worker
- [`pool_get()`](https://bbuchsbaum.github.io/shard/reference/pool_get.md)
  : Get the Current Worker Pool
- [`pool_health_check()`](https://bbuchsbaum.github.io/shard/reference/pool_health_check.md)
  : Check Pool Health
- [`pool_lapply()`](https://bbuchsbaum.github.io/shard/reference/pool_lapply.md)
  : Parallel Dispatch with Async Workers
- [`pool_sapply()`](https://bbuchsbaum.github.io/shard/reference/pool_sapply.md)
  : Parallel sapply with Supervision
- [`pool_status()`](https://bbuchsbaum.github.io/shard/reference/pool_status.md)
  : Get Pool Status
- [`pool_stop()`](https://bbuchsbaum.github.io/shard/reference/pool_stop.md)
  : Stop the Worker Pool
- [`print(`*`<arena_result>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.arena_result.md)
  : Print an arena_result object
- [`print(`*`<shard_apply_policy>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_apply_policy.md)
  : Print a shard_apply_policy Object
- [`print(`*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_buffer.md)
  : Print a Shared Memory Buffer
- [`print(`*`<shard_deep_shared>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_deep_shared.md)
  : Print a Deep-Shared Object
- [`print(`*`<shard_descriptor>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_descriptor.md)
  : Print a shard_descriptor Object
- [`print(`*`<shard_descriptor_lazy>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_descriptor_lazy.md)
  : Print a shard_descriptor_lazy Object
- [`print(`*`<shard_dispatch_result>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_dispatch_result.md)
  : Print a shard_dispatch_result Object
- [`print(`*`<shard_health_report>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_health_report.md)
  : Print a shard_health_report Object
- [`print(`*`<shard_idx_range>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_idx_range.md)
  : Print a shard_idx_range object
- [`print(`*`<shard_pool>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_pool.md)
  : Print a shard_pool Object
- [`print(`*`<shard_reduce_result>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_reduce_result.md)
  : Print a shard_reduce_result Object
- [`print(`*`<shard_report>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_report.md)
  : Print a shard_report Object
- [`print(`*`<shard_result>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_result.md)
  : Print a shard_result Object
- [`print(`*`<shard_segment>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_segment.md)
  : Print a Shared Memory Segment
- [`print(`*`<shard_shared>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_shared.md)
  : Print a Shared Object
- [`print(`*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_shared_vector.md)
  : Print a Shared Vector
- [`print(`*`<shard_tiles>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_tiles.md)
  : Print a shard_tiles object
- [`print(`*`<shard_view_block>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_view_block.md)
  : Print a shard_view_block object
- [`print(`*`<shard_view_gather>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_view_gather.md)
  : Print a shard_view_gather object
- [`print(`*`<shard_worker>`*`)`](https://bbuchsbaum.github.io/shard/reference/print.shard_worker.md)
  : Print a shard_worker Object
- [`queue`](https://bbuchsbaum.github.io/shard/reference/queue.md) :
  Chunk Queue Management
- [`recommendations()`](https://bbuchsbaum.github.io/shard/reference/recommendations.md)
  : Performance Recommendations
- [`register_kernel()`](https://bbuchsbaum.github.io/shard/reference/register_kernel.md)
  : Register a shard kernel
- [`report()`](https://bbuchsbaum.github.io/shard/reference/report.md) :
  Generate Shard Runtime Report
- [`results()`](https://bbuchsbaum.github.io/shard/reference/results.md)
  : Extract Results from shard_map
- [`row_layout()`](https://bbuchsbaum.github.io/shard/reference/row_layout.md)
  : Row layout for fixed-row table outputs
- [`rss`](https://bbuchsbaum.github.io/shard/reference/rss.md) : RSS
  Monitoring Utilities
- [`schema()`](https://bbuchsbaum.github.io/shard/reference/schema.md) :
  Define a table schema
- [`scratch_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/scratch_diagnostics.md)
  : Scratch pool diagnostics
- [`scratch_matrix()`](https://bbuchsbaum.github.io/shard/reference/scratch_matrix.md)
  : Get a scratch matrix
- [`scratch_pool_config()`](https://bbuchsbaum.github.io/shard/reference/scratch_pool_config.md)
  : Configure scratch pool limits
- [`segment`](https://bbuchsbaum.github.io/shard/reference/segment.md) :
  Shared Memory Segment
- [`segment_advise()`](https://bbuchsbaum.github.io/shard/reference/segment_advise.md)
  : Advise OS about expected access pattern for a segment
- [`segment_close()`](https://bbuchsbaum.github.io/shard/reference/segment_close.md)
  : Close a shared memory segment
- [`segment_create()`](https://bbuchsbaum.github.io/shard/reference/segment_create.md)
  : Create a new shared memory segment
- [`segment_info()`](https://bbuchsbaum.github.io/shard/reference/segment_info.md)
  : Get segment information
- [`segment_open()`](https://bbuchsbaum.github.io/shard/reference/segment_open.md)
  : Open an existing shared memory segment
- [`segment_path()`](https://bbuchsbaum.github.io/shard/reference/segment_path.md)
  : Get the path or name of a segment
- [`segment_protect()`](https://bbuchsbaum.github.io/shard/reference/segment_protect.md)
  : Make a segment read-only
- [`segment_read()`](https://bbuchsbaum.github.io/shard/reference/segment_read.md)
  : Read raw data from a segment
- [`segment_report()`](https://bbuchsbaum.github.io/shard/reference/segment_report.md)
  : Shared Memory Segment Report
- [`segment_size()`](https://bbuchsbaum.github.io/shard/reference/segment_size.md)
  : Get the size of a segment
- [`segment_write()`](https://bbuchsbaum.github.io/shard/reference/segment_write.md)
  : Write data to a segment
- [`set_affinity()`](https://bbuchsbaum.github.io/shard/reference/set_affinity.md)
  : Set CPU affinity for the current process
- [`shard_apply_matrix()`](https://bbuchsbaum.github.io/shard/reference/shard_apply_matrix.md)
  : Apply a Function Over Matrix Columns with Shared Inputs
- [`shard_apply_policy()`](https://bbuchsbaum.github.io/shard/reference/shard_apply_policy.md)
  : Apply Wrapper Policy
- [`shard_crossprod()`](https://bbuchsbaum.github.io/shard/reference/shard_crossprod.md)
  : Parallel crossprod() using shard views + output buffers
- [`shard_get_adapter()`](https://bbuchsbaum.github.io/shard/reference/shard_get_adapter.md)
  : Get Adapter for an Object
- [`shard_lapply_shared()`](https://bbuchsbaum.github.io/shard/reference/shard_lapply_shared.md)
  : Apply a Function Over a List with Optional Auto-Sharing
- [`shard_list_adapters()`](https://bbuchsbaum.github.io/shard/reference/shard_list_adapters.md)
  : List Registered Adapters
- [`shard_map()`](https://bbuchsbaum.github.io/shard/reference/shard_map.md)
  : Parallel Execution with shard_map
- [`shard_reduce()`](https://bbuchsbaum.github.io/shard/reference/shard_reduce.md)
  : Streaming Reductions over Shards
- [`shard_register_adapter()`](https://bbuchsbaum.github.io/shard/reference/shard_register_adapter.md)
  : Register an Adapter for Class-Specific Traversal
- [`shard_share_hook()`](https://bbuchsbaum.github.io/shard/reference/shard_share_hook.md)
  : Deep Sharing Hook for Custom Classes
- [`shard_unregister_adapter()`](https://bbuchsbaum.github.io/shard/reference/shard_unregister_adapter.md)
  : Unregister an Adapter
- [`shards()`](https://bbuchsbaum.github.io/shard/reference/shards.md) :
  Shard Descriptor Creation
- [`shards_list()`](https://bbuchsbaum.github.io/shard/reference/shards_list.md)
  : Create Shards from an Explicit Index List
- [`share()`](https://bbuchsbaum.github.io/shard/reference/share.md) :
  Zero-Copy Shared Objects
- [`share_open()`](https://bbuchsbaum.github.io/shard/reference/share_open.md)
  : Open an Existing Shared Object by Path
- [`shared_advise()`](https://bbuchsbaum.github.io/shard/reference/shared_advise.md)
  : Advise access pattern for a shared input vector/matrix
- [`shared_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/shared_diagnostics.md)
  : Get diagnostics for a shared vector
- [`shared_info()`](https://bbuchsbaum.github.io/shard/reference/shared_info.md)
  : Get Information About a Shared Object
- [`shared_reset_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/shared_reset_diagnostics.md)
  : Reset diagnostic counters for a shared vector
- [`shared_segment()`](https://bbuchsbaum.github.io/shard/reference/shared_segment.md)
  : Get the underlying segment from a shared vector
- [`shared_vector()`](https://bbuchsbaum.github.io/shard/reference/shared_vector.md)
  : Create a shared vector from a segment
- [`shared_view()`](https://bbuchsbaum.github.io/shard/reference/shared_view.md)
  : Create a view (subset) of a shared vector
- [`stream_count()`](https://bbuchsbaum.github.io/shard/reference/stream_count.md)
  : Stream row count
- [`stream_filter()`](https://bbuchsbaum.github.io/shard/reference/stream_filter.md)
  : Stream-filter a dataset/row-groups into a new partitioned dataset
- [`stream_group_count()`](https://bbuchsbaum.github.io/shard/reference/stream_group_count.md)
  : Stream group-wise count
- [`stream_group_sum()`](https://bbuchsbaum.github.io/shard/reference/stream_group_sum.md)
  : Stream group-wise sum
- [`stream_map()`](https://bbuchsbaum.github.io/shard/reference/stream_map.md)
  : Stream over row-groups/datasets and map
- [`stream_reduce()`](https://bbuchsbaum.github.io/shard/reference/stream_reduce.md)
  : Stream over row-groups/datasets and reduce
- [`stream_sum()`](https://bbuchsbaum.github.io/shard/reference/stream_sum.md)
  : Stream sum of a numeric column
- [`stream_top_k()`](https://bbuchsbaum.github.io/shard/reference/stream_top_k.md)
  : Stream top-k rows by a numeric column
- [`` `[`( ``*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-.shard_buffer.md)
  : Extract Buffer Elements
- [`` `[`( ``*`<shard_descriptor>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-.shard_descriptor.md)
  : Subset Shard Descriptor
- [`` `[`( ``*`<shard_descriptor_lazy>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-.shard_descriptor_lazy.md)
  : Subset a shard_descriptor_lazy Object
- [`` `[[`( ``*`<shard_descriptor>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-sub-.shard_descriptor.md)
  : Get Single Shard
- [`` `[[`( ``*`<shard_descriptor_lazy>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-sub-.shard_descriptor_lazy.md)
  : Extract a Single Shard from a shard_descriptor_lazy Object
- [`` `[[<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/sub-subset-.shard_shared_vector.md)
  : Double-bracket Subset-assign a Shared Vector
- [`` `[<-`( ``*`<shard_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/subset-.shard_buffer.md)
  : Assign to Buffer Elements
- [`` `[<-`( ``*`<shard_shared_vector>`*`)`](https://bbuchsbaum.github.io/shard/reference/subset-.shard_shared_vector.md)
  : Subset-assign a Shared Vector
- [`succeeded()`](https://bbuchsbaum.github.io/shard/reference/succeeded.md)
  : Check if shard_map Succeeded
- [`table_buffer()`](https://bbuchsbaum.github.io/shard/reference/table_buffer.md)
  : Allocate a fixed-row table buffer
- [`table_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/table_diagnostics.md)
  : Table Diagnostics
- [`table_finalize()`](https://bbuchsbaum.github.io/shard/reference/table_finalize.md)
  : Finalize a table buffer or sink
- [`table_finalize(`*`<shard_table_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/table_finalize.shard_table_buffer.md)
  : Finalize a table buffer
- [`table_finalize(`*`<shard_table_sink>`*`)`](https://bbuchsbaum.github.io/shard/reference/table_finalize.shard_table_sink.md)
  : Finalize a sink
- [`table_sink()`](https://bbuchsbaum.github.io/shard/reference/table_sink.md)
  : Create a table sink for row-group or partitioned outputs
- [`table_write()`](https://bbuchsbaum.github.io/shard/reference/table_write.md)
  : Write tabular results into a table buffer or sink
- [`table_write(`*`<shard_table_buffer>`*`)`](https://bbuchsbaum.github.io/shard/reference/table_write.shard_table_buffer.md)
  : Write into a table buffer
- [`table_write(`*`<shard_table_sink>`*`)`](https://bbuchsbaum.github.io/shard/reference/table_write.shard_table_sink.md)
  : Write a shard's row-group output
- [`task_report()`](https://bbuchsbaum.github.io/shard/reference/task_report.md)
  : Task Execution Report
- [`utils`](https://bbuchsbaum.github.io/shard/reference/utils.md) :
  Utility Functions
- [`view()`](https://bbuchsbaum.github.io/shard/reference/view.md) :
  Create a view over a shared matrix
- [`view_block()`](https://bbuchsbaum.github.io/shard/reference/view_block.md)
  : Create a contiguous block view
- [`view_diagnostics()`](https://bbuchsbaum.github.io/shard/reference/view_diagnostics.md)
  : View diagnostics
- [`view_gather()`](https://bbuchsbaum.github.io/shard/reference/view_gather.md)
  : Create a gather (indexed) view over a shared matrix
- [`view_info()`](https://bbuchsbaum.github.io/shard/reference/view_info.md)
  : Introspection for a view
- [`views`](https://bbuchsbaum.github.io/shard/reference/views.md) :
  Zero-copy Views
- [`worker`](https://bbuchsbaum.github.io/shard/reference/worker.md) :
  Individual Worker Control
