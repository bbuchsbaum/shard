#' shard: Deterministic, Zero-Copy Parallel Execution for R
#'
#' The shard package provides a parallel execution runtime for R that emphasizes
#' deterministic memory behavior and efficient handling of large shared inputs.
#'
#' @section Core API:
#' \describe{
#'   \item{\code{\link{share}}}{Create shared immutable memory segments}
#'   \item{\code{\link{buffer}}}{Create typed writable output buffers}
#'   \item{\code{\link{shards}}}{Create shard descriptors for parallel iteration}
#'   \item{\code{\link{shard_map}}}{Execute parallel computation with supervision}
#'   \item{\code{\link{arena}}}{Semantic scope for scratch memory}
#' }
#'
#' @section Diagnostics:
#' \describe{
#'   \item{\code{\link{report}}}{Primary entry point for run diagnostics}
#'   \item{\code{\link{mem_report}}}{Memory metrics data frame}
#'   \item{\code{\link{cow_report}}}{Copy-on-write event data frame}
#'   \item{\code{\link{copy_report}}}{Materialization/copy event data frame}
#'   \item{\code{\link{task_report}}}{Per-task metrics data frame}
#'   \item{\code{\link{segment_report}}}{Shared segment metrics data frame}
#' }
#'
#' @section Memory Model:
#' shard introduces a new parallel execution contract:
#' \enumerate{
#'   \item Large inputs are shared and immutable via mmap/shm
#'   \item Results go into explicit output buffers
#'   \item Memory stability is enforced via worker supervision and recycling
#' }
#'
#' @docType package
#' @name shard-package
#' @aliases shard
#' @useDynLib shard, .registration = TRUE
NULL

#' @importFrom stats setNames
#' @importFrom utils object.size
#' @importFrom methods setOldClass
#' @importFrom parallel clusterCall makeCluster stopCluster
NULL
