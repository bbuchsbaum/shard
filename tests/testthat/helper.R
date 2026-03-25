# Test helpers loaded before all test files

#' Skip test when R socket connections are nearly exhausted.
#'
#' Each PSOCK worker needs at least 2 socket connections.
#' On shared CI nodes the per-process limit (default 128) can be
#' reached when many tests create/destroy worker pools.
skip_if_conn_exhausted <- function(n_needed = 6L) {
  n_open <- nrow(showConnections(all = TRUE))
  max_conn <- 128L
  if (n_open > max_conn - n_needed) {
    testthat::skip(
      sprintf("Socket connections nearly exhausted (%d/%d in use)", n_open, max_conn)
    )
  }
}
