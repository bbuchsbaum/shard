# Internal utility functions for shard package

# Null coalescing operator (if not already available from rlang)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
