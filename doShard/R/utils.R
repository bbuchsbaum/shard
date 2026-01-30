# Small helpers (kept minimal to avoid adding dependencies).

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

