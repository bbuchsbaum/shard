try({
  if (requireNamespace("withr", quietly = TRUE)) {
    withr::defer({
      try(pool_stop(), silent = TRUE)
      try(closeAllConnections(), silent = TRUE)
    }, testthat::teardown_env())
  }
}, silent = TRUE)

