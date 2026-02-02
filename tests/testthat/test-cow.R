# Tests for copy-on-write enforcement on shared vectors.
# The COW policy is controlled via the readonly parameter of share():
#   readonly=TRUE -> cow='deny' (mutations blocked)
#   readonly=FALSE -> cow='allow' (mutations permitted)

test_that("cow='deny' (readonly=TRUE) blocks [<- assignment", {
  x <- share(1:10, readonly = TRUE)

  expect_error(x[1] <- 99L, "cow='deny'")
  expect_error(x[1:3] <- c(1L, 2L, 3L), "cow='deny'")
})

test_that("cow='deny' (readonly=TRUE) blocks [[<- assignment on vectors", {
  # Note: [[<- on vectors (single element access)
  x <- share(1:10, readonly = TRUE)

  expect_error(x[[1]] <- 99L, "cow='deny'")
})

test_that("cow='deny' (readonly=TRUE) blocks names<- assignment", {
  x <- share(c(a = 1, b = 2, c = 3), readonly = TRUE)

  expect_error(names(x) <- c("x", "y", "z"), "cow='deny'")
})

test_that("cow='deny' (readonly=TRUE) blocks dim<- assignment", {
  x <- share(1:12, readonly = TRUE)

  expect_error(dim(x) <- c(3, 4), "cow='deny'")
})

test_that("cow='deny' (readonly=TRUE) blocks dimnames<- assignment", {
  M <- share(matrix(1:6, nrow = 2), readonly = TRUE)

  expect_error(dimnames(M) <- list(c("a", "b"), c("x", "y", "z")), "cow='deny'")
})

# Note: attr<- and attributes<- methods are defined but R's primitive dispatch
# behavior may not always call them. The COW enforcement for these is best-effort.

test_that("cow='allow' (readonly=FALSE) permits all modifications", {
  x <- share(1:10, readonly = FALSE)

  # These should all succeed
  x[1] <- 99L
  expect_equal(as.integer(x[1]), 99L)

  names(x) <- letters[1:10]
  expect_equal(names(x)[1], "a")
})

test_that("default readonly=TRUE blocks modifications", {
  # Default is readonly=TRUE, which sets cow='deny'
  x <- share(1:10)

  expect_error(x[1] <- 99L, "cow='deny'")
})

test_that("print.shard_shared_vector works correctly", {
  x <- share(1:5)

  out <- capture.output(print(x))
  expect_true(length(out) > 0)
})

test_that("print.shard_shared_vector preserves underlying class", {
  # Date vector
  d <- share(as.Date(c("2024-01-01", "2024-01-02")))

  out <- capture.output(print(d))
  # Should print as dates, not as raw integers
  expect_true(any(grepl("2024", out)))
})

test_that(".shard_cow_policy extracts policy from attribute", {
  x <- share(1:5, readonly = TRUE)
  expect_equal(shard:::.shard_cow_policy(x), "deny")

  y <- share(1:5, readonly = FALSE)
  expect_equal(shard:::.shard_cow_policy(y), "allow")
})

test_that(".shard_cow_policy returns 'allow' for missing/invalid policy", {
  x <- 1:5  # Not shared, no policy
  expect_equal(shard:::.shard_cow_policy(x), "allow")

  # Shared but with NULL policy attr
  y <- share(1:5, readonly = FALSE)
  attr(y, "shard_cow") <- NULL
  expect_equal(shard:::.shard_cow_policy(y), "allow")
})

test_that("cow replacement methods preserve class", {
  x <- share(1:10, readonly = FALSE)
  original_class <- class(x)

  x[1] <- 99L
  expect_equal(class(x), original_class)

  names(x) <- letters[1:10]
  expect_equal(class(x), original_class)
})

test_that("cow='deny' works with matrices", {
  M <- share(matrix(1:12, nrow = 3), readonly = TRUE)

  expect_error(M[1, 1] <- 99L, "cow='deny'")
  expect_error(M[, 1] <- c(1L, 2L, 3L), "cow='deny'")
})

test_that("cow replacement preserves shard_shared_vector class after mutation", {
  x <- share(1:10, readonly = FALSE)

  # After mutation, class should still have shard_shared_vector
  x[1] <- 99L
  expect_true("shard_shared_vector" %in% class(x))

  # dim<- assignment
  y <- share(1:12, readonly = FALSE)
  dim(y) <- c(3, 4)
  expect_true("shard_shared_vector" %in% class(y))
  expect_equal(dim(y), c(3, 4))
})
