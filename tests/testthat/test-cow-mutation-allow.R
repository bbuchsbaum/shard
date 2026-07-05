# The cow='allow' mutation paths for shard_shared_vector replacement methods.
# R's primitive dispatch does not reliably route attributes<-/attr<- through S3
# methods, so we invoke the methods directly to exercise the allow-policy body
# (class save/restore around the primitive replacement).

test_that("[[<- allows element assignment under cow='allow'", {
  x <- share(1:10, readonly = FALSE)
  y <- shard:::`[[<-.shard_shared_vector`(x, 2L, value = 99L)
  expect_equal(as.integer(y[[2]]), 99L)
  expect_true(inherits(y, "shard_shared_vector"))
})

test_that("attr<- allows attribute assignment under cow='allow'", {
  x <- share(1:10, readonly = FALSE)
  y <- shard:::`attr<-.shard_shared_vector`(x, "label", "hello")
  expect_equal(attr(y, "label"), "hello")
  expect_true(inherits(y, "shard_shared_vector"))
})

test_that("attributes<- allows bulk attribute assignment under cow='allow'", {
  x <- share(1:10, readonly = FALSE)
  y <- shard:::`attributes<-.shard_shared_vector`(x, list(names = letters[1:10]))
  expect_equal(names(y)[1], "a")
  expect_true(inherits(y, "shard_shared_vector"))
})

test_that("dimnames<- allows dimnames assignment under cow='allow'", {
  M <- share(matrix(1:6, nrow = 2), readonly = FALSE)
  y <- shard:::`dimnames<-.shard_shared_vector`(M, list(c("a", "b"), c("x", "y", "z")))
  expect_equal(dimnames(y)[[1]], c("a", "b"))
  expect_true(inherits(y, "shard_shared_vector"))
})

test_that("all replacement methods still enforce cow='deny'", {
  x <- share(1:10, readonly = TRUE)
  expect_error(shard:::`[[<-.shard_shared_vector`(x, 1L, value = 1L), "cow='deny'")
  expect_error(shard:::`attr<-.shard_shared_vector`(x, "a", 1), "cow='deny'")
  expect_error(shard:::`attributes<-.shard_shared_vector`(x, list(a = 1)), "cow='deny'")
  M <- share(matrix(1:6, 2), readonly = TRUE)
  expect_error(shard:::`dimnames<-.shard_shared_vector`(M, list(NULL, NULL)), "cow='deny'")
})
