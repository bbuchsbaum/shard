# Tests for deep sharing hook system

test_that("default hook returns empty list (no-op)", {
    x <- list(a = 1:10)
    ctx <- list(
        path = "<root>",
        class = class(x),
        mode = "balanced",
        min_bytes = 1000,
        types = c("double", "integer"),
        deep = TRUE
    )

    result <- shard_share_hook(x, ctx)

    expect_type(result, "list")
    expect_length(result, 0)
})

test_that("hook with skip_paths prevents traversal", {
    # Define a custom class with a hook
    setClass("SkipPathTest", slots = c(data = "numeric", cache = "list"))

    # Create hook that skips the cache
    shard_share_hook.SkipPathTest <- function(x, ctx) {
        list(skip_paths = paste0(ctx$path, "@cache"))
    }

    # Register the method
    registerS3method("shard_share_hook", "SkipPathTest", shard_share_hook.SkipPathTest)

    obj <- new("SkipPathTest",
               data = rnorm(10000),  # Large enough to share
               cache = list(temp = rnorm(10000)))

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # The data slot should be shared, but cache contents should be kept
    # (cache path was skipped, so its children weren't traversed deeply)
    expect_true(shared$summary$shared_count >= 1)

    recovered <- fetch(shared)
    expect_equal(recovered@data, obj@data)
    expect_equal(recovered@cache, obj@cache)

    close(shared)

    # Clean up
    rm(shard_share_hook.SkipPathTest, envir = globalenv())
    removeClass("SkipPathTest")
})

test_that("hook with skip_slots prevents slot traversal for S4 objects", {
    # Define S4 class
    setClass("CacheModel", slots = c(
        coefficients = "numeric",
        cache = "list"
    ))

    # Hook that skips cache slot
    shard_share_hook.CacheModel <- function(x, ctx) {
        list(skip_slots = "cache")
    }

    registerS3method("shard_share_hook", "CacheModel", shard_share_hook.CacheModel)

    obj <- new("CacheModel",
               coefficients = rnorm(10000),
               cache = list(fitted = rnorm(10000), residuals = rnorm(10000)))

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # Only coefficients should be shared (cache slot was skipped entirely)
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered@coefficients, obj@coefficients)
    # Cache should still be there but wasn't traversed/shared
    expect_equal(recovered@cache, obj@cache)

    close(shared)

    rm(shard_share_hook.CacheModel, envir = globalenv())
    removeClass("CacheModel")
})

test_that("hook with force_share_paths shares small objects", {
    # Define class that forces sharing of small data
    setClass("ForceShareTest", slots = c(
        important_small = "numeric",
        unimportant_large = "numeric"
    ))

    shard_share_hook.ForceShareTest <- function(x, ctx) {
        list(force_share_paths = paste0(ctx$path, "@important_small"))
    }

    registerS3method("shard_share_hook", "ForceShareTest", shard_share_hook.ForceShareTest)

    obj <- new("ForceShareTest",
               important_small = 1:10,  # Small - normally wouldn't be shared
               unimportant_large = rnorm(10000))  # Large - would be shared

    # Use high threshold so only forced paths are shared
    shared <- share(obj, deep = TRUE, min_bytes = 100000)

    expect_s3_class(shared, "shard_deep_shared")

    # important_small was forced, unimportant_large is below threshold
    # So only 1 segment should be shared (the forced one)
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered@important_small, obj@important_small)
    expect_equal(recovered@unimportant_large, obj@unimportant_large)

    close(shared)

    rm(shard_share_hook.ForceShareTest, envir = globalenv())
    removeClass("ForceShareTest")
})

test_that("hook with rewrite transforms object before sharing", {
    # Class that has lazy data needing materialization
    setClass("LazyData", slots = c(data = "ANY", materialized = "logical"))

    shard_share_hook.LazyData <- function(x, ctx) {
        list(
            rewrite = function(obj) {
                # Transform the object - e.g., materialize lazy data
                obj@materialized <- TRUE
                obj
            }
        )
    }

    registerS3method("shard_share_hook", "LazyData", shard_share_hook.LazyData)

    obj <- new("LazyData", data = rnorm(10000), materialized = FALSE)

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    recovered <- fetch(shared)
    # The rewrite function should have set materialized to TRUE
    expect_true(recovered@materialized)
    expect_equal(recovered@data, obj@data)

    close(shared)

    rm(shard_share_hook.LazyData, envir = globalenv())
    removeClass("LazyData")
})

test_that("hook errors in strict mode propagate", {
    setClass("ErrorHook", slots = c(data = "numeric"))

    shard_share_hook.ErrorHook <- function(x, ctx) {
        stop("Deliberate hook error")
    }

    registerS3method("shard_share_hook", "ErrorHook", shard_share_hook.ErrorHook)

    obj <- new("ErrorHook", data = rnorm(10000))

    expect_error(
        share(obj, deep = TRUE, min_bytes = 1000, mode = "strict"),
        "Hook error in strict mode"
    )

    rm(shard_share_hook.ErrorHook, envir = globalenv())
    removeClass("ErrorHook")
})

test_that("hook errors in balanced mode continue and track error", {
    setClass("ErrorHookBalanced", slots = c(data = "numeric"))

    shard_share_hook.ErrorHookBalanced <- function(x, ctx) {
        stop("Deliberate hook error for balanced mode")
    }

    registerS3method("shard_share_hook", "ErrorHookBalanced", shard_share_hook.ErrorHookBalanced)

    obj <- new("ErrorHookBalanced", data = rnorm(10000))

    # Should NOT error in balanced mode
    shared <- share(obj, deep = TRUE, min_bytes = 1000, mode = "balanced")

    expect_s3_class(shared, "shard_deep_shared")

    # Should have recorded the hook error
    expect_true(shared$summary$hook_error_count > 0)
    expect_true(length(shared$summary$hook_errors) > 0)

    # Data should still be shared
    recovered <- fetch(shared)
    expect_equal(recovered@data, obj@data)

    close(shared)

    rm(shard_share_hook.ErrorHookBalanced, envir = globalenv())
    removeClass("ErrorHookBalanced")
})

test_that("custom class hook dispatches correctly", {
    # Define a custom S3 class
    my_object <- structure(
        list(
            large_data = rnorm(10000),
            small_data = 1:10
        ),
        class = c("MyCustomClass", "list")
    )

    # Define hook for this class
    shard_share_hook.MyCustomClass <- function(x, ctx) {
        # Force share small_data even though it's tiny
        list(force_share_paths = paste0(ctx$path, "$small_data"))
    }

    registerS3method("shard_share_hook", "MyCustomClass", shard_share_hook.MyCustomClass)

    # With very high threshold, normally nothing would be shared
    shared <- share(my_object, deep = TRUE, min_bytes = 1000000)

    # But hook forced small_data to be shared
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered$large_data, my_object$large_data)
    expect_equal(recovered$small_data, my_object$small_data)

    close(shared)

    rm(shard_share_hook.MyCustomClass, envir = globalenv())
})

test_that("hook context contains correct information", {
    captured_ctx <- NULL

    setClass("ContextCapture", slots = c(data = "numeric"))

    shard_share_hook.ContextCapture <- function(x, ctx) {
        captured_ctx <<- ctx
        list()
    }

    registerS3method("shard_share_hook", "ContextCapture", shard_share_hook.ContextCapture)

    obj <- new("ContextCapture", data = rnorm(100))

    shared <- share(obj, deep = TRUE, min_bytes = 1000, mode = "balanced")

    # Check context fields
    expect_equal(captured_ctx$path, "<root>")
    expect_true("ContextCapture" %in% captured_ctx$class)
    expect_equal(captured_ctx$mode, "balanced")
    expect_equal(captured_ctx$min_bytes, 1000)
    expect_true("double" %in% captured_ctx$types)
    expect_true(captured_ctx$deep)

    close(shared)

    rm(shard_share_hook.ContextCapture, captured_ctx, envir = globalenv())
    removeClass("ContextCapture")
})

test_that("hook rewrite error in strict mode propagates", {
    setClass("RewriteError", slots = c(data = "numeric"))

    shard_share_hook.RewriteError <- function(x, ctx) {
        list(
            rewrite = function(obj) {
                stop("Rewrite failed!")
            }
        )
    }

    registerS3method("shard_share_hook", "RewriteError", shard_share_hook.RewriteError)

    obj <- new("RewriteError", data = rnorm(100))

    expect_error(
        share(obj, deep = TRUE, min_bytes = 1000, mode = "strict"),
        "Rewrite function error"
    )

    rm(shard_share_hook.RewriteError, envir = globalenv())
    removeClass("RewriteError")
})

test_that("hook rewrite error in balanced mode continues", {
    setClass("RewriteErrorBalanced", slots = c(data = "numeric"))

    shard_share_hook.RewriteErrorBalanced <- function(x, ctx) {
        list(
            rewrite = function(obj) {
                stop("Rewrite failed in balanced!")
            }
        )
    }

    registerS3method("shard_share_hook", "RewriteErrorBalanced", shard_share_hook.RewriteErrorBalanced)

    obj <- new("RewriteErrorBalanced", data = rnorm(10000))

    # Should not error
    shared <- share(obj, deep = TRUE, min_bytes = 1000, mode = "balanced")

    expect_s3_class(shared, "shard_deep_shared")

    # Rewrite error should be tracked
    expect_true(shared$summary$hook_error_count > 0)

    # Original data should still be present
    recovered <- fetch(shared)
    expect_equal(recovered@data, obj@data)

    close(shared)

    rm(shard_share_hook.RewriteErrorBalanced, envir = globalenv())
    removeClass("RewriteErrorBalanced")
})

test_that("S4 object with list slot shares list contents", {
    setClass("S4WithList", slots = c(
        matrix_data = "matrix",
        list_data = "list"
    ))

    obj <- new("S4WithList",
               matrix_data = matrix(rnorm(10000), 100, 100),
               list_data = list(a = rnorm(10000), b = rnorm(10000)))

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # Matrix and both list elements should be shared
    expect_equal(shared$summary$shared_count, 3)

    recovered <- fetch(shared)
    expect_equal(recovered@matrix_data, obj@matrix_data)
    expect_equal(recovered@list_data$a, obj@list_data$a)
    expect_equal(recovered@list_data$b, obj@list_data$b)

    close(shared)
    removeClass("S4WithList")
})
