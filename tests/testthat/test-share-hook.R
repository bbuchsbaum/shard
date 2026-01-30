# Tests for deep sharing hook system

.shard_test_register_hook <- function(class, fun) {
    nm <- paste0("shard_share_hook.", class)
    # S3 method lookup for UseMethod() will find methods in the global env,
    # and we avoid mutating the (locked) package namespace.
    assign(nm, fun, envir = globalenv())
    nm
}

.shard_test_unregister_hook <- function(name) {
    if (exists(name, envir = globalenv(), inherits = FALSE)) {
        rm(list = name, envir = globalenv())
    }
    invisible(NULL)
}

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
    hook_name <- .shard_test_register_hook("SkipPathTest", function(x, ctx) {
        list(skip_paths = paste0(ctx$path, "@cache"))
    })

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
    .shard_test_unregister_hook(hook_name)
    removeClass("SkipPathTest")
})

test_that("hook with skip_slots prevents slot traversal for S4 objects", {
    # Define S4 class
    setClass("CacheModel", slots = c(
        coefficients = "numeric",
        cache = "list"
    ))

    # Hook that skips cache slot
    hook_name <- .shard_test_register_hook("CacheModel", function(x, ctx) {
        list(skip_slots = "cache")
    })

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

    .shard_test_unregister_hook(hook_name)
    removeClass("CacheModel")
})

test_that("hook with force_share_paths shares small objects", {
    # Define class that forces sharing of small data
    setClass("ForceShareTest", slots = c(
        important_small = "numeric",
        unimportant_large = "numeric"
    ))

    hook_name <- .shard_test_register_hook("ForceShareTest", function(x, ctx) {
        list(force_share_paths = paste0(ctx$path, "@important_small"))
    })

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

    .shard_test_unregister_hook(hook_name)
    removeClass("ForceShareTest")
})

test_that("hook with rewrite transforms object before sharing", {
    # Class that has lazy data needing materialization
    setClass("LazyData", slots = c(data = "ANY", materialized = "logical"))

    hook_name <- .shard_test_register_hook("LazyData", function(x, ctx) {
        list(
            rewrite = function(obj) {
                # Transform the object - e.g., materialize lazy data
                obj@materialized <- TRUE
                obj
            }
        )
    })

    obj <- new("LazyData", data = rnorm(10000), materialized = FALSE)

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    recovered <- fetch(shared)
    # The rewrite function should have set materialized to TRUE
    expect_true(recovered@materialized)
    expect_equal(recovered@data, obj@data)

    close(shared)

    .shard_test_unregister_hook(hook_name)
    removeClass("LazyData")
})

test_that("hook errors in strict mode propagate", {
    setClass("ErrorHook", slots = c(data = "numeric"))

    hook_name <- .shard_test_register_hook("ErrorHook", function(x, ctx) {
        stop("Deliberate hook error")
    })

    obj <- new("ErrorHook", data = rnorm(10000))

    expect_error(
        share(obj, deep = TRUE, min_bytes = 1000, mode = "strict"),
        "Hook error in strict mode"
    )

    .shard_test_unregister_hook(hook_name)
    removeClass("ErrorHook")
})

test_that("hook errors in balanced mode continue and track error", {
    setClass("ErrorHookBalanced", slots = c(data = "numeric"))

    hook_name <- .shard_test_register_hook("ErrorHookBalanced", function(x, ctx) {
        stop("Deliberate hook error for balanced mode")
    })

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

    .shard_test_unregister_hook(hook_name)
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
    hook_name <- .shard_test_register_hook("MyCustomClass", function(x, ctx) {
        # Force share small_data even though it's tiny
        list(force_share_paths = paste0(ctx$path, "$small_data"))
    })

    # With very high threshold, normally nothing would be shared
    shared <- share(my_object, deep = TRUE, min_bytes = 1000000)

    # But hook forced small_data to be shared
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered$large_data, my_object$large_data)
    expect_equal(recovered$small_data, my_object$small_data)

    close(shared)

    .shard_test_unregister_hook(hook_name)
})

test_that("hook context contains correct information", {
    captured_ctx <- NULL

    setClass("ContextCapture", slots = c(data = "numeric"))

    hook_name <- .shard_test_register_hook("ContextCapture", function(x, ctx) {
        captured_ctx <<- ctx
        list()
    })

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

    .shard_test_unregister_hook(hook_name)
    removeClass("ContextCapture")
})

test_that("hook rewrite error in strict mode propagates", {
    setClass("RewriteError", slots = c(data = "numeric"))

    hook_name <- .shard_test_register_hook("RewriteError", function(x, ctx) {
        list(
            rewrite = function(obj) {
                stop("Rewrite failed!")
            }
        )
    })

    obj <- new("RewriteError", data = rnorm(100))

    expect_error(
        share(obj, deep = TRUE, min_bytes = 1000, mode = "strict"),
        "Rewrite function error"
    )

    .shard_test_unregister_hook(hook_name)
    removeClass("RewriteError")
})

test_that("hook rewrite error in balanced mode continues", {
    setClass("RewriteErrorBalanced", slots = c(data = "numeric"))

    hook_name <- .shard_test_register_hook("RewriteErrorBalanced", function(x, ctx) {
        list(
            rewrite = function(obj) {
                stop("Rewrite failed in balanced!")
            }
        )
    })

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

    .shard_test_unregister_hook(hook_name)
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
