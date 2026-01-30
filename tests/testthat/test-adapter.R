# Tests for adapter registry and class-specific traversal

test_that("shard_register_adapter registers adapter correctly", {
    # Clean up any existing adapters
    shard_clear_adapters()

    adapter <- list(
        class = "TestClass",
        children = function(x) list(data = x$data),
        replace = function(x, children) {
            x$data <- children$data
            x
        }
    )

    result <- shard_register_adapter("TestClass", adapter)

    # First registration returns NULL
    expect_null(result)

    # Check adapter is registered
    expect_true("TestClass" %in% shard_list_adapters())

    # Clean up
    shard_clear_adapters()
})

test_that("shard_register_adapter returns previous adapter on re-registration", {
    shard_clear_adapters()

    adapter1 <- list(
        class = "TestClass",
        children = function(x) list(a = 1),
        replace = function(x, children) x
    )

    adapter2 <- list(
        class = "TestClass",
        children = function(x) list(b = 2),
        replace = function(x, children) x
    )

    shard_register_adapter("TestClass", adapter1)
    previous <- shard_register_adapter("TestClass", adapter2)

    expect_identical(previous$class, "TestClass")
    expect_identical(previous$children(NULL), list(a = 1))

    shard_clear_adapters()
})

test_that("shard_unregister_adapter removes adapter and returns it", {
    shard_clear_adapters()

    adapter <- list(
        class = "TestClass",
        children = function(x) list(),
        replace = function(x, children) x
    )

    shard_register_adapter("TestClass", adapter)
    expect_true("TestClass" %in% shard_list_adapters())

    removed <- shard_unregister_adapter("TestClass")

    expect_identical(removed$class, "TestClass")
    expect_false("TestClass" %in% shard_list_adapters())

    shard_clear_adapters()
})

test_that("shard_unregister_adapter returns NULL for non-existent adapter", {
    shard_clear_adapters()

    result <- shard_unregister_adapter("NonExistentClass")
    expect_null(result)
})

test_that("shard_get_adapter returns adapter for object's class", {
    shard_clear_adapters()

    adapter <- list(
        class = "myclass",
        children = function(x) list(val = x$value),
        replace = function(x, children) {
            x$value <- children$val
            x
        }
    )

    shard_register_adapter("myclass", adapter)

    obj <- structure(list(value = 42), class = "myclass")
    found <- shard_get_adapter(obj)

    expect_identical(found$class, "myclass")

    shard_clear_adapters()
})

test_that("shard_get_adapter returns NULL for unregistered class", {
    shard_clear_adapters()

    obj <- structure(list(value = 42), class = "unregistered")
    found <- shard_get_adapter(obj)

    expect_null(found)
})

test_that("shard_get_adapter checks class hierarchy", {
    shard_clear_adapters()

    adapter <- list(
        class = "parentclass",
        children = function(x) list(val = x$value),
        replace = function(x, children) {
            x$value <- children$val
            x
        }
    )

    shard_register_adapter("parentclass", adapter)

    # Object with multiple classes - parentclass is second
    obj <- structure(list(value = 42), class = c("childclass", "parentclass"))
    found <- shard_get_adapter(obj)

    expect_identical(found$class, "parentclass")

    shard_clear_adapters()
})

test_that("adapter validation rejects invalid adapters", {
    shard_clear_adapters()

    # Missing class
    expect_error(
        shard_register_adapter("Test", list(
            children = function(x) list(),
            replace = function(x, ch) x
        )),
        "class"
    )

    # Class mismatch
    expect_error(
        shard_register_adapter("Test", list(
            class = "Other",
            children = function(x) list(),
            replace = function(x, ch) x
        )),
        "does not match"
    )

    # Missing children
    expect_error(
        shard_register_adapter("Test", list(
            class = "Test",
            replace = function(x, ch) x
        )),
        "children"
    )

    # Missing replace
    expect_error(
        shard_register_adapter("Test", list(
            class = "Test",
            children = function(x) list()
        )),
        "replace"
    )

    # Invalid path_prefix
    expect_error(
        shard_register_adapter("Test", list(
            class = "Test",
            children = function(x) list(),
            replace = function(x, ch) x,
            path_prefix = c("a", "b")  # Should be single string
        )),
        "path_prefix"
    )
})

test_that("deep share uses adapter's children function", {
    shard_clear_adapters()

    # Create a simple S3 class with internal structure
    create_myobj <- function(data, meta) {
        structure(list(data = data, meta = meta), class = "myobj")
    }

    # Register adapter that only exposes 'data'
    shard_register_adapter("myobj", list(
        class = "myobj",
        children = function(x) list(data = x$data),
        replace = function(x, children) {
            x$data <- children$data
            x
        }
    ))

    big_data <- rnorm(10000)
    obj <- create_myobj(data = big_data, meta = list(name = "test"))

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")

    # Only the data should be shared (meta is not exposed by adapter)
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered$data, big_data)
    expect_equal(recovered$meta, list(name = "test"))  # meta preserved

    close(shared)
    shard_clear_adapters()
})

test_that("deep share uses adapter's replace function for reconstruction", {
    shard_clear_adapters()

    # Track calls to replace
    replace_called <- FALSE

    shard_register_adapter("tracked", list(
        class = "tracked",
        children = function(x) list(val = x$value),
        replace = function(x, children) {
            replace_called <<- TRUE
            x$value <- children$val
            x$reconstructed <- TRUE
            x
        }
    ))

    obj <- structure(list(value = rnorm(10000)), class = "tracked")
    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    recovered <- fetch(shared)

    expect_true(replace_called)
    expect_true(recovered$reconstructed)

    close(shared)
    shard_clear_adapters()
})

test_that("adapter with path_prefix uses custom path", {
    shard_clear_adapters()

    shard_register_adapter("prefixed", list(
        class = "prefixed",
        children = function(x) list(data = x$data),
        replace = function(x, children) {
            x$data <- children$data
            x
        },
        path_prefix = "/custom"
    ))

    obj <- structure(list(data = rnorm(10000)), class = "prefixed")
    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    # Check that the path includes the custom prefix
    expect_true(grepl("/custom", shared$tree$children$data$path))

    close(shared)
    shard_clear_adapters()
})

test_that("no adapter falls back to S4 slot traversal", {
    skip_if_not_installed("methods")
    shard_clear_adapters()

    # Define a simple S4 class
    setClass("TestS4", slots = c(mat = "matrix", info = "character"))

    big_mat <- matrix(rnorm(10000), nrow = 100)
    obj <- new("TestS4", mat = big_mat, info = "test")

    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    expect_s3_class(shared, "shard_deep_shared")
    expect_equal(shared$summary$shared_count, 1)  # matrix should be shared

    recovered <- fetch(shared)
    expect_s4_class(recovered, "TestS4")
    expect_equal(recovered@mat, big_mat)
    expect_equal(recovered@info, "test")

    close(shared)
    removeClass("TestS4")
})

test_that("S4 adapter takes precedence over generic slot traversal", {
    skip_if_not_installed("methods")
    shard_clear_adapters()

    setClass("AdaptedS4", slots = c(data = "numeric", private = "numeric"))

    # Adapter only exposes 'data', not 'private'
    shard_register_adapter("AdaptedS4", list(
        class = "AdaptedS4",
        children = function(x) list(data = x@data),
        replace = function(x, children) {
            x@data <- children$data
            x
        }
    ))

    obj <- new("AdaptedS4", data = rnorm(10000), private = 1:100)
    shared <- share(obj, deep = TRUE, min_bytes = 1000)

    # Only data should be traversed (via adapter), not private
    expect_equal(shared$summary$shared_count, 1)

    recovered <- fetch(shared)
    expect_equal(recovered@data, obj@data)
    expect_equal(recovered@private, 1:100)  # preserved unchanged

    close(shared)
    shard_clear_adapters()
    removeClass("AdaptedS4")
})

test_that("shard_list_adapters returns registered classes", {
    shard_clear_adapters()

    expect_length(shard_list_adapters(), 0)

    shard_register_adapter("ClassA", list(
        class = "ClassA",
        children = function(x) list(),
        replace = function(x, ch) x
    ))

    shard_register_adapter("ClassB", list(
        class = "ClassB",
        children = function(x) list(),
        replace = function(x, ch) x
    ))

    adapters <- shard_list_adapters()
    expect_length(adapters, 2)
    expect_true("ClassA" %in% adapters)
    expect_true("ClassB" %in% adapters)

    shard_clear_adapters()
})

test_that("shard_clear_adapters removes all adapters", {
    shard_clear_adapters()

    shard_register_adapter("A", list(class = "A", children = function(x) list(), replace = function(x, ch) x))
    shard_register_adapter("B", list(class = "B", children = function(x) list(), replace = function(x, ch) x))
    shard_register_adapter("C", list(class = "C", children = function(x) list(), replace = function(x, ch) x))

    expect_length(shard_list_adapters(), 3)

    n <- shard_clear_adapters()

    expect_equal(n, 3)
    expect_length(shard_list_adapters(), 0)
})
