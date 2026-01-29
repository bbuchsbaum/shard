# Tests for arena() - semantic scope for scratch memory

test_that("arena evaluates expression and returns result", {
    result <- arena({
        x <- 1:10
        sum(x)
    })

    expect_equal(result, 55)
})

test_that("arena evaluates in parent environment", {
    y <- 100
    result <- arena({
        y + 1
    })

    expect_equal(result, 101)
})

test_that("arena can modify parent environment variables via assign", {
    env <- environment()
    z <- 0
    arena({
        assign("z", 42, envir = env)
    })

    expect_equal(z, 42)
})

test_that("in_arena returns FALSE outside arena", {
    expect_false(in_arena())
})

test_that("in_arena returns TRUE inside arena", {
    result <- arena({
        in_arena()
    })

    expect_true(result)
})

test_that("arena_depth is 0 outside arena", {
    expect_equal(arena_depth(), 0)
})

test_that("arena supports nesting with correct depth", {
    result <- arena({
        depth1 <- arena_depth()
        depth2 <- arena({
            arena_depth()
        })
        c(depth1, depth2)
    })

    expect_equal(result[1], 1)  # Outer arena
    expect_equal(result[2], 2)  # Inner arena
})

test_that("arena with diagnostics returns arena_result", {
    result <- arena({
        sum(1:100)
    }, diagnostics = TRUE)

    expect_s3_class(result, "arena_result")
    expect_equal(result$result, 5050)
    expect_true(!is.null(result$diagnostics))
    expect_true(!is.null(result$diagnostics$arena_id))
})

test_that("arena diagnostics includes result size", {
    result <- arena({
        rnorm(100)
    }, diagnostics = TRUE)

    expect_true(!is.na(result$diagnostics$result_size))
    expect_true(result$diagnostics$result_size > 0)
})

test_that("arena strict mode warns on large escaping objects", {
    # Create a large object that escapes
    expect_warning(
        arena({
            # Create ~1MB+ object
            rnorm(200000)  # ~1.6MB
        }, strict = TRUE, escape_threshold = 1024 * 1024),
        "Large object escaping"
    )
})

test_that("arena strict mode does not warn on small objects", {
    expect_silent(
        arena({
            1:10  # Tiny object
        }, strict = TRUE)
    )
})

test_that("arena gc_after triggers garbage collection", {
    # Hard to test GC directly, but at least verify it doesn't error
    expect_no_error(
        arena({
            x <- rnorm(1000)
            sum(x)
        }, gc_after = TRUE)
    )
})

test_that("arena handles errors in expression", {
    expect_error(
        arena({
            stop("test error")
        }),
        "test error"
    )

    # Stack should be clean after error
    expect_equal(arena_depth(), 0)
})

test_that("arena handles warnings in expression", {
    expect_warning(
        result <- arena({
            warning("test warning")
            42
        }),
        "test warning"
    )

    expect_equal(result, 42)
})

test_that("arena can return NULL", {
    result <- arena({
        NULL
    })

    expect_null(result)
})

test_that("arena can return complex objects", {
    result <- arena({
        list(
            a = 1:10,
            b = matrix(1:6, nrow = 2),
            c = data.frame(x = 1:3, y = letters[1:3])
        )
    })

    expect_true(is.list(result))
    expect_equal(names(result), c("a", "b", "c"))
    expect_equal(result$a, 1:10)
})

test_that("print.arena_result works", {
    result <- arena({
        sum(1:10)
    }, diagnostics = TRUE)

    expect_output(print(result), "arena_result")
    expect_output(print(result), "Arena ID:")
    expect_output(print(result), "Result:")
})

test_that("arena stack is properly maintained across multiple calls", {
    # Run multiple arenas sequentially
    for (i in 1:5) {
        result <- arena({
            arena_depth()
        })
        expect_equal(result, 1)
    }

    # Stack should be empty after all
    expect_equal(arena_depth(), 0)
})

test_that("arena respects custom escape_threshold", {
    # Should warn with very low threshold
    expect_warning(
        arena({
            1:100  # ~400 bytes
        }, strict = TRUE, escape_threshold = 100),
        "Large object escaping"
    )

    # Should not warn with high threshold
    expect_silent(
        arena({
            rnorm(1000)  # ~8KB
        }, strict = TRUE, escape_threshold = 10 * 1024 * 1024)
    )
})

test_that("arena works with zero-length results", {
    result <- arena({
        integer(0)
    })

    expect_equal(result, integer(0))
})

test_that("arena diagnostics tracks RSS when available", {
    result <- arena({
        x <- rnorm(10000)
        sum(x)
    }, diagnostics = TRUE)

    # RSS tracking may not be available on all platforms
    # Just check the fields exist
    diag <- result$diagnostics
    expect_true("rss_before" %in% names(diag))
    expect_true("rss_after" %in% names(diag))
    expect_true("rss_delta" %in% names(diag))
})

test_that("nested arenas track depth correctly", {
    depths <- arena({
        d1 <- arena_depth()
        d2 <- arena({
            arena_depth()
        })
        d3 <- arena({
            arena({
                arena_depth()
            })
        })
        list(outer = d1, inner = d2, double_nested = d3)
    })

    expect_equal(depths$outer, 1)
    expect_equal(depths$inner, 2)
    expect_equal(depths$double_nested, 3)
})
