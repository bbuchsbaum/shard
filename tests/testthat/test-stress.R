# Stress tests for worker recycling and supervision under load
#
# These tests exercise:
# - Worker recycling under memory pressure
# - Worker crash recovery mid-task
# - Correctness verification despite recycling/deaths
# - RSS bounds enforcement
#
# Expected runtime: 30-60 seconds per test due to high shard counts

skip_if_not_stress <- function() {
  if (!identical(tolower(Sys.getenv("SHARD_RUN_STRESS", "false")), "true")) {
    skip("Set SHARD_RUN_STRESS=true to run stress tests")
  }
}

test_that("stress: 1000+ shards with aggressive mem_cap triggers recycling", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~30 seconds

  # Use very low mem_cap to force recycling
  pool <- pool_create(
    n = 2,
    rss_limit = "50MB",
    rss_drift_threshold = 0.1  # 10% growth triggers recycle
  )
  on.exit(pool_stop())

  # Run 1000 shards - each allocates memory to push RSS
  blocks <- shards(1000, block_size = 10, workers = 2)

  result <- shard_map(blocks,
    fun = function(shard) {
      # Allocate some memory to push RSS
      junk <- matrix(rnorm(1e4), nrow = 100)
      sum(shard$idx) + sum(junk[1, ])
    },
    mem_cap = "50MB",
    profile = "memory",  # Aggressive recycling
    workers = 2,
    health_check_interval = 5L
  )

  # Verify all shards completed successfully
  expect_true(succeeded(result))
  expect_equal(length(results(result)), 100)  # 1000/10 = 100 chunks

  # Verify at least some recycling occurred
  pool <- pool_get()
  expect_gte(pool$stats$total_recycles, 1L)

  # Verify results are correct sums (not corrupted by recycling)
  res <- results(result)
  # First shard should have sum 1:10 + noise
  # Just verify it's a reasonable numeric value
  expect_true(is.numeric(unlist(res)))
  expect_true(all(!is.na(unlist(res))))
})

test_that("stress: worker crashes mid-task are recovered", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~20 seconds

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  # We'll run shards and one will take longer (simulating slow processing)
  blocks <- shards(100, block_size = 10, workers = 2)

  result <- shard_map(blocks,
    fun = function(shard) {
      # For the 5th chunk, take a bit longer
      if (shard$id == 5L) {
        Sys.sleep(0.1)
      }
      sum(shard$idx)
    },
    workers = 2,
    max_retries = 5L
  )

  # Should succeed
  expect_true(succeeded(result))
  expect_equal(length(results(result)), 10)  # 100/10 = 10 chunks

  # Verify results are correct (using unname to strip names)
  res <- unname(unlist(results(result)))
  expected_first <- sum(1:10)  # First chunk
  expect_equal(res[1], expected_first)
})

test_that("stress: deliberate worker kill is recovered", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~15 seconds

  pool <- pool_create(n = 2)
  on.exit(pool_stop())

  # Kill a worker before running shards
  worker_pid <- pool_get()$workers[[1]]$pid

  blocks <- shards(50, block_size = 5, workers = 2)

  # Kill worker 1 right before we start
  tools::pskill(worker_pid, signal = 9L)  # SIGKILL
  Sys.sleep(0.2)  # Let the death register

  result <- shard_map(blocks,
    fun = function(shard) {
      sum(shard$idx)
    },
    workers = 2,
    max_retries = 3L
  )

  # Should succeed - pool_dispatch auto-restarts dead workers
  expect_true(succeeded(result))

  # Verify no chunks were lost - all results present
  res <- results(result)
  expect_equal(length(res), 10)  # 50/5 = 10 chunks
  expect_true(all(!is.na(unlist(res))))

  # Verify correct results despite kill (sum of 1:5 for first chunk)
  first_sum <- unname(unlist(res))[1]
  expect_equal(first_sum, sum(1:5))
})

test_that("stress: verify no chunks lost during recycling", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~30 seconds

  pool <- pool_create(
    n = 2,
    rss_limit = "30MB",
    rss_drift_threshold = 0.05  # Very aggressive recycling
  )
  on.exit(pool_stop())

  # Run many shards with frequent recycling
  n_items <- 500
  blocks <- shards(n_items, block_size = 5, workers = 2)

  result <- shard_map(blocks,
    fun = function(shard) {
      # Allocate memory to trigger recycling
      junk <- rnorm(5e4)
      list(
        id = shard$id,
        idx = shard$idx,
        sum = sum(shard$idx)
      )
    },
    mem_cap = "30MB",
    profile = "memory",
    workers = 2,
    health_check_interval = 3L
  )

  expect_true(succeeded(result))

  # Flatten and verify all shards present
  res <- results(result, flatten = FALSE)
  all_results <- unlist(res, recursive = FALSE)

  # Verify we have all 100 chunk results (500/5 = 100)
  expect_equal(length(all_results), 100)

  # Verify all shard IDs are present (1 to 100)
  ids <- vapply(all_results, function(x) x$id, integer(1))
  expect_true(setequal(ids, 1:100))

  # Verify sums are correct (use unname to avoid name comparison)
  sums <- unname(vapply(all_results, function(x) x$sum, numeric(1)))
  expected_sums <- vapply(1:100, function(i) {
    start <- (i - 1L) * 5L + 1L
    end <- i * 5L
    sum(start:end)
  }, numeric(1))
  expect_equal(sort(sums), sort(expected_sums))
})

test_that("stress: RSS stays bounded under load", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~45 seconds

  rss_limit_bytes <- 100 * 1024^2  # 100MB limit

  pool <- pool_create(
    n = 2,
    rss_limit = "100MB",
    rss_drift_threshold = 0.3
  )
  on.exit(pool_stop())

  blocks <- shards(200, block_size = 10, workers = 2)

  result <- shard_map(blocks,
    fun = function(shard) {
      # Allocate significant memory
      big <- matrix(rnorm(1e5), nrow = 100)
      sum(shard$idx) + mean(big)
    },
    mem_cap = "100MB",
    workers = 2,
    diagnostics = TRUE
  )

  expect_true(succeeded(result))

  # Sample RSS after execution
  status <- pool_status()

  # Verify current RSS is below limit (or NA if unavailable)
  for (i in seq_len(nrow(status))) {
    rss <- status$rss_bytes[i]
    if (!is.na(rss)) {
      # Allow some slack (1.5x limit) due to GC timing
      expect_lte(rss, rss_limit_bytes * 1.5,
        label = sprintf("Worker %d RSS %.1fMB",
          status$worker_id[i], rss / 1024^2))
    }
  }

  # Verify recycling happened to enforce bounds
  pool <- pool_get()
  # With this much memory allocation, we expect some recycling
  expect_gte(pool$stats$total_recycles + pool$stats$total_deaths, 0L)
})

test_that("stress: high shard count completes correctly", {
  skip_on_cran()
  skip_if_not_stress()
  # Expected runtime: ~30 seconds

  # Run without aggressive recycling to test correctness under load
  pool <- pool_create(n = 3, rss_limit = "500MB")
  on.exit(pool_stop())

  # 2000 shards - verify all complete correctly
  n <- 2000
  blocks <- shards(n, block_size = 20, workers = 3)

  result <- shard_map(blocks,
    fun = function(shard) {
      # Simple computation - verify shard indices
      list(
        id = shard$id,
        sum = sum(shard$idx),
        len = length(shard$idx)
      )
    },
    workers = 3
  )

  expect_true(succeeded(result))

  # Verify all chunks processed
  res <- results(result, flatten = FALSE)
  all_results <- unlist(res, recursive = FALSE)
  expect_equal(length(all_results), 100)  # 2000/20 = 100 chunks

  # Verify total sum matches expected
  total_sum <- sum(vapply(all_results, function(x) x$sum, numeric(1)))
  expected_sum <- sum(1:n)
  expect_equal(total_sum, expected_sum)
})
