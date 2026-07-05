#!/usr/bin/env Rscript
# =============================================================================
# Benchmark: current shard checkout vs CRAN shard
# =============================================================================
#
# Installs the CRAN release and the current checkout into isolated temporary
# libraries, then runs the same benchmark cases against each install in child
# Rscript processes. This avoids compiled-namespace unload/reload issues and
# prevents accidental writes to the user's normal R library.
#
# Run from the package root:
#   Rscript inst/bench/current_vs_cran.R
#
# Common options:
#   --profile=smoke|standard       Dataset sizes; default: smoke
#   --reps=N                       Timed repetitions per backend/benchmark
#   --workers=N                    shard workers; default: min(4, cores)
#   --timeout=SECONDS              Timeout per backend/benchmark child process
#   --bench=a,b,c                  Benchmark subset
#   --out=/path/results.csv        Combined CSV output path
#   --root=/path/run-dir           Temp run directory to reuse/inspect
#   --cran-type=source|binary      CRAN install type; default: source
#   --skip-install                 Reuse --cran-lib and --current-lib
#   --cran-lib=/path/lib           Existing CRAN library for --skip-install
#   --current-lib=/path/lib        Existing current library for --skip-install
#
# Environment variables with the same intent are also supported:
#   SHARD_BENCH_PROFILE, SHARD_BENCH_REPS, SHARD_BENCH_WORKERS,
#   SHARD_BENCH_TIMEOUT, SHARD_BENCH_ONLY, SHARD_BENCH_OUT,
#   SHARD_BENCH_ROOT, SHARD_BENCH_CRAN_TYPE, SHARD_BENCH_CRAN_REPOS
#
# =============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

is_true <- function(x) {
  tolower(trimws(x %||% "")) %in% c("1", "true", "yes", "y", "on")
}

timestamp <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

parse_args <- function(args) {
  out <- list()
  for (arg in args) {
    if (!startsWith(arg, "--")) {
      stop("Unknown positional argument: ", arg, call. = FALSE)
    }
    x <- substring(arg, 3L)
    if (grepl("=", x, fixed = TRUE)) {
      parts <- strsplit(x, "=", fixed = TRUE)[[1L]]
      key <- parts[[1L]]
      val <- paste(parts[-1L], collapse = "=")
      out[[key]] <- val
    } else {
      out[[x]] <- TRUE
    }
  }
  out
}

cfg <- parse_args(commandArgs(trailingOnly = TRUE))

repo_root <- normalizePath(getwd(), mustWork = TRUE)
desc <- file.path(repo_root, "DESCRIPTION")
if (!file.exists(desc)) {
  stop("Run this script from the shard package root.", call. = FALSE)
}
d <- read.dcf(desc)
if (!identical(unname(d[1L, "Package"]), "shard")) {
  stop("DESCRIPTION does not describe the shard package.", call. = FALSE)
}

profile <- cfg[["profile"]] %||% Sys.getenv("SHARD_BENCH_PROFILE", "smoke")
profile <- match.arg(profile, c("smoke", "standard"))

workers <- as.integer(cfg[["workers"]] %||% Sys.getenv("SHARD_BENCH_WORKERS", ""))
if (is.na(workers)) {
  workers <- min(4L, max(1L, parallel::detectCores(logical = FALSE) %||% 2L))
}
if (workers < 1L) stop("--workers must be >= 1", call. = FALSE)

reps <- as.integer(cfg[["reps"]] %||% Sys.getenv("SHARD_BENCH_REPS", "3"))
if (is.na(reps) || reps < 1L) stop("--reps must be >= 1", call. = FALSE)

timeout <- as.integer(cfg[["timeout"]] %||% Sys.getenv("SHARD_BENCH_TIMEOUT", "180"))
if (is.na(timeout) || timeout < 1L) stop("--timeout must be >= 1", call. = FALSE)

cran_type <- cfg[["cran-type"]] %||% Sys.getenv("SHARD_BENCH_CRAN_TYPE", "source")
cran_type <- match.arg(cran_type, c("source", "binary"))
cran_repos <- Sys.getenv("SHARD_BENCH_CRAN_REPOS", "https://cloud.r-project.org")

root <- cfg[["root"]] %||% Sys.getenv("SHARD_BENCH_ROOT", "")
if (!nzchar(root)) {
  root <- file.path(tempdir(), paste0("shard-current-vs-cran-", timestamp()))
}
root <- normalizePath(root, mustWork = FALSE)
dir.create(root, recursive = TRUE, showWarnings = FALSE)

logs_dir <- file.path(root, "logs")
partials_dir <- file.path(root, "partials")
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(partials_dir, recursive = TRUE, showWarnings = FALSE)

out_path <- cfg[["out"]] %||% Sys.getenv("SHARD_BENCH_OUT", "")
if (!nzchar(out_path)) {
  out_path <- file.path(root, "current_vs_cran_results.csv")
}
out_path <- normalizePath(out_path, mustWork = FALSE)

cran_lib <- cfg[["cran-lib"]] %||% file.path(root, "cran-lib")
current_lib <- cfg[["current-lib"]] %||% file.path(root, "current-lib")
cran_lib <- normalizePath(cran_lib, mustWork = FALSE)
current_lib <- normalizePath(current_lib, mustWork = FALSE)

skip_install <- isTRUE(cfg[["skip-install"]]) || is_true(Sys.getenv("SHARD_BENCH_SKIP_INSTALL", ""))

all_benchmarks <- c(
  "rpc_tiny_hot",
  "rpc_payload_probe",
  "shm_queue_tiny_hot",
  "view_xTy_block_hot",
  "view_xTy_gather_hot",
  "col_vars_kernel_hot",
  "shard_crossprod_tiled_hot"
)

bench_raw <- cfg[["bench"]] %||% Sys.getenv("SHARD_BENCH_ONLY", "")
benchmarks <- if (nzchar(trimws(bench_raw))) {
  trimws(strsplit(bench_raw, ",", fixed = TRUE)[[1L]])
} else {
  all_benchmarks
}
bad_bench <- setdiff(benchmarks, all_benchmarks)
if (length(bad_bench) > 0L) {
  stop("Unknown benchmark(s): ", paste(bad_bench, collapse = ", "),
       "\nKnown benchmarks: ", paste(all_benchmarks, collapse = ", "),
       call. = FALSE)
}

cat("=============================================================================\n")
cat("Benchmark: current shard checkout vs CRAN shard\n")
cat("=============================================================================\n")
cat("Repo root:     ", repo_root, "\n", sep = "")
cat("Run dir:       ", root, "\n", sep = "")
cat("Output CSV:    ", out_path, "\n", sep = "")
cat("Profile:       ", profile, "\n", sep = "")
cat("Workers:       ", workers, "\n", sep = "")
cat("Reps:          ", reps, "\n", sep = "")
cat("Timeout:       ", timeout, " seconds per backend/benchmark\n", sep = "")
cat("Benchmarks:    ", paste(benchmarks, collapse = ", "), "\n", sep = "")
cat("CRAN repos:    ", cran_repos, "\n", sep = "")
cat("CRAN type:     ", cran_type, "\n\n", sep = "")

install_libs <- function() {
  dir.create(cran_lib, recursive = TRUE, showWarnings = FALSE)
  dir.create(current_lib, recursive = TRUE, showWarnings = FALSE)

  cat("Installing CRAN shard into ", cran_lib, "...\n", sep = "")
  cran_install_out <- file.path(logs_dir, "install-cran.out")
  cran_install_err <- file.path(logs_dir, "install-cran.err")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      "-e",
      sprintf(
        paste0(
          "options(repos = c(CRAN = %s)); ",
          "install.packages('shard', lib = %s, type = %s, quiet = TRUE)"
        ),
        deparse(cran_repos),
        deparse(cran_lib),
        deparse(cran_type)
      )
    ),
    stdout = cran_install_out,
    stderr = cran_install_err
  )
  if (!identical(status, 0L)) {
    stop("CRAN install failed; see ", cran_install_out, " and ", cran_install_err, call. = FALSE)
  }

  cat("Installing current checkout into ", current_lib, "...\n", sep = "")
  current_install_out <- file.path(logs_dir, "install-current.out")
  current_install_err <- file.path(logs_dir, "install-current.err")
  status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--preclean", "-l", current_lib, repo_root),
    stdout = current_install_out,
    stderr = current_install_err
  )
  if (!identical(status, 0L)) {
    stop("Current checkout install failed; see ", current_install_out, " and ", current_install_err, call. = FALSE)
  }
}

if (skip_install) {
  cat("Skipping installation; reusing provided libraries.\n")
  if (!dir.exists(file.path(cran_lib, "shard"))) {
    stop("--skip-install requires --cran-lib containing shard.", call. = FALSE)
  }
  if (!dir.exists(file.path(current_lib, "shard"))) {
    stop("--skip-install requires --current-lib containing shard.", call. = FALSE)
  }
} else {
  install_libs()
}

runner_path <- file.path(root, "current_vs_cran_runner.R")
runner_code <- '
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 7L) {
  stop("usage: runner lib label benchmark reps workers profile out_csv", call. = FALSE)
}
lib <- args[[1L]]
label <- args[[2L]]
benchmark <- args[[3L]]
reps <- as.integer(args[[4L]])
workers <- as.integer(args[[5L]])
profile <- args[[6L]]
out_csv <- args[[7L]]

.libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages(library(shard))

sizes <- switch(
  profile,
  smoke = list(
    rpc_n = 1000L,
    shm_n = 2000L,
    view_n = 1200L,
    view_p = 16L,
    view_v = 64L,
    block_size = 16L,
    gather_tasks = 40L,
    gather_width = 24L,
    vars_n = 1200L,
    vars_p = 64L,
    cross_n = 1000L,
    cross_p = 32L,
    cross_v = 64L,
    cross_block_x = 16L,
    cross_block_y = 32L
  ),
  standard = list(
    rpc_n = 3000L,
    shm_n = 8000L,
    view_n = 3000L,
    view_p = 24L,
    view_v = 192L,
    block_size = 32L,
    gather_tasks = 120L,
    gather_width = 48L,
    vars_n = 4000L,
    vars_p = 192L,
    cross_n = 2500L,
    cross_p = 64L,
    cross_v = 128L,
    cross_block_x = 32L,
    cross_block_y = 64L
  ),
  stop("unknown profile", call. = FALSE)
)

result_row <- function(rep, elapsed_s, status = "ok", error = "",
                       tasks_total = NA_integer_,
                       tasks_per_sec = NA_real_,
                       payload_mean_bytes = NA_real_,
                       payload_median_bytes = NA_real_,
                       payload_min_bytes = NA_real_,
                       payload_max_bytes = NA_real_,
                       materialized_mb = NA_real_,
                       notes = "") {
  data.frame(
    label = label,
    version = as.character(utils::packageVersion("shard")),
    lib = find.package("shard"),
    benchmark = benchmark,
    profile = profile,
    workers = workers,
    rep = rep,
    elapsed_s = elapsed_s,
    status = status,
    error = error,
    tasks_total = tasks_total,
    tasks_per_sec = tasks_per_sec,
    payload_mean_bytes = payload_mean_bytes,
    payload_median_bytes = payload_median_bytes,
    payload_min_bytes = payload_min_bytes,
    payload_max_bytes = payload_max_bytes,
    materialized_mb = materialized_mb,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

cleanup <- function() {
  suppressWarnings({
    tryCatch(pool_stop(), error = function(e) NULL)
    gc(verbose = FALSE)
  })
}
on.exit(cleanup(), add = TRUE)

timed_rep <- function(rep, fn, tasks_total = NA_integer_, notes = "", hot_pool = TRUE) {
  cleanup()
  gc(verbose = FALSE)
  if (isTRUE(hot_pool)) pool_create(workers)
  t <- try(system.time(fn())[[ "elapsed" ]], silent = TRUE)
  cleanup()
  if (inherits(t, "try-error")) {
    result_row(
      rep = rep,
      elapsed_s = NA_real_,
      status = "error",
      error = conditionMessage(attr(t, "condition")),
      tasks_total = tasks_total,
      notes = notes
    )
  } else {
    t <- as.numeric(t)
    result_row(
      rep = rep,
      elapsed_s = t,
      tasks_total = tasks_total,
      tasks_per_sec = if (is.finite(tasks_total) && t > 0) tasks_total / t else NA_real_,
      notes = notes
    )
  }
}

bench_rpc_tiny_hot <- function() {
  blocks <- shards(sizes$rpc_n, block_size = 1L)
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      res <- shard_map(
        blocks,
        function(sh) sh$id,
        workers = workers,
        chunk_size = 1L,
        health_check_interval = 10000L,
        diagnostics = FALSE
      )
      vals <- unlist(results(res), use.names = FALSE)
      if (length(vals) != sizes$rpc_n) stop("bad rpc result length", call. = FALSE)
    }, tasks_total = sizes$rpc_n)
  })
}

bench_rpc_payload_probe <- function() {
  n_probe <- min(sizes$rpc_n, 200L)
  payload_env <- new.env(parent = emptyenv())
  payload_env$bytes <- numeric()
  blocks <- shards(n_probe, block_size = 1L)
  cleanup()
  pool_create(workers)
  traced <- FALSE
  t <- try({
    suppressMessages(trace(
      what = "sendCall",
      where = asNamespace("parallel"),
      tracer = bquote({
        .e <- .(payload_env)
        .e$bytes <- c(.e$bytes, length(serialize(list(fun = fun, args = args), NULL)))
      }),
      print = FALSE
    ))
    traced <- TRUE
    system.time({
      res <- shard_map(
        blocks,
        function(sh) sh$id,
        workers = workers,
        chunk_size = 1L,
        health_check_interval = 10000L,
        diagnostics = FALSE
      )
      vals <- unlist(results(res), use.names = FALSE)
      if (length(vals) != n_probe) stop("bad payload probe result length", call. = FALSE)
    })[[ "elapsed" ]]
  }, silent = TRUE)
  if (traced) {
    suppressMessages(try(untrace("sendCall", where = asNamespace("parallel")), silent = TRUE))
  }
  cleanup()
  if (inherits(t, "try-error")) {
    result_row(
      rep = 1L,
      elapsed_s = NA_real_,
      status = "error",
      error = conditionMessage(attr(t, "condition")),
      tasks_total = n_probe
    )
  } else {
    b <- payload_env$bytes
    result_row(
      rep = 1L,
      elapsed_s = as.numeric(t),
      tasks_total = n_probe,
      tasks_per_sec = n_probe / as.numeric(t),
      payload_mean_bytes = mean(b),
      payload_median_bytes = stats::median(b),
      payload_min_bytes = min(b),
      payload_max_bytes = max(b),
      notes = paste0("payload_n=", length(b), "; tracing overhead included")
    )
  }
}

bench_shm_queue_tiny_hot <- function() {
  taskq_ok <- exists("taskq_supported", asNamespace("shard"), inherits = FALSE) &&
    shard:::taskq_supported()
  if (!taskq_ok) {
    return(list(result_row(
      rep = 1L,
      elapsed_s = NA_real_,
      status = "skipped",
      error = "taskq unsupported",
      tasks_total = sizes$shm_n
    )))
  }
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      out <- buffer("integer", dim = sizes$shm_n, init = 0L, backing = "mmap")
      res <- shard_map(
        sizes$shm_n,
        out = list(out = out),
        fun = function(sh, out) {
          out[sh$idx] <- sh$idx
          NULL
        },
        workers = workers,
        chunk_size = 1L,
        dispatch_mode = "shm_queue",
        dispatch_opts = list(block_size = 1L, claim_batch = 4L),
        diagnostics = FALSE
      )
      if (!succeeded(res)) stop("shm_queue failed", call. = FALSE)
      if (!identical(as.integer(out[]), seq_len(sizes$shm_n))) {
        stop("bad shm_queue output", call. = FALSE)
      }
    }, tasks_total = sizes$shm_n, notes = "claim_batch=4")
  })
}

prepare_view_inputs <- function() {
  set.seed(20260705)
  X <- matrix(rnorm(sizes$view_n * sizes$view_p), nrow = sizes$view_n)
  Y <- matrix(rnorm(sizes$view_n * sizes$view_v), nrow = sizes$view_n)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  list(
    X = X,
    Y = Y,
    Xsh = share(X, backing = "mmap"),
    Ysh = share(Y, backing = "mmap")
  )
}

bench_view_xTy_block_hot <- function() {
  dat <- prepare_view_inputs()
  expected <- crossprod(dat$X, dat$Y)
  blocks <- shards(ncol(dat$Y), block_size = sizes$block_size)
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      res <- shard_map(
        blocks,
        borrow = list(X = dat$Xsh, Y = dat$Ysh),
        fun = function(shard, X, Y) {
          vY <- view_block(Y, cols = idx_range(shard$start, shard$end))
          shard:::view_xTy(X, vY)
        },
        workers = workers,
        chunk_size = 1L,
        diagnostics = FALSE
      )
      z <- do.call(cbind, results(res))
      if (max(abs(z - expected)) > 1e-7) stop("bad view_xTy block result", call. = FALSE)
    }, tasks_total = length(blocks$shards), notes = paste0("cols=", ncol(dat$Y)))
  })
}

bench_view_xTy_gather_hot <- function() {
  dat <- prepare_view_inputs()
  set.seed(20260706)
  idxs <- replicate(
    sizes$gather_tasks,
    sort(sample.int(ncol(dat$Y), sizes$gather_width)),
    simplify = FALSE
  )
  gshards <- shards_list(idxs)
  expected_first <- crossprod(dat$X, dat$Y[, idxs[[1L]], drop = FALSE])
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      res <- shard_map(
        gshards,
        borrow = list(X = dat$Xsh, Y = dat$Ysh),
        fun = function(shard, X, Y) {
          vY <- view(Y, cols = shard$idx, type = "auto")
          shard:::view_xTy(X, vY)
        },
        workers = workers,
        chunk_size = 1L,
        diagnostics = FALSE
      )
      mats <- results(res)
      if (length(mats) != length(idxs)) stop("bad gather result count", call. = FALSE)
      if (max(abs(mats[[1L]] - expected_first)) > 1e-7) {
        stop("bad view_xTy gather result", call. = FALSE)
      }
    }, tasks_total = length(idxs), notes = paste0("width=", sizes$gather_width))
  })
}

bench_col_vars_kernel_hot <- function() {
  set.seed(20260707)
  X <- matrix(rnorm(sizes$vars_n * sizes$vars_p), nrow = sizes$vars_n)
  Xsh <- share(X, backing = "mmap")
  expected <- apply(X, 2L, stats::var)
  blocks <- shards(ncol(X), block_size = min(32L, ncol(X)))
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      out <- buffer("double", dim = ncol(X), init = NA_real_, backing = "mmap")
      res <- shard_map(
        blocks,
        kernel = "col_vars",
        borrow = list(X = Xsh),
        out = list(out = out),
        workers = workers,
        diagnostics = FALSE
      )
      if (!succeeded(res)) stop("col_vars failed", call. = FALSE)
      vals <- as.numeric(out[])
      if (max(abs(vals - expected), na.rm = TRUE) > 1e-8) {
        stop("bad col_vars result", call. = FALSE)
      }
    }, tasks_total = length(blocks$shards), notes = paste0("matrix=", nrow(X), "x", ncol(X)))
  })
}

bench_shard_crossprod_tiled_hot <- function() {
  set.seed(20260708)
  X <- matrix(rnorm(sizes$cross_n * sizes$cross_p), nrow = sizes$cross_n)
  Y <- matrix(rnorm(sizes$cross_n * sizes$cross_v), nrow = sizes$cross_n)
  expected <- crossprod(X, Y)
  lapply(seq_len(reps), function(i) {
    timed_rep(i, function() {
      res <- shard_crossprod(
        X,
        Y,
        workers = workers,
        block_x = sizes$cross_block_x,
        block_y = sizes$cross_block_y,
        backing = "mmap",
        materialize = "always",
        diagnostics = FALSE
      )
      if (max(abs(res$value - expected)) > 1e-7) {
        stop("bad shard_crossprod result", call. = FALSE)
      }
    }, notes = paste0("matrix=", nrow(X), "x", ncol(X), "_by_", ncol(Y)))
  })
}

bench_fun <- switch(
  benchmark,
  rpc_tiny_hot = bench_rpc_tiny_hot,
  rpc_payload_probe = bench_rpc_payload_probe,
  shm_queue_tiny_hot = bench_shm_queue_tiny_hot,
  view_xTy_block_hot = bench_view_xTy_block_hot,
  view_xTy_gather_hot = bench_view_xTy_gather_hot,
  col_vars_kernel_hot = bench_col_vars_kernel_hot,
  shard_crossprod_tiled_hot = bench_shard_crossprod_tiled_hot,
  stop("unknown benchmark: ", benchmark, call. = FALSE)
)

rows <- tryCatch(
  {
    raw_rows <- bench_fun()
    if (is.data.frame(raw_rows)) raw_rows else do.call(rbind, raw_rows)
  },
  error = function(e) {
    result_row(
      rep = NA_integer_,
      elapsed_s = NA_real_,
      status = "error",
      error = conditionMessage(e)
    )
  }
)

write.csv(rows, out_csv, row.names = FALSE)
print(rows)
'
writeLines(runner_code, runner_path)

run_child <- function(label, lib, benchmark) {
  safe <- gsub("[^A-Za-z0-9_.-]+", "_", paste(label, benchmark, sep = "-"))
  partial <- file.path(partials_dir, paste0(safe, ".csv"))
  stdout <- file.path(logs_dir, paste0(safe, ".out"))
  stderr <- file.path(logs_dir, paste0(safe, ".err"))
  if (file.exists(partial)) unlink(partial)

  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      runner_path,
      lib,
      label,
      benchmark,
      as.character(reps),
      as.character(workers),
      profile,
      partial
    ),
    stdout = stdout,
    stderr = stderr,
    timeout = timeout
  )

  status_code <- as.integer(status)

  if (file.exists(partial)) {
    rows <- utils::read.csv(partial, stringsAsFactors = FALSE)
    rows$child_status <- if (identical(status_code, 0L)) "ok" else paste0("exit_", status_code)
    rows$stdout <- stdout
    rows$stderr <- stderr
    rows
  } else {
    data.frame(
      label = label,
      version = NA_character_,
      lib = lib,
      benchmark = benchmark,
      profile = profile,
      workers = workers,
      rep = NA_integer_,
      elapsed_s = NA_real_,
      status = if (identical(status_code, 124L)) "timeout" else "error",
      error = paste0("child process exited with status ", status_code),
      tasks_total = NA_integer_,
      tasks_per_sec = NA_real_,
      payload_mean_bytes = NA_real_,
      payload_median_bytes = NA_real_,
      payload_min_bytes = NA_real_,
      payload_max_bytes = NA_real_,
      materialized_mb = NA_real_,
      notes = "",
      child_status = paste0("exit_", status_code),
      stdout = stdout,
      stderr = stderr,
      stringsAsFactors = FALSE
    )
  }
}

targets <- data.frame(
  label = c("cran", "current"),
  lib = c(cran_lib, current_lib),
  stringsAsFactors = FALSE
)

all_rows <- list()
k <- 0L
for (benchmark in benchmarks) {
  cat("\n--- ", benchmark, " ---\n", sep = "")
  for (i in seq_len(nrow(targets))) {
    label <- targets$label[[i]]
    lib <- targets$lib[[i]]
    cat("Running ", label, " ... ", sep = "")
    rows <- run_child(label, lib, benchmark)
    k <- k + 1L
    all_rows[[k]] <- rows
    ok <- all(rows$status == "ok", na.rm = TRUE) && !any(rows$status != "ok", na.rm = TRUE)
    if (isTRUE(ok)) {
      cat("ok")
      elapsed <- rows$elapsed_s[is.finite(rows$elapsed_s)]
      if (length(elapsed)) {
        med <- stats::median(elapsed, na.rm = TRUE)
        if (is.finite(med)) cat(" median=", sprintf("%.4fs", med), sep = "")
      }
      payload <- rows$payload_median_bytes[is.finite(rows$payload_median_bytes)]
      if (length(payload)) cat(" payload_median=", sprintf("%.0fB", stats::median(payload)), sep = "")
      cat("\n")
    } else {
      cat(paste(unique(rows$status), collapse = ","), "\n", sep = "")
    }
  }
}

combined <- do.call(rbind, all_rows)
utils::write.csv(combined, out_path, row.names = FALSE)

ok_rows <- combined[combined$status == "ok" & is.finite(combined$elapsed_s), , drop = FALSE]
if (nrow(ok_rows) > 0L) {
  med <- aggregate(
    elapsed_s ~ benchmark + label,
    data = ok_rows,
    FUN = stats::median
  )
  wide <- reshape(
    med,
    idvar = "benchmark",
    timevar = "label",
    direction = "wide"
  )
  names(wide) <- sub("^elapsed_s\\.", "", names(wide))
  if (all(c("cran", "current") %in% names(wide))) {
    wide$speedup_current_vs_cran <- wide$cran / wide$current
  }
  cat("\n=============================================================================\n")
  cat("Median elapsed summary\n")
  cat("=============================================================================\n")
  print(wide, row.names = FALSE)
}

payload_rows <- combined[
  combined$status == "ok" & is.finite(combined$payload_median_bytes),
  ,
  drop = FALSE
]
if (nrow(payload_rows) > 0L) {
  payload_med <- aggregate(
    payload_median_bytes ~ benchmark + label,
    data = payload_rows,
    FUN = stats::median
  )
  cat("\nPayload median bytes\n")
  print(payload_med, row.names = FALSE)
}

cat("\nWrote combined CSV: ", out_path, "\n", sep = "")
cat("Logs:               ", logs_dir, "\n", sep = "")
cat("Libraries:\n")
cat("  CRAN:    ", cran_lib, "\n", sep = "")
cat("  current: ", current_lib, "\n", sep = "")
cat("\nReuse this installation with:\n")
cat(
  "  Rscript inst/bench/current_vs_cran.R --skip-install",
  " --cran-lib=", shQuote(cran_lib),
  " --current-lib=", shQuote(current_lib),
  " --root=", shQuote(root),
  "\n",
  sep = ""
)
