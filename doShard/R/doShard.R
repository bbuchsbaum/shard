#' @title doShard: Foreach Parallel Adapter for shard
#' @description Provides a foreach backend using the shard package for parallel execution.
#' @name doShard
#' @docType package
#' @import foreach
#' @importFrom iterators iter nextElem
#' @importFrom parallel detectCores
#' @importFrom shard pool_create pool_get pool_stop shard_map shards
#' @importFrom utils packageVersion
NULL

# Package environment for storing backend state
.doShard_env <- new.env(parent = emptyenv())
.doShard_env$registered <- FALSE
.doShard_env$options <- list()

#' Register the doShard Backend
#'
#' Registers doShard as the foreach parallel backend. This function must be
#' called before using `%dopar%` with shard.
#'
#' @param workers Integer. Number of worker processes. If NULL, uses
#'   `parallel::detectCores() - 1`.
#' @param mem_cap Memory limit per worker (e.g., "2GB"). Workers exceeding
#'   this limit are recycled.
#' @param recycle Logical or numeric. If TRUE, recycle workers on RSS drift.
#'   If numeric, specifies the drift threshold (default 0.5 = 50% growth).
#' @param profile Execution profile: "default", "memory" (aggressive recycling),
#'   or "speed" (minimal overhead).
#' @param packages Character vector. Packages to load in workers.
#' @param init_expr Expression to evaluate in each worker on startup.
#' @param chunk_size Integer. Iterations to batch per worker dispatch.
#' @param verbose Logical. Print diagnostic messages.
#'
#' @return Invisibly returns NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(foreach)
#' library(doShard)
#'
#' # Register with 4 workers
#' registerDoShard(workers = 4)
#'
#' # Use foreach with shard backend
#' result <- foreach(i = 1:100, .combine = c) %dopar% {
#'   sqrt(i)
#' }
#'
#' # Clean up
#' stopDoShard()
#' }
registerDoShard <- function(workers = NULL,
                            mem_cap = "2GB",
                            recycle = TRUE,
                            profile = c("default", "memory", "speed"),
                            packages = NULL,
                            init_expr = NULL,
                            chunk_size = 1L,
                            verbose = FALSE) {
  profile <- match.arg(profile)

  # Determine worker count
  if (is.null(workers)) {
    workers <- max(parallel::detectCores() - 1L, 1L)
  }
  workers <- max(as.integer(workers), 1L)

  # Store options for later use
  .doShard_env$options <- list(
    workers = workers,
    mem_cap = mem_cap,
    recycle = recycle,
    profile = profile,
    packages = packages,
    init_expr = init_expr,
    chunk_size = chunk_size,
    verbose = verbose
  )

  # Create the worker pool
  pool <- shard::pool_get()
  if (is.null(pool) || pool$n != workers) {
    if (!is.null(pool)) {
      shard::pool_stop()
    }

    rss_drift <- if (is.numeric(recycle)) recycle else 0.5
    if (isFALSE(recycle)) rss_drift <- Inf

    shard::pool_create(
      n = workers,
      rss_limit = mem_cap,
      rss_drift_threshold = rss_drift,
      packages = packages,
      init_expr = init_expr
    )

    if (verbose) {
      message("doShard: Created worker pool with ", workers, " workers")
    }
  }

  # Register the backend with foreach
  setDoPar(
    fun = doShard_backend,
    data = .doShard_env$options,
    info = info_doShard
  )

  .doShard_env$registered <- TRUE

  if (verbose) {
    message("doShard: Registered as foreach backend")
  }

  invisible(NULL)
}

#' Stop the doShard Backend
#'
#' Stops the shard worker pool and unregisters the foreach backend.
#'
#' @return Invisibly returns NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' registerDoShard(workers = 4)
#' # ... do work ...
#' stopDoShard()
#' }
stopDoShard <- function() {
  shard::pool_stop()
  .doShard_env$registered <- FALSE
  .doShard_env$options <- list()

  # Register sequential backend
  registerDoSEQ()

  invisible(NULL)
}

#' Get the doShard Worker Pool
#'
#' Returns the current shard worker pool, if one exists.
#'
#' @return A `shard_pool` object or NULL.
#' @export
getDoShardPool <- function() {
  shard::pool_get()
}

#' Backend Info Function
#'
#' Returns information about the doShard backend for foreach.
#'
#' @param data Backend options.
#' @param item Info item to retrieve.
#' @return Requested info.
#' @keywords internal
info_doShard <- function(data, item) {
  switch(item,
    name = "doShard",
    version = packageVersion("doShard"),
    workers = {
      pool <- shard::pool_get()
      if (!is.null(pool)) pool$n else data$workers
    },
    NULL
  )
}

#' doShard Backend Function
#'
#' The main backend function called by foreach to execute iterations.
#'
#' @param obj The foreach object.
#' @param expr The expression to evaluate.
#' @param envir The evaluation environment.
#' @param data Backend options.
#' @return Combined results.
#' @keywords internal
doShard_backend <- function(obj, expr, envir, data) {
  # Extract foreach options
  verbose <- data$verbose
  chunk_size <- data$chunk_size

  # Get the iterator arguments
  it_args <- obj$args
  accumulator <- obj$combineInfo$fun
  error_handling <- obj$errorHandling
  packages <- unique(c(obj$packages, data$packages))

  # Note: packages are loaded via shard's pool mechanism when pool_create is

  # called with the packages argument. Additional per-call packages from
  # foreach's .packages are handled by including them in the worker function.

  # Collect all iterations from iterators
  iterations <- collect_iterations(it_args)
  n_iter <- length(iterations)

  if (n_iter == 0) {
    return(NULL)
  }

  if (verbose) {
    message("doShard: Processing ", n_iter, " iterations")
  }

  # Get exported variables from foreach .export
  export_vars <- obj$export
  export_env <- new.env(parent = envir)

  if (!is.null(export_vars)) {
    for (var in export_vars) {
      if (exists(var, envir = envir, inherits = TRUE)) {
        export_env[[var]] <- get(var, envir = envir, inherits = TRUE)
      }
    }
  }

  # Also capture variables referenced in expr that exist in envir
  expr_vars <- all.vars(expr)
  for (var in expr_vars) {
    if (!exists(var, envir = export_env) &&
        exists(var, envir = envir, inherits = TRUE)) {
      export_env[[var]] <- get(var, envir = envir, inherits = TRUE)
    }
  }

  # Capture iterator variable names
  iter_names <- names(it_args)

  # Create the worker function
  # This function will be executed for each shard (iteration batch)
  worker_fun <- function(shard, .iterations, .expr, .export_env, .iter_names, .packages) {
    # Load required packages in worker
    for (pkg in .packages) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE, quietly = TRUE)
      )
    }

    results <- vector("list", length(shard$idx))

    for (j in seq_along(shard$idx)) {
      iter_idx <- shard$idx[j]
      iter_data <- .iterations[[iter_idx]]

      # Create evaluation environment with iterator values
      eval_env <- new.env(parent = .export_env)

      for (k in seq_along(.iter_names)) {
        name <- .iter_names[k]
        if (!is.null(name) && nchar(name) > 0) {
          eval_env[[name]] <- iter_data[[k]]
        }
      }

      # Evaluate the expression
      results[[j]] <- tryCatch(
        eval(.expr, envir = eval_env),
        error = function(e) {
          structure(
            list(message = conditionMessage(e), call = conditionCall(e)),
            class = c("doShard_error", "condition")
          )
        }
      )
    }

    results
  }

  # Execute using shard_map
  shard_desc <- shard::shards(n_iter, workers = data$workers)

  result <- shard::shard_map(
    shards = shard_desc,
    fun = worker_fun,
    borrow = list(
      .iterations = iterations,
      .expr = expr,
      .export_env = export_env,
      .iter_names = iter_names,
      .packages = packages
    ),
    workers = data$workers,
    chunk_size = chunk_size,
    profile = data$profile,
    mem_cap = data$mem_cap,
    recycle = data$recycle,
    diagnostics = FALSE
  )

  # Flatten results
  all_results <- unlist(result$results, recursive = FALSE)

  # Handle errors based on error handling mode
  if (error_handling == "stop") {
    errors <- Filter(function(x) inherits(x, "doShard_error"), all_results)
    if (length(errors) > 0) {
      stop("Error in foreach: ", errors[[1]]$message, call. = FALSE)
    }
  } else if (error_handling == "remove") {
    all_results <- Filter(function(x) !inherits(x, "doShard_error"), all_results)
  }
  # "pass" mode: leave errors in results

  # Apply combiner function
  if (!is.null(accumulator) && length(all_results) > 0) {
    combine_results(all_results, accumulator, obj$combineInfo)
  } else {
    all_results
  }
}

#' Collect Iterations from Iterators
#'
#' Collects all values from foreach iterator arguments.
#'
#' @param it_args List of iterator arguments.
#' @return List of iteration value lists.
#' @keywords internal
collect_iterations <- function(it_args) {
  if (length(it_args) == 0) {
    return(list())
  }

  # Convert each argument to an iterator
  iters <- lapply(it_args, function(x) {
    if (inherits(x, "iter")) {
      x
    } else {
      iterators::iter(x)
    }
  })

  # Collect all values
  iterations <- list()
  repeat {
    values <- tryCatch({
      lapply(iters, iterators::nextElem)
    }, error = function(e) {
      if (inherits(e, "StopIteration")) {
        NULL
      } else {
        stop(e)
      }
    })

    if (is.null(values)) break

    iterations[[length(iterations) + 1L]] <- values
  }

  iterations
}

#' Combine Results
#'
#' Applies the foreach combine function to results.
#'
#' @param results List of results.
#' @param accumulator Combine function.
#' @param combine_info Combine info from foreach.
#' @return Combined result.
#' @keywords internal
combine_results <- function(results, accumulator, combine_info) {
  if (length(results) == 0) {
    return(combine_info$init)
  }

  # Handle .multicombine
  if (isTRUE(combine_info$multicombine)) {
    # Combine all at once if possible
    if (is.null(combine_info$init)) {
      do.call(accumulator, results)
    } else {
      do.call(accumulator, c(list(combine_info$init), results))
    }
  } else {
    # Combine pairwise
    acc <- if (is.null(combine_info$init)) results[[1]] else combine_info$init
    start <- if (is.null(combine_info$init)) 2L else 1L

    if (start <= length(results)) {
      for (i in start:length(results)) {
        acc <- accumulator(acc, results[[i]])
      }
    }

    acc
  }
}
