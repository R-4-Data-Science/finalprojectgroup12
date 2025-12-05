#' @title Stability estimation via resampling and multi-path search
#'
#' @description Run multi-path forward selection on resampled datasets and computes
#' stability scores pi_j = average proportion of models containing predictor j across resamples.
#'
#' @param x A \code{data.frame} (matrix) of numerical predictors.
#' @param y A \code{vector} of numerical or factor response.
#' @param B A \code{numeric} (integer) used to denote number of resamples (default = 50).
#' @param resample A \code{string} used to denote type of resampling. Must be bootstrap or subsample (default = "bootstrap").
#' @param m A \code{numeric} used to denote subsample size if resample="subsample" (default = floor(0.7*n)).
#' @param build_args \code{list} of arguments to pass to build_paths()(except x,y).
#' @param seed \code{integer} seed for reproducibility.
#' @param verbose A \code{logical} that determines whether the function prints progress reports as it runs (default = FALSE).
#' @return A \code{list} with elements:
#' \describe{
#' \item{pi}{each variables stability}
#' \item{metadata}{parameters and counts}
#' }
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' # Gaussian example
#' stable <- stability(x = X, y = y, B = 50, resample = "bootstrap")
#'
#' # Binomial example with subsampling
#' stable <- stability(x = X, y = y_bin, B = 40, resample = "subsample", m = 100, verbose = TRUE)
#' }
stability <- function(x, y, B = 50, resample = c("bootstrap", "subsample"), m = NULL,
                      build_args = list(),
                      seed = NULL, verbose = FALSE) {

  # Match arguments
  resample <- match.arg(resample)

  # Convert to data.frame for consistent handling
  x <- as.data.frame(x)
  n <- nrow(x)
  p <- ncol(x)

  # Auto-detect family if not specified in build_args
  auto_detect_family <- function(y) {
    if (is.factor(y)) {
      if (nlevels(y) == 2) {
        return("binomial")
      } else {
        return("gaussian")
      }
    } else if (is.logical(y)) {
      return("binomial")
    } else if (is.numeric(y)) {
      # Check if y contains only 0 and 1 (binary response)
      unique_vals <- unique(y)
      if (length(unique_vals) == 2 && all(sort(unique_vals) == c(0, 1))) {
        return("binomial")
      } else {
        return("gaussian")
      }
    } else {
      # Default to gaussian for unknown types
      return("gaussian")
    }
  }

  # Default arguments for build_paths
  default_build_args <- list(
    K = NULL,
    eps = 1e-6,
    delta = 1,
    L = 50,
    keep_fits = FALSE
  )

  # Merge user build_args with defaults
  build_args <- modifyList(default_build_args, build_args)

  # Auto-detect family if not provided by user
  if (is.null(build_args$family)) {
    build_args$family <- auto_detect_family(y)
    if (verbose) {
      message("Auto-detected family: ", build_args$family)
    }
  }

  # Set default K if not provided
  if (is.null(build_args$K)) {
    build_args$K <- min(p, 10)
  }

  # Get variable names
  varnames <- colnames(x)
  if (is.null(varnames)) {
    varnames <- paste0("V", seq_len(p))
  }

  # Validate B
  if (!is.numeric(B) || B < 1) {
    stop("B must be a positive integer")
  }
  B <- as.integer(B)

  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Set default subsample size according to project specification
  if (resample == "subsample" && is.null(m)) {
    m <- ceiling(sqrt(n))
    if (verbose) {
      message("Using subsample size m = ", m, " (ceiling(sqrt(n)))")
    }
  }

  if (resample == "subsample") {
    m <- as.integer(m)
    if (m < 1 || m > n) {
      stop("Subsample size m must be between 1 and n")
    }
  }

  # Initialize matrix to store z_j^(b) values
  # Each row corresponds to a resample, each column to a predictor
  z_matrix <- matrix(0, nrow = B, ncol = p)
  colnames(z_matrix) <- varnames

  # Track failed resamples
  failed_idx <- integer(0)
  empty_models_idx <- integer(0)

  # Main resampling loop
  for (b in seq_len(B)) {
    # Create resample indices
    if (resample == "bootstrap") {
      idx <- sample.int(n, size = n, replace = TRUE)
    } else {  # subsample
      idx <- sample.int(n, size = m, replace = FALSE)
    }

    # Extract resampled data
    xb <- x[idx, , drop = FALSE]
    yb <- y[idx]

    # Prepare y for binomial family if needed
    if (build_args$family == "binomial") {
      if (is.factor(yb)) {
        # Convert factor to 0/1 (second level = 1)
        yb <- as.integer(yb == levels(yb)[2])
      } else if (is.logical(yb)) {
        yb <- as.integer(yb)
      }

      # Skip if only one class present (cannot fit logistic model)
      if (length(unique(yb)) < 2) {
        failed_idx <- c(failed_idx, b)
        if (verbose) {
          message(sprintf("Resample %d skipped: only one class in y for binomial family", b))
        }
        next
      }
    }

    # Skip if NA values present
    if (any(is.na(xb)) || any(is.na(yb))) {
      failed_idx <- c(failed_idx, b)
      next
    }

    # Prepare arguments for build_paths
    args_bp <- modifyList(build_args, list(x = xb, y = yb))

    # Run multi-path search on resampled data
    forest_b <- tryCatch({
      do.call(build_paths, args_bp)
    }, error = function(e) {
      failed_idx <<- c(failed_idx, b)
      if (verbose) {
        message(sprintf("build_paths error at resample %d: %s", b, e$message))
      }
      NULL
    })

    # Check if build_paths returned valid results
    if (is.null(forest_b) ||
        !("aic_by_model" %in% names(forest_b)) ||
        nrow(forest_b$aic_by_model) == 0) {
      empty_models_idx <- c(empty_models_idx, b)
      next
    }

    # Extract variable sets from all models
    models_vars <- forest_b$aic_by_model$vars
    models_vars <- lapply(models_vars, function(v) {
      if (is.null(v)) {
        character(0)
      } else {
        as.character(v)
      }
    })

    n_models_b <- length(models_vars)
    if (n_models_b == 0) {
      empty_models_idx <- c(empty_models_idx, b)
      next
    }

    # Compute z_j^(b): proportion of models containing each variable
    for (j in seq_along(varnames)) {
      var_name <- varnames[j]
      count <- sum(sapply(models_vars, function(vars) var_name %in% vars))
      z_matrix[b, j] <- count / n_models_b
    }

    # Progress reporting
    if (verbose && B > 10 && b %% 10 == 0) {
      message(sprintf("Completed %d / %d resamples", b, B))
    }
  }

  # Determine successful resamples
  successful_rows <- rowSums(z_matrix, na.rm = TRUE) > 0
  B_successful <- sum(successful_rows)

  # Compute stability scores pi_j
  if (B_successful > 0) {
    pi <- colMeans(z_matrix[successful_rows, , drop = FALSE], na.rm = TRUE)
  } else {
    warning("All resamples failed or returned empty model sets. Returning NA stability scores.")
    pi <- setNames(rep(NA_real_, p), varnames)
  }

  # Ensure names are set
  names(pi) <- varnames

  # Prepare metadata
  metadata <- list(
    B_attempted = B,
    B_successful = B_successful,
    failed_idx = sort(unique(failed_idx)),
    empty_models_idx = sort(unique(empty_models_idx)),
    resample_type = resample,
    subsample_size = if (resample == "subsample") m else NULL,
    family = build_args$family,
    build_paths_args = build_args[c("K", "eps", "delta", "L", "keep_fits")]
  )

  # Create result object
  result <- list(
    pi = pi,
    metadata = metadata
  )

  class(result) <- "path_stability"
  return(result)
}
