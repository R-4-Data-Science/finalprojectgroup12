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
#' stable <- stability(x = X, y = y, B = 50, resample = "bootstrap")
#' stable <- stability(x = X, y = y, B = 40, resample = "subsample", m = 100, verbose = TRUE)
#' }
stability <- function(x, y, B = 50, resample = c("bootstrap","subsample"), m = NULL,
                      build_args = list(),
                      seed = NULL, verbose = FALSE) {
  resample <- match.arg(resample)
  x <- as.data.frame(x)
  n <- nrow(x)
  p <- ncol(x)
  default_build_args <- list(
    K = min(p,10),
    eps = 1e-6,
    delta = 1,
    L = 50,
    family = "gaussian",
    keep_fits = FALSE
  )

  # merge user build_args with defaults
  build_args <- modifyList(default_build_args, build_args)
  if (isTRUE(verbose)) {
    message("Merged build_args:")
    print(build_args)
  }
  varnames <- colnames(x)
  if (is.null(varnames)) varnames <- paste0("V", seq_len(p))

  if (!is.numeric(B) || B < 1) stop("B must be positive integer")
  B <- as.integer(B)

  if (!is.null(seed)) set.seed(seed)

  if (resample == "subsample" && is.null(m)) m <- floor(0.7 * n)
  if (resample == "subsample"){
    m <- as.integer(m)
    if (m < 1 || m > n) stop("m must be between 1 and n")
  }

  # Initialize results matrix
  z_matrix <- matrix(0, nrow = B, ncol = p)
  colnames(z_matrix) <- varnames
  failed_idx <- integer(0)
  empty_models_idx <- integer(0)

  for (b in seq_len(B)) {
    # resample indices
    if (resample == "bootstrap") {
      idx <- sample.int(n, size = n, replace = TRUE)
    } else {
      idx <- sample.int(n, size = m, replace = FALSE)
    }
    xb <- x[idx, , drop = FALSE]
    yb <- y[idx]

    # Ensure binomial is numeric 0/1
    if (is.factor(yb)) yb <- as.integer(yb == levels(yb)[2])
    if (is.logical(yb)) yb <- as.integer(yb)

    # Skip resample if binomial y has only 1 class
    if (build_args$family == "binomial" && length(unique(yb)) < 2) {
      failed_idx <- c(failed_idx, b)
      if (verbose) message(sprintf("Resample %d skipped: only one class in y", b))
      next
    }

    # skip if NA
    if (any(is.na(xb)) || any(is.na(yb))){
      failed_idx <- c(failed_idx, b)
      next
    }

    args_bp <- modifyList(build_args, list(x = xb, y = yb))
    forest_b <- tryCatch({
      do.call(build_paths, args_bp)
    }, error = function(e){
      failed_idx <<- c(failed_idx, b)
      if (verbose) message(sprintf("build_paths error at resample %d: %s", b, e$message))
      NULL
    })

    if (is.null(forest_b) || !("aic_by_model" %in% names(forest_b)) || nrow(forest_b$aic_by_model) == 0){
      empty_models_idx <- c(empty_models_idx, b)
      next
    }

    models_vars <- forest_b$aic_by_model$vars
    models_vars <- lapply(models_vars, function(v) if (is.null(v)) character(0) else as.character(v))
    n_models_b <- length(models_vars)
    if(n_models_b == 0){
      empty_models_idx <- c(empty_models_idx, b)
      next
    }

    # compute variable presence proportion
    z_matrix[b, ] <- vapply(varnames, function(vj) sum(vapply(models_vars, function(v) vj %in% v, integer(1))) / n_models_b, numeric(1))

    if (isTRUE(verbose) && B > 10 && b %% 10 == 0) message(sprintf("Completed %d / %d resample", b, B))
  }

  # determine successful resamples (rows with any non-zero entry)
  successful_rows <- rowSums(z_matrix, na.rm = TRUE) > 0
  B_successful <- sum(successful_rows)

  if (B_successful > 0) {
    pi <- colMeans(z_matrix[successful_rows, , drop = FALSE], na.rm = TRUE)
  } else {
    # If all failed, return zeros with warning
    warning("All resamples failed or returned empty model sets. Returning zeros for stability scores.")
    pi <- setNames(rep(0, p), varnames)
  }
  names(pi) <- varnames

  result <- list(
    pi = pi,
    resampling = list(
      B_attempted = B,
      B_successful = B_successful,
      failed_idx = sort(unique(failed_idx)),
      empty_models_idx = sort(unique(empty_models_idx)),
      type = resample,
      m = if (resample == "subsample") m else NULL,
      build_paths_args = build_args
    ),
    z_matrix = z_matrix
  )

  class(result) <- "path_stability"
  return(result)
}

