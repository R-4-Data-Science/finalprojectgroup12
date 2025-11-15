#' @title Compute variable stability via resampling
#'
#' @description Computing stability of elements selected by multi-path forward regression
#' by resampling and rerunning multi-path forward regression on each rerun.
#'
#' @param x A \code{data.frame} (matrix) of numerical predictors.
#' @param y A \code{vector} of a numerical response.
#' @param B A \code{numeric} (integer) used to denote number of resamples (default = 50).
#' @param resample A \code{string} used to denote type of resampling. Must be bootstrap or subsample.
#' @param m A \code{numeric} used to denote subsample size if resample="subsample"
#' @param ... additional parameters passed to build_paths()
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
#' stable <- stability(x = X, y = y, B = 40, resample = "subsample", m = 100)
#' }
stability <- function(x, y, B = 50, resample = "bootstrap", m = NULL,
                      family = "gaussian", K = NULL, eps = 1e-6, delta = 1,
                      L = 50, ..., verbose = FALSE) {
  # Input validation
  x <- as.data.frame(x)
  n <- nrow(x);p <- ncol(x)
  varnames <- colnames(x)
  if (is.null(varnames)) varnames <- paste0("V", seq_len(p))

  B <- as.integer(B)
  if (is.na(B) || B < 1) stop("B must be positive integer")
  if (!resample %in% c("bootstrap", "subsample")) stop("resample must be `bootstrap` or `subsample`")


  # Set default subsample size
  if (is.null(m)) {
    m <- if (resample == "subsample") ceiling(sqrt(n)) else n
  }
  if (resample == "subsample"){
    m <- as.integer(m)
    if (is.na(m) || m < 1 || m > n) stop("For subsample, m must be integer between 1 and n")
  }

  dots <- list(...)
  if(!("keep_fits" %in% names(dots))) dots$keep_fits <- FALSE

  # Initialize results matrix
  z_list <- matrix(NA_real_, nrow = B, ncol = p)
  colnames(z_list) <- varnames

  failed_idx <- integer(0)
  empty_models_idx <- integer(0)

  # Progress indicator for large B
  if (verbose && B > 0) message(sprintf("Running %d resamples (%s) ...", B, resample))

  for (b in seq_len(B)) {
    if (resample == "bootstrap") {
      idx <- sample.int(n, size = n, replace = TRUE)
    } else {
      idx <- sample.int(n, size = m, replace = FALSE)
    }

    xb <- x[idx, , drop = FALSE]
    yb <- y[idx]

    #call biuld_paths safely
    args_bp <- c(list(x = xb, y = yb, family = family, K = K, eps = eps, delta = delta, L = L), dots)
    forest_b <- tryCatch({
      do.call(build_paths, args_bp)
    }, error = function(e){
      if (verbose) message(sprintf("build_paths error on resample %d: %s", b, e$message))
      NULL
    })

    if (is.null(forest_b)){
      failed_idx <- c(failed_idx, b)
      next
    }


    if(!("aic_by_model" %in% names(forest_b)) || nrow(forest_b$aic_by_model) == 0){
      empty_models_idx <- c(empty_models_idx, b)
      z_list[b, ] <- 0
      next
    }

    models_vars <- forest_b$aic_by_model$vars
    models_vars <- lapply(models_vars, function(v) if (is.null(v)) character(0) else as.character(v))
    n_models_b <- length(models_vars)
    if (n_models_b == 0){
      empty_models_idx <- c(empty_models_idx, b)
      z_list[b, ] <- 0
      next
    }

      var_presence <- vapply(varnames, function(var_j){
        sum(vapply(models_vars, function(v) var_j %in% v, logical(1)))
      }, integer(1))

      z_list[b, ] <- var_presence / n_models_b

      if (verbose && B > 10 && b %% 10 == 0) message(sprintf(" Completed %d / %d", b, B))
  }


  pi <- colMeans(z_list, na.rm = TRUE)
  pi[is.nan(pi)] <- NA_real_
  names(pi) <- varnames

  B_successful <- sum(!apply(is.na(z_list), 1, all))
  result <- list(
    pi = pi,
    resampling = list(
      B_attempted = B,
      B_successful = B_successful,
      failed_idx = sort(unique(failed_idx)),
      empty_models_idx = sort(unique(empty_models_idx)),
      type = resample,
      m = if (resample == "subsample") m else NULL,
      build_paths_args = list(family = family, K = K, eps = eps, delta = delta, L = L)
    ),
    z_matrix = z_list
  )

  class(result) <- "path_stability"
  return(result)
}

