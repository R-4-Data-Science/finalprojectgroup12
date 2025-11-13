#' Compute variable stability via resampling
#'
#' @param x predictor matrix or data.frame
#' @param y response vector
#' @param B number of resamples
#' @param resample "bootstrap" or "subsample"
#' @param m subsample size if resample="subsample"
#' @param ... additional parameters passed to build_paths()
#' @return list with pi (variable stability) and metadata
#' @export
stability <- function(x, y, B = 50, resample = "bootstrap", m = NULL, ...) {
  x <- as.data.frame(x)
  n <- nrow(x); p <- ncol(x)
  if (is.null(m)) m <- floor(sqrt(n))
  varnames <- colnames(x)
  z_list <- matrix(0, nrow = B, ncol = p)

  for (b in seq_len(B)) {
    if (resample == "bootstrap") {
      idx <- sample(n, n, replace = TRUE)
    } else if (resample == "subsample") {
      idx <- sample(n, m, replace = FALSE)
    } else stop("resample must be 'bootstrap' or 'subsample'")

    xb <- x[idx, , drop = FALSE]
    yb <- y[idx]
    forest_b <- build_paths(xb, yb, ...)

    # All the variables in the models
    models_vars <- forest_b$aic_by_model$vars
    for (j in seq_len(p)) {
      var_j <- varnames[j]
      count <- sum(sapply(models_vars, function(v) var_j %in% v))
      z_list[b,j] <- count / length(models_vars)
    }
  }

  pi <- colMeans(z_list)
  names(pi) <- varnames
  return(list(pi = pi, resampling = list(B = B, type = resample, m = m)))
}

