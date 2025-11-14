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

