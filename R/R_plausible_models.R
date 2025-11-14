#' @title Select plausible models based on AIC and average stability
#'
#' @description Taking a collection of models that have AIC and stability measures and keeping models
#' that both have an AIC below a threshold and have stability across variables above a threshold.
#'
#' @param forest A \code{list} that is an output from build_paths()
#' @param pi A numeric \code{vector} of variable stability
#' @param Delta A \code{numeric] that denotes AIC tolerance (default 2)
#' @param tau A \code{numeric} that denotes minimum average stability threshold (default 0.6)
#' @param jaccard_thresh A \code{numeric] that denotes the threshold at which near-duplicate models are
#' dropped if their Jaccard similarity is above
#' @return A \code{data.frame} of plausible models with columns: key, aic, size, vars, avg_stability
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' plaus <- plausible_models(forest = forest, pi = pi)
#' plaus <- plausible_models(forest = forest, pi = pi, Delta = 3, tau = 0.5, jaccard_thresh = 1)
#' }
plausible_models <- function(forest, pi, Delta = 2, tau = 0.4, jaccard_thresh = NULL) {
  models <- forest$aic_by_model
  models$avg_stability <- sapply(models$vars, function(vs) {
    if (length(vs)==0) return(0)
    mean(pi[vs])
  })

  # keep AIC <= AIC_min + Delta
  AIC_min <- min(models$aic)
  keep <- models$aic <= (AIC_min + Delta)
  models <- models[keep, , drop = FALSE]

  # keep avg_stability >= tau
  models <- models[models$avg_stability >= tau, , drop = FALSE]

  # optional Jaccard filter
  if (!is.null(jaccard_thresh) && nrow(models) > 1) {
    n <- nrow(models)
    keep_idx <- rep(TRUE, n)
    for (i in 1:(n-1)) {
      if (!keep_idx[i]) next
      vars_i <- models$vars[[i]]
      for (j in (i+1):n) {
        if (!keep_idx[j]) next
        vars_j <- models$vars[[j]]
        union_len <- length(union(vars_i, vars_j))
        inter_len <- length(intersect(vars_i, vars_j))
        if (inter_len / union_len >= jaccard_thresh) {
          keep_idx[j] <- FALSE
        }
      }
    }
    models <- models[keep_idx, , drop = FALSE]
  }

  rownames(models) <- NULL
  return(models)
}
