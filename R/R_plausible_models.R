#' Select plausible models based on AIC and average stability
#'
#' @param forest output from build_paths()
#' @param pi named numeric vector of variable stability
#' @param Delta AIC tolerance (default 2)
#' @param tau minimum average stability threshold (default 0.6)
#' @param jaccard_thresh optional, drop near-duplicate models with Jaccard similarity >= threshold
#' @return data.frame of plausible models with columns: key, aic, size, vars, avg_stability
#' @export
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
