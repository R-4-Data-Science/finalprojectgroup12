#' @title Select plausible models based on AIC and average stability
#'
#' @description Filter models from a multi-path forward selection forest based on AIC,
#' average stability of predictors, and optional Jaccard similarity to remove near-duplicates.
#'
#' @param forest A \code{list} that is an output from build_paths()
#' @param pi A numeric \code{vector} of variable stability
#' @param Delta A \code{numeric} that denotes AIC tolerance for keeping near-best models (default = 2)
#' @param tau A \code{numeric} that denotes minimum average stability threshold (default = 0.4)
#' @param jaccard_thresh A \code{numeric} that denotes the optional threshold (0-1) to drop near-duplicate models(default = NULL).
#' @return A \code{data.frame} of plausible models with columns: key, aic, size, vars, avg_stability
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' plaus <- plausible_models(forest = forest, pi = pi)
#' plaus <- plausible_models(forest = forest, pi = pi, Delta = 3, tau = 0.5, jaccard_thresh = 1)
#' }
plausible_models <- function(forest, pi, Delta = 2, tau = 0.4, jaccard_thresh = NULL) {
  # Input check
  if (!inherits(forest, "path_forest")) stop("forest must be a path_forest object from build_paths()")
  if (!is.numeric(pi)) stop("pi must be numeric")
  if (is.null(names(pi))) names(pi) <- paste0("V", seq_len(forest$meta$p))
  if (length(pi) != forest$meta$p) stop("pi length must match number of predictors")
  if (Delta < 0) stop("Delta must be non-negative")
  if (tau < 0 || tau > 1) stop("tau must be between 0 and 1")
  if (!is.null(jaccard_thresh) && (jaccard_thresh < 0 || jaccard_thresh > 1)) stop("jaccard_thresh must be 0-1")

  models <- forest$aic_by_model
  if (nrow(models) == 0) return(data.frame())

  # Compute average stability for each model
  models$avg_stability <- vapply(models$vars, function(vs) {
    if (length(vs) == 0) return(0)
    mean(pi[vs], na.rm = TRUE)
  }, numeric(1))

  # filter by aic and stability
  AIC_min <- min(models$aic)
  models <- models[models$aic <= (AIC_min + Delta) & models$avg_stability >= tau, , drop = FALSE]
  if (nrow(models) == 0) return(data.frame())

  # remove near-duplicate model using jaccard
  if (!is.null(jaccard_thresh) && nrow(models) > 1){
    models <- models[order(models$aic), , drop = FALSE]
    keep <- rep(TRUE, nrow(models))
    for (i in seq_len(nrow(models) -1)){
      if (!keep[i]) next
      vars_i <- models$vars[[i]]
      for (j in (i+1):nrow(models)) {
        if (!keep[j]) next
        vars_j <- models$vars[[j]]
        jaccard <- if (length(union(vars_i, vars_j)) == 0) 1 else length(intersect(vars_i, vars_j)) / length(union(vars_i, vars_j))
        if (jaccard >= jaccard_thresh) keep[j] <- FALSE
    }
  }
    models <- models[keep, , drop = FALSE]
  }

  attr(models, "AIC_min") <- AIC_min
  attr(models, "Delta") <- Delta
  attr(models, "tau") <- tau
  attr(models, "jaccard_thresh") <- jaccard_thresh


  rownames(models) <- NULL
  return(models)
}
