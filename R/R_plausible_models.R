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


# 1. Plausible models selection (AIC + average stability + Jaccard filter)
plausible_models <- function(forest, pi, Delta = 2, tau = 0.4, jaccard_thresh = NULL) {
  # Input validation
  if (!inherits(forest, "path_forest"))
    stop("forest must be a path_forest object from build_paths()")
  p <- forest$meta$p
  if (length(pi) != forest$meta$p)
    stop("pi length must match number of predictors")
  if (Delta < 0) stop("Delta must be non-negative")
  if (tau < 0 || tau > 1) stop("tau must be between 0 and 1")
  if (!is.null(jaccard_thresh) && (jaccard_thresh < 0 || jaccard_thresh > 1))

  models <- forest$aic_by_model

  # Handle case with no models
  if (nrow(models) == 0){
    warning("No models found in the forest")
    return(data.frame())
  }

  # Compute average stability for each model
  models$avg_stability <- sapply(models$vars, function(vs) {
    if (length(vs) == 0) return(0)
    mean(pi[vs], na.rm = TRUE)
  })

  # Keep models with AIC <= AIC_min + Delta
  AIC_min <- min(models$aic)
  models <- models[models$aic <= (AIC_min + Delta), , drop = FALSE]

  # Keep models with average stability >= tau
  models <- models[models$avg_stability >= tau, , drop = FALSE]

  if (nrow(models) == 0){
    warning("No models meet stability threshold tau = ", tau)
    return(data.frame())
  }


  if (!is.null(jaccard_thresh) && nrow(models) > 1) {

    models <- models[order(models$aic), drop = FALSE]

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
        jaccard_sim <- if (union_len == 0) 1 else inter_len / union_len

        if (jaccard_sim >= jaccard_thresh) keep_idx[j] <- FALSE
      }
    }
    models <- models[keep_idx, , drop = FALSE]
  }

  attr(models, "AIC_min") <- AIC_min
  attr(models, "Delta") <- Delta
  attr(models, "tau") <- tau
  attr(models, "jaccard_thresh") <- jaccard_thresh


  rownames(models) <- NULL
  return(models)
}

# 2. Confusion metrics for logistic models
confusion_metrics <- function(fit, y_true, cutoff = 0.5) {
  if (!inherits(fit, "glm") || family(fit)$family != "binomial")
    stop("fit must be a logistic regression glm object")
  if (length(y_true) != length(fitted(fit)))
    stop("y_true must have same length as fitted values")

  y_pred <- ifelse(fitted(fit) >= cutoff, 1, 0)

  TP <- sum(y_pred == 1 & y_true == 1)
  TN <- sum(y_pred == 0 & y_true == 0)
  FP <- sum(y_pred == 1 & y_true == 0)
  FN <- sum(y_pred == 0 & y_true == 1)

  prevalence <- (TP + FN) / length(y_true)
  accuracy <- (TP + TN) / length(y_true)
  sensitivity <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  specificity <- if ((TN + FP) == 0) NA else TN / (TN + FP)
  FDR <- if ((TP + FP) == 0) NA else FP / (TP + FP)
  DOR <- if ((FP * FN) == 0) NA else (TP * TN) / (FP * FN)

  data.frame(
    prevalence = prevalence,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    FDR = FDR,
    DOR = DOR
  )
}
