#' @title Compute confusion metrics for logistic regression models
#'
#' @description Calculates common performance metrics (accuracy, sensitivity, specificity, FDR, DOR)
#' for a logistic regression model given true outcomes and a prediction cutoff.
#'
#' @param fit A \code{glm} object with family = binomial (logistic regression)
#' @param y_true A numeric vector of true binary outcomes (0/1)
#' @param cutoff A numeric threshold for classifying predicted probabilities as 1 (default = 0.5)
#'
#' @return A \code{data.frame} containing:
#' \describe{
#'   \item{prevalence}{Proportion of positive cases in y_true}
#'   \item{accuracy}{Overall accuracy}
#'   \item{sensitivity}{True positive rate}
#'   \item{specificity}{True negative rate}
#'   \item{FDR}{False discovery rate}
#'   \item{DOR}{Diagnostic odds ratio}
#' }
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' fit <- glm(y ~ x1 + x2, family = binomial, data = df)
#' confusion_metrics(fit, y_true = df$y, cutoff = 0.5)
#' }
confusion_metrics <- function(fit, y_true = NULL, cutoff = 0.5) {
  if (!inherits(fit, "glm") || family(fit)$family != "binomial")
    stop("fit must be a logistic regression glm object")

  if (is.null(y_true)) y_true <- fit$y  # 默认使用 fit 内部 y

  if (length(y_true) != length(fitted(fit)))
    stop("y_true must have same length as fitted values")

  # predicted class
  y_pred <- ifelse(fitted(fit) >= cutoff, 1, 0)

  # confusion table components
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


