#' Build multi-path forward selection using AIC
#'
#' A compact implementation of the multi-path forward selection described
#' in the project specification. Starts from the empty model and at each
#' step tries adding each unused variable to every current model, keeps
#' near-best children per parent within `delta` of the parent's best child,
#' and requires improvement >= eps. Deduplicates and caps models per level by L.
#'
#' @param x data.frame or matrix of predictors.
#' @param y response vector.
#' @param family "gaussian" or "binomial".
#' @param K maximum number of steps (default min(p,10)).
#' @param eps minimum AIC improvement to expand (default 1e-6).
#' @param delta AIC tolerance for near-ties (default 1).
#' @param L max number of models kept per level (default 50).
#' @return A list with elements:
#' \item{path_forest}{list of frontiers (each frontier is a list of model entries)}
#' \item{aic_by_model}{data.frame of unique models and their AICs}
#' \item{meta}{parameters and counts}
#' \item{fits}{named list of fitted model objects (may be many)}
#' @examples
#' \dontrun{
#' forest <- build_paths(x = X, y = y, family = "gaussian", K = 6)
#' }
#' @export
build_paths <- function(x, y, family = c("gaussian","binomial"),
                        K = NULL, eps = 1e-6, delta = 1, L = 50) {
  family <- match.arg(family)
  x <- as.data.frame(x)
  n <- nrow(x); p <- ncol(x)
  if (is.null(K)) K <- min(p, 10)
  varnames <- colnames(x)
  if (is.null(varnames) || any(nchar(varnames)==0)) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
    varnames <- colnames(x)
  }

  # helper: fit model by variable set and compute AIC; returns list(fit,aic)
  fit_aic <- function(vars) {
    if (length(vars) == 0) {
      if (family == "gaussian") f <- lm(y ~ 1)
      else f <- glm(y ~ 1, family = binomial())
    } else {
      form <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
      if (family == "gaussian") f <- lm(form, data = cbind(y = y, x))
      else f <- glm(form, data = cbind(y = y, x), family = binomial())
    }
    return(list(fit = f, aic = AIC(f)))
  }

  model_key <- function(vars) {
    if (length(vars) == 0) return("<NULL>")
    paste(sort(vars), collapse = "||")
  }

  # store all unique models to avoid refit
  all_models <- new.env(parent = emptyenv())

  # create entry and cache
  get_entry <- function(vars) {
    key <- model_key(vars)
    if (exists(key, envir = all_models, inherits = FALSE)) {
      return(get(key, envir = all_models, inherits = FALSE))
    } else {
      fa <- fit_aic(vars)
      entry <- list(vars = sort(vars), key = key, aic = fa$aic, fit = fa$fit)
      assign(key, entry, envir = all_models)
      return(entry)
    }
  }

  # initial frontier: empty model
  root <- get_entry(character(0))
  frontier <- list(root)
  path_forest <- list(frontier)

  for (step in seq_len(K)) {
    candidates <- list()
    # For each parent in frontier, try adding every remaining variable
    for (parent in frontier) {
      used <- parent$vars
      remaining <- setdiff(varnames, used)
      if (length(remaining) == 0) next
      # create children
      children <- lapply(remaining, function(v) {
        vars <- c(used, v)
        get_entry(vars)
      })
      # compute AICs and select those within delta of best child provided improvement >= eps
      child_aics <- sapply(children, `[[`, "aic")
      best_child_aic <- min(child_aics)
      # require best child improves over parent
      if ((parent$aic - best_child_aic) < eps) next
      keep_idx <- which(child_aics <= (best_child_aic + delta))
      if (length(keep_idx) > 0) {
        kept <- children[keep_idx]
        candidates <- c(candidates, kept)
      }
    }
    if (length(candidates) == 0) break
    # deduplicate candidates by key, keep best aic per key
    keys <- sapply(candidates, `[[`, "key")
    uniq_keys <- unique(keys)
    uniq_cands <- lapply(uniq_keys, function(k) {
      group <- Filter(function(x) x$key == k, candidates)
      group[[ which.min(sapply(group, `[[`, "aic")) ]]
    })
    # sort by aic and cap by L
    uniq_cands <- uniq_cands[order(sapply(uniq_cands, `[[`, "aic"))]
    if (length(uniq_cands) > L) uniq_cands <- uniq_cands[1:L]
    frontier <- uniq_cands
    path_forest[[length(path_forest) + 1]] <- frontier
  }

  # collect all models from env into data.frame
  all_keys <- ls(envir = all_models)
  aic_list <- lapply(all_keys, function(k) {
    e <- get(k, envir = all_models)
    data.frame(key = k, aic = e$aic, size = length(e$vars), stringsAsFactors = FALSE)
  })
  aic_by_model <- if (length(aic_list) > 0) do.call(rbind, aic_list) else data.frame()

  # attach vars as list-column and fits as named list
  vars_list <- lapply(all_keys, function(k) get(k, envir = all_models)$vars)
  fits_list <- lapply(all_keys, function(k) get(k, envir = all_models)$fit)
  names(fits_list) <- all_keys
  aic_by_model$vars <- I(vars_list)

  meta <- list(params = list(K = K, eps = eps, delta = delta, L = L, family = family),
               n_models = length(all_keys), p = p)

  return(list(path_forest = path_forest,
              aic_by_model = aic_by_model,
              meta = meta,
              fits = fits_list))
}
