#' @title Build multi-path forward selection using AIC
#'
#' @description A compact implementation of the multi-path forward selection described
#' in the project specification. Starts from the empty model and at each
#' step tries adding each unused variable to every current model, keeps
#' near-best children per parent within `delta` of the parent's best child,
#' and requires improvement >= eps. Deduplicates and caps models per level by L.
#'
#' @param x A \code{data.frame} (matrix) of numerical predictors.
#' @param y A \code{vector} of a numerical response.
#' @param family A \code{string} containing what family distribution y follows. Must be gaussian or binomial.
#' @param K A \code{numeric} used to denote the maximum number of steps (default min(p,10)).
#' @param eps A \code{numeric} used to denote the minimum AIC improvement to expand (default 1e-6).
#' @param delta A \code{numeric} used to denote the AIC tolerance for near-ties (default 1).
#' @param L A \code{numeric} used to denote the max number of models kept per level (default 50).
#' @return A \code{list} with elements:
#' \describe{
#' \item{path_forest}{list of frontiers (each frontier is a list of model entries)}
#' \item{aic_by_model}{data.frame of unique models and their AICs}
#' \item{meta}{parameters and counts}
#' \item{fits}{named list of fitted model objects (may be many)}
#' }
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' forest <- build_paths(x = X, y = y, family = "gaussian", K = 6)
#' forest <- build_paths(x = X, y = y, family = "binomial", K = 6, eps = 1e-2, delta = 2, L = 40)
#' }
build_paths <- function(x, y, family = c("gaussian","binomial"),
                        K = NULL, eps = 1e-6, delta = 1, L = 50,
                        keep_fits = TRUE) {
  family <- match.arg(family)
  x <- as.data.frame(x)
  n <- nrow(x); p <- ncol(x)
  if (is.null(K)) K <- min(p, 10)
  K <- min(K, p)

  varnames <- colnames(x)
  if (is.null(varnames) || any(nchar(varnames) == 0)) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
    varnames <- colnames(x)
  }

  if (length(y) != n) stop("Length of y must equal number of rows in x.")
  if (family == "binomial") {
    if (is.factor(y) && nlevels(y) != 2) stop("For binomial family, y must have 2 levels.")
    if (is.numeric(y) && !all(y %in% c(0,1))) stop("Numeric y for binomial must be 0/1.")
    if (!is.numeric(y) && !is.logical(y) && !is.factor(y)) stop("y must be numeric 0/1, logical, or a factor for binomial family.")
  }

  # Construct data.frame once to avoid reconstruction every time it is fit
  data_df <- data.frame(y = y, x, check.names = FALSE)

  # helper: fit model by variable set and compute AIC; returns list(fit,aic)
  fit_aic <- function(vars) {
    if (length(vars) == 0) {
      form <- as.formula("y ~ 1")
    } else {
      form <- stats::reformulate(vars, response = "y")
    }
    if (family == "gaussian") {
      f <- stats::lm(form, data = data_df)
    } else {
      f <- stats::glm(form, data = data_df, family = stats::binomial())
    }
    return(list(fit = f, aic = stats::AIC(f)))
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
      improvement <- parent$aic - best_child_aic
      # require best child improves over parent by at least eps
      if (improvement < eps) next
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

  # attach vars as list-column and (optionally) fits as named list
  vars_list <- lapply(all_keys, function(k) get(k, envir = all_models)$vars)
  fits_list <- NULL
  if (keep_fits) {
    fits_list <- lapply(all_keys, function(k) get(k, envir = all_models)$fit)
    names(fits_list) <- all_keys
  }
  aic_by_model$vars <- I(vars_list)

  # add formula column (可读性更好) and sort by AIC
  if (nrow(aic_by_model) > 0) {
    aic_by_model$formula <- sapply(all_keys, function(k) {
      vars <- get(k, envir = all_models)$vars
      if (length(vars) == 0) "y ~ 1" else paste("y ~", paste(vars, collapse = " + "))
    })
    aic_by_model <- aic_by_model[order(aic_by_model$aic), , drop = FALSE]
    rownames(aic_by_model) <- aic_by_model$key
  }

  meta <- list(params = list(K = K, eps = eps, delta = delta, L = L, family = family),
               n_models = length(all_keys), p = p,
               aic_min = if (nrow(aic_by_model) > 0) min(aic_by_model$aic) else NA)

  class_out <- list(path_forest = path_forest,
                    aic_by_model = aic_by_model,
                    meta = meta,
                    fits = fits_list)
  class(class_out) <- "path_forest"
  return(class_out)
}
