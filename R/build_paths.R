#' @title Build multi-path forward selection using AIC
#'
#' @description Multi-path forward selection: start from empty model; at each
#' step add each unused predictor to each current model, keep
#' near-best children per parent within `delta` of the parent's best child,
#' require improvement >= eps. Deduplicates and cap models per level by L.
#'
#' @param x A \code{data.frame} (matrix) of numerical predictors.
#' @param y A \code{vector} of a numerical response for gaussian; 0/1 or factor/logical for binomial.
#' @param family A \code{string} containing what family distribution y follows. Must be gaussian or binomial.
#' @param K A \code{numeric} maximum number of forward-selection steps (default min(p,10)).
#' @param eps A \code{numeric} used to denote the minimum AIC improvement to expand (default 1e-6).
#' @param delta A \code{numeric} used to denote the AIC tolerance for near-ties (default 1).
#' @param L A \code{numeric} used to denote the max number of models kept per level (default 50).
#' @param keep_fits A \code{logical} that determines whether the function saves the model fit objects (default = FALSE).
#' @param trace A \code{logical} whether to print light progress messages (default FALSE).
#' @return A \code{list} with elements:
#' \describe{
#' \item{path_forest}{list of frontiers (each frontier is a list of model entries)}
#' \item{aic_by_model}{data.frame of unique models and their AICs}
#' \item{meta}{parameters and counts}
#' \item{fits}{named list of fits if keep_fits = TRUE, otherwise NULL}
#' }
#' @author Lijuan Wang, Kira Noordwijk, Evan Jerome
#' @export
#' @examples
#' \dontrun{
#' forest <- build_paths(x = X, y = y, family = "gaussian", K = 6)
#' forest <- build_paths(x = X, y = y, family = "binomial", K = 6, eps = 1e-2, delta = 2, L = 40, keep_fits = FALSE)
#' }
build_paths <- function(x, y, family = c("gaussian","binomial"),
                        K = NULL, eps = 1e-6, delta = 1, L = 50,
                        keep_fits = FALSE, trace = FALSE) {
  family <- match.arg(family)
  x <- as.data.frame(x)
  n <- nrow(x)
  p <- ncol(x)
  if (is.null(K)) K <- min(p, 10)
  K <- min(K, p)

  # basic checks
  if (any(is.na(x)) || any(is.na(y))){
    stop("x and y must not contain NA. Please handle missing values before calling build_paths().")
  }
  if (!all(sapply(x, is.numeric))){
    stop("All columns in x must be numeric.")
  }

  varnames <- colnames(x)
  if (is.null(varnames) || any(nchar(varnames) == 0)) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
    varnames <- colnames(x)
  }

  if (family == "binomial") {
    if (is.factor(y)) {
      if (nlevels(y) != 2)
        stop("For binomial family, y must have exactly 2 levels.")
      # Explicitly map the LAST factor level to 1, first to 0
      lv <- levels(y)
      y <- as.integer(y == lv[2])
    }
    if (is.logical(y)) {
      y <- as.integer(y)
    }
    if (is.numeric(y)) {
      if (!all(y %in% c(0,1)))
        stop("Numeric y for binomial must be only 0/1.")
    }
  }

  # build data once
  data_df <- data.frame(y = y, x, check.names = FALSE)

  # robust fit function with tryCatch: return list(fit, aic, error, warning)
  fit_aic <- function(vars) {
    if (length(vars) == 0) {
      form <- stats::as.formula("y ~ 1")
    } else {
      form <- stats::reformulate(vars, response = "y")
    }

    warn_msg <- NULL
    res <- tryCatch({
      withCallingHandlers({
        if (family == "gaussian") {
          f <- stats::lm(form, data = data_df)
        } else {
          f <- stats::glm(form, data = data_df,
                          family = stats::binomial(),
                          control = stats::glm.control(maxit = 50))
        }
        list(fit = f, aic = stats::AIC(f), error = NULL, warning = NULL)
      }, warning = function(w) {
        warn_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      })
    }, error = function(e) {
      # on error, return Inf AIC (can't use fit)
      list(fit = NULL, aic = Inf, error = conditionMessage(e), warning = NULL)
    })

    if (!is.null(warn_msg)) res$warning <- warn_msg
    return(res)
  }

  # deterministic unique key for a given variable set (order-insensitive)
  model_key <- function(vars) {
    if (length(vars) == 0) return("<NULL>")
    paste(sort(vars), collapse = "||")
  }

  # env cache to avoid refitting the same model multiple times
  all_models <- new.env(parent = emptyenv())

  # fetch or compute entry; only save fit if keep_fits TRUE
  get_entry <- function(vars) {
    key <- model_key(vars)
    if (exists(key, envir = all_models, inherits = FALSE)) {
      return(get(key, envir = all_models, inherits = FALSE))
    } else {
      fa <- fit_aic(vars)
      entry <- list(vars = sort(vars), key = key, aic = fa$aic)
      if (keep_fits && !is.null(fa$fit)) entry$fit <- fa$fit
      # record potential errors/warnings (may be NULL)
      entry$error <- fa$error
      entry$warning <- fa$warning
      assign(key, entry, envir = all_models)
      return(entry)
    }
  }

  # initialize with null model
  root <- get_entry(character(0))
  # add step index for root
  root$step <- 0
  root$parent_key <- NA_character_
  frontier <- list(root)
  path_forest <- list(frontier)

  if (trace) message("build_paths: starting search, K=", K, ", p=", p)

  # main multi-path search loop
  for (step in seq_len(K)) {
    if (trace) message(" step ", step, " — frontier size: ", length(frontier))
    candidates <- list()

    # for each parent in frontier, try adding every remaining variable
    for (parent in frontier) {
      used <- parent$vars
      remaining <- setdiff(varnames, used)
      if (length(remaining) == 0) next

      # skip expanding from a parent with non-finite AIC (failed fit)
      if (!is.finite(parent$aic)) next

      # children from this parent
      children <- lapply(remaining, function(v) {
        vars <- c(used, v)
        e <- get_entry(vars)
        # attach parent info & step for traceability in the frontier item (not stored in cache)
        e$parent_key <- parent$key
        e$step <- step
        e
      })

      # extract aics for those children
      child_aics <- sapply(children, `[[`, "aic")
      # if all children failed (Inf) then skip
      if (all(!is.finite(child_aics))) next

      best_child_aic <- min(child_aics, na.rm = TRUE)
      improvement <- parent$aic - best_child_aic
      # require minimum improvement to expand from this parent
      if (is.na(improvement) || improvement < eps) next

      keep_idx <- which(child_aics <= (best_child_aic + delta))
      if (length(keep_idx) > 0) {
        kept <- children[keep_idx]
        candidates <- c(candidates, kept)
      }
    }

    if (length(candidates) == 0) {
      if (trace) message("  no candidates retained at step ", step, "; stopping.")
      break
    }

    # dedupe candidates by key, keep the best aic per key
    keys <- sapply(candidates, `[[`, "key")
    uniq_keys <- unique(keys)
    uniq_cands <- lapply(uniq_keys, function(k) {
      group <- Filter(function(x) x$key == k, candidates)
      # pick one with minimal aic; if tie, pick first
      idx <- which.min(sapply(group, `[[`, "aic"))
      selected <- group[[idx]]
      # ensure step and parent_key reflect the selected object
      selected
    })

    # sort by aic across all unique candidates and cap by L
    uniq_cands <- uniq_cands[order(sapply(uniq_cands, `[[`, "aic"))]
    if (length(uniq_cands) > L) {
      if (trace) message("  capping candidates to L = ", L, " (was ", length(uniq_cands), ")")
      uniq_cands <- uniq_cands[1:L]
    }

    frontier <- uniq_cands
    path_forest[[length(path_forest) + 1]] <- frontier
  }

  # collect cached models from env into aic_by_model (robust alignment)
  all_keys <- ls(envir = all_models)
  aic_list <- lapply(all_keys, function(k) {
    e <- get(k, envir = all_models)
    data.frame(key = k, aic = e$aic, size = length(e$vars), stringsAsFactors = FALSE)
  })
  aic_by_model <- if (length(aic_list) > 0) do.call(rbind, aic_list) else data.frame()

  # build vars list and fits list aligned to keys
  if (nrow(aic_by_model) > 0) {
    vars_list_named <- setNames(lapply(all_keys, function(k) get(k, envir = all_models)$vars), all_keys)
    aic_by_model$vars <- I(lapply(aic_by_model$key, function(k) vars_list_named[[k]]))
  }

  fits_list <- NULL
  if (keep_fits && length(all_keys) > 0) {
    fits_raw <- lapply(all_keys, function(k) get(k, envir = all_models)$fit)
    # keep names; fits might contain NULL for failed fits
    names(fits_raw) <- all_keys
    fits_list <- fits_raw
  }

  # formula column and sort by aic
  if (nrow(aic_by_model) > 0) {
    aic_by_model$formula <- sapply(aic_by_model$vars, function(vars) {
      if (length(vars) == 0) "y ~ 1" else paste("y ~", paste(vars, collapse = " + "))
    })
    aic_by_model <- aic_by_model[order(aic_by_model$aic), , drop = FALSE]
    rownames(aic_by_model) <- aic_by_model$key
  }

  meta <- list(params = list(K = K, eps = eps, delta = delta, L = L, family = family),
               n_models = length(all_keys),
               p = p,
               aic_min = if (nrow(aic_by_model) > 0) min(aic_by_model$aic) else NA,
               n_steps = length(path_forest) - 1)

  class_out <- list(path_forest = path_forest,
                    aic_by_model = aic_by_model,
                    meta = meta,
                    fits = fits_list)
  class(class_out) <- "path_forest"
  return(class_out)
}
