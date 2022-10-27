# %% ####################################################
#' Bayesian bootstrap for linear regression
#' @param object model object of class lm or fixest
#' @param reps number of replications
#' @param cluster clustering variable passed as bare formula or char vector
#' @return bboot object
#' @examples
#' lm(mpg ~ wt + hp, mtcars) |>
#'   bboot(reps = 1e3) |>
#'   summary()
#' @export
bboot = function(object, reps = 100L, cluster = NULL, ...) {
  require(data.table); require(fixest)
  # flag for fixest
  fixest_obj = inherits(object, c('fixest', 'fixest_multi'))
  if (inherits(object, c('lm'))) {
    Ymat = object$model[, 1]
  } else if (fixest_obj) {
    Ymat = model.matrix(object, type = 'lhs', as.matrix = TRUE)
  } else {
    stop('\nModel or object class not currently supported.\n')
  }
  Xmat = model.matrix(object)
  n_weights = nrow(Xmat)
  fmat = NULL
  if (fixest_obj && !is.null(object$fixef_vars)) {
    fmat = model.matrix(object, type = 'fixef')
  }
  ## Have to do a bit of leg work to pull out the clusters and match to
  ## model matrix
  if (!is.null(cluster)) {
    if (inherits(cluster, "formula")) {
      cl_string = strsplit(paste0(cluster)[2], split = ' \\+ ')[[1]]
    } else {
      cl_string = paste(cluster)
    }
    if (!is.null(fmat) && all(cl_string %in% colnames(fmat))) {
      cl_mat = fmat[, cl_string]
    } else if (all(cl_string %in% colnames(Xmat))) {
      cl_mat = Xmat[, cl_string]
    } else {
      DATA = eval(object$call$data)
      if (all(cl_string %in% names(DATA))) {
        all_vars = sapply(list(Ymat, Xmat, fmat), colnames)
        if (inherits(all_vars, 'list')) all_vars = do.call('c', all_vars)
        all_vars = union(all_vars, cl_string)
        DATA = data.frame(DATA)[, intersect(colnames(DATA), all_vars)]
        DATA = DATA[complete.cases(DATA), ]
        cl_mat = model.matrix(~ 0 + ., DATA[, cl_string, drop = FALSE])
      } else {
        stop(paste0('Could not find ', cluster, '. Please provide a valid input.\n'))
      }
    }
    if (!inherits(cl_mat, "matrix")) cl_mat = matrix(cl_mat)
    n_weights = nrow(unique(cl_mat))
    ## Keep track of cluster id for consistent weighting  within each
    ## cluster later on
    cl_mat = data.table::as.data.table(cl_mat)
    cl_mat$cl_id = data.table::frank(cl_mat, ties.method = "dense")
  }
  ## Pre-allocate space for efficiency
  wfits = matrix(0, reps, length(object$coefficients))
  for (i in 1:reps) {
    if (is.null(cluster)) {
      weights = rexp(n_weights, rate = 1)
    } else {
      weights = cl_mat[, wt := rexp(1, rate = 1), by = cl_id][, wt]
    }
    ## Demean X and Y matrices if fixed effects are present
    if (!is.null(fmat)) {
      Xmat = fixest::demean(X = Xmat, f = fmat, weights = weights)
      Ymat = fixest::demean(X = Ymat, f = fmat, weights = weights)
    }
    ## Fit weighted reg
    wfits[i, ] = lm.wfit(x = Xmat, y = Ymat, w = weights)$coefficients
  }
  colnames(wfits) = colnames(Xmat)
  class(wfits) = "bboot"
  return(wfits)
}

# %% ####################################################
#' summary method for bayesian bootstrap
#' @param object bboot object
#' @param level test level (0.95 by default)
#' @export
summary.bboot = function(object, level = 0.95, ...) {
  alpha = 1 - level
  lwr = alpha / 2
  upr = 1 - lwr
  out = t(apply(object, 2, \(x) c(mean(x), quantile(x, c(lwr, upr)))))
  colnames(out) = c("mean", "conf.low", "conf.upper")
  out
}
