# %%
#' Replaces the standard errors (t and p vals)in FELM model object with robust SE
#' @param felm object
#' @export
#' @keywords hc0 robust heteroskedasticity output
#' @examples
#' \dontrun{
#' robustify(felm(y ~ x, data = df))
#' }
# returns lm summary object with cluster-robust standard errors
robustify = function(model) {
  model$se = model$rse
  model$tval = model$rtval
  model$pval = model$rpval
  return(model)
}

# %% ####################################################
#' return cross-fit predictions from glmnet object
#' @param m glmnet model object fit with keep = T
#' @return vector of cross-fit predictions
#' @export
fitGet = function(m) {
  m$fit.preval[, !is.na(colSums(m$fit.preval))][
    , # slice to nonmissing cols
    m$lambda[!is.na(colSums(m$fit.preval))] == m$lambda.min
  ] # match lambda
}

# %%
#' Partial out controls and fixed effects and return residualised outcome and treatment
#' @param y outcome
#' @param a primary rhs variable
#' @param x controls
#' @param d fixed effects
#' @param df dataframe
#' @keywords Frisch-Waugh-Lovell partial out
#' @export
#' @examples
#' \dontrun{
#' residualise('mpg', 'wt', 'cyl', mtcars)
#' }
residualise = function(y, a, x = "1", d = "0", df) {
  require(fixest)
  y_tilde = feols(formula_fixest(y, X = x, D = d), df)$residuals
  a_tilde = feols(formula_fixest(a, X = x, D = d), df)$residuals
  data.frame(y_tilde, a_tilde)
}


#' Partial Residual Plot
#'
#' Makes a Partial Residual plot
#'
#' @name prplot
#' @param g An object returned from lm()
#' @param i index of predictor
#' @return none
#' @keywords regression
#' @examples
#'
#' data(stackloss)
#' g = lm(stack.loss ~ ., stackloss)
#' prplot(g, 1)
#'
#' @export prplot
prplot = function(g, i) {
  # Partial residuals plot for predictor i
  xl = attributes(g$terms)$term.labels[i]
  yl = paste("beta*", xl, "+res", sep = "")
  x = model.matrix(g)[, i + 1]
  plot(x, g$coeff[i + 1] * x + g$res, xlab = xl, ylab = yl)
  abline(0, g$coeff[i + 1])
  invisible()
}

# %%
#' prepare X matrix with flexible interactions and polynomials
#' @description Prepare matrix with polynomial basis expansion and/or interactions.
#' @param dat              data table / dataframe
#' @param dummies          Names of dummy vars (null by default - auto-detected)
#' @param continuouses     Names of continuous vars to modify fn form (null by default - auto detected)
#' @param corr_cut         cutoff for correlation threshold to drop one of the vars (default = 0.9)
#' @param k                order of interactions: defaults to pairwise interactions
#' @param m                order of functions: defaults to quadratic functions
#' @param raw              raw or orthogonal bases
#' @return                 data.frame with base terms and interactions + basis
#' @export

polySieveM = function(dat,
                      dummies = NULL,
                      continuouses = NULL,
                      corr_cut = 0.90,
                      k = 2,
                      m = 2,
                      raw = F) {
  # coerce to df
  dat = as.data.frame(dat)
  nuniqs = apply(dat, 2, function(x) length(unique(x)))
  # populate lists if missing
  if (is.null(dummies)) dummies = colnames(dat)[which(nuniqs == 2)]
  if (is.null(continuouses)) continuouses = colnames(dat)[which(nuniqs >= 5)]
  # concat names
  controls = c(dummies, continuouses)
  data = dat[, controls]
  if (!is.null(continuouses)) { # if there are any continous variables
    # functional form changes for continuous vars
    # polynomials (orthogonal by default)
    polyfml = paste(paste0(
      "poly(", continuouses, ",", m,
      ", raw=", raw, ")"
    ), collapse = " + ")
    # model matrix with sieve polynomial basis
    powmat = model.matrix(as.formula(paste0("~ -1 + ", polyfml)), dat[, continuouses])
    # log variables with all positives
    loggable = apply(dat[, continuouses], 2, function(x) sum(x < 0) == 0)
    if (sum(loggable) > 0) {
      logmat = log1p(dat[, loggable]) %>% as.matrix()
      colnames(logmat) = paste0("log1p_", names(which(loggable == TRUE)))
      powmat = cbind(powmat, logmat)
    }
    # cbind base terms with smooth fns
    data = cbind(data, powmat)
  }
  if (k > 1) {
    # all n-way interactions with polynomials and dummies
    X = model.matrix(as.formula(glue("~.^{k} - 1")), data)
  } else {
    X = as.matrix(data)
  }
  # drop non-varying Xs (e.g. interactions bw mutually exclusive dummies)
  varyvar = apply(X, 2, function(col) length(unique(col)) > 1)
  X = X[, varyvar]
  # drop highly correlated Xs - this drops polynomials of binary variables
  corm = cor(X)
  hc = findCorr(corm, cutoff = corr_cut) # put any value as a "cutoff"
  hc = sort(hc)
  return(X[, -c(hc)])
}

#' prepare sparse X matrix with interactions
#' @description Prepare sparse matrix with  interactions.
#' @param data              data table / dataframe
#' @param k                order of interactions: defaults to pairwise interactions
#' @return                matrix with interactions
#' @export

interSparseM = function(data, k, corr_cut = 0.9) {
  X = Matrix::sparse.model.matrix(
    as.formula(glue("~.^{k} - 1")),
    data
  )
  # drop non-varying Xs (e.g. interactions bw mutually exclusive dummies)
  varyvar = apply(X, 2, function(col) length(unique(col)) > 1)
  X = X[, varyvar]
  # drop highly correlated Xs - this drops polynomials of binary variables
  corm = cor(X)
  hc = findCorr(corm, cutoff = corr_cut) # put any value as a "cutoff"
  hc = sort(hc)
  X[, -c(hc)]
}

# %% fast correlation matrix filtration
findCorr = function(x, cutoff = .90) {
  if (any(!complete.cases(x)))
    stop("The correlation matrix has some missing values.")
  averageCorr = colMeans(abs(x))
  averageCorr = as.numeric(as.factor(averageCorr))
  x[lower.tri(x, diag = TRUE)] = NA
  combsAboveCutoff = which(abs(x) > cutoff)
  colsToCheck = ceiling(combsAboveCutoff / nrow(x))
  rowsToCheck = combsAboveCutoff %% nrow(x)
  colsToDiscard = averageCorr[colsToCheck] > averageCorr[rowsToCheck]
  rowsToDiscard = !colsToDiscard
  deletecol = c(colsToCheck[colsToDiscard], rowsToCheck[rowsToDiscard])
  deletecol = unique(deletecol)
  deletecol
}
