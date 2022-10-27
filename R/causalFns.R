#' Compute subclassification ATT and ATE
#' @param df data.table
#' @param x character vector of discrete covariates
#' @param y outcome
#' @param w treatment
#' @param debug boolean to return strata-specific estimates
#' @return list with ATE and ATT estimate in est and strata level averages in table
#' @export
#' @examples
#' data(lalonde.psid); setDT(lalonde.psid)
#' subclassify(lalonde.psid,
#'   x = c("u74", "u75"),
#'   y = "re78", w = "treat"
#' )
#' subclassify(lalonde.psid,
#'   x = c("married", "black", "hispanic", "u74", "u75"),
#'   y = "re78", w = "treat"
#' )
#'
subclassify = function(df, x, y = 're78', w = 'treat', debug = F) {
  if (!data.table::is.data.table(df)) {
    df = data.table::as.data.table(df)
  }
  N = nrow(df); N1 = sum(df[[w]])
  grpmeans = df[, list(grpmean = mean(get(y)), N = .N), by = c(x, w)]
  fml = as.formula(paste0(paste(x, collapse = "+"), "~", w))
  strata_level = dcast(grpmeans, fml, value.var = c("grpmean", "N"))
  strata_level[, `:=`(DiM = grpmean_1 - grpmean_0, N_k = N_1 + N_0)]
  ATE = strata_level[, sum(DiM * (N_k / N))]; ATT = strata_level[, sum(DiM * (N_1 / N1))]
  return(list(est = data.frame(ATE = ATE, ATT = ATT), table = strata_level))
}

# %%
#' Simple Double-ML implementation with cross-fitting
#' @param y The dependent variable
#' @param x Matrix of covariates
#' @param d treatment variable
#' @param D vector of factor variables to be partialed out
#' @param dreg function that wraps learner - e.g. glmnet - for pscore
#' @param yreg function that wraps learner - e.g. glmnet - for outcome regression
#' @param nfolds number of folds for cross fitting
#' @keywords double ml, causal inferenc
#' @export
#' @examples
#' \dontrun{
#' dreg = \(x, d) cv.glmnet(x, d, alpha = 1)
#' yreg = \(x, y) cv.glmnet(x, y, alpha = 1)
#' res = DMLReg(x = x, d = d, y = y, dreg = dreg, yreg = yreg, nfold = 5)
#' }
DMLReg = function(x, d, y, dreg, yreg, nfold = 5) {
  require(estimatr)
  # randomly split data into folds
  nobs = nrow(x)
  foldid = rep.int(1:nfold, times = ceiling(nobs / nfold))[sample.int(nobs)]
  I = split(1:nobs, foldid)
  # create residualized objects to fill
  ytil = dtil = rep(NA, nobs)
  # run the OOS orthogonalizations
  cat("fold: ")
  for (b in 1:length(I)) {
    dfit = dreg(x[-I[[b]], ], d[-I[[b]]])
    yfit = yreg(x[-I[[b]], ], y[-I[[b]]])
    dhat = predict(dfit, x[I[[b]], ], type = "response")
    yhat = predict(yfit, x[I[[b]], ], type = "response")
    dtil[I[[b]]] = drop(d[I[[b]]] - dhat)
    ytil[I[[b]]] = drop(y[I[[b]]] - yhat)
    cat(b, " ")
  }
  rfit = lm_robust(ytil ~ dtil)
  effect_se = summary(rfit)$coefficients[2, 1:2]
  return(list(res = effect_se, dtil = dtil, ytil = ytil))
}

# %%
#' Compute OLS with arbitrary vector of weights (possibly negative)
#' @param y Response vector
#' @param X A numeric data matrix
#' @param w vector of weights (same length as y)
#' @return Regression vector beta of length ncol(X).
#' @export
#' @import car
OLSw = function(y, X, w) {
  XtWX = car::wcrossprod(X, w = w)
  XtWy = car::wcrossprod(X, y, w = w)
  solve(XtWX) %*% XtWy
}

# %% ####################################################
#' Regression Adjustment estimation of ATE or ATT
#' @param df dataframe
#' @param y outcome name
#' @param w treatment name
#' @param xs vector of covariate names
#' @param estimand : runs Lin regression when "ATE" and Oaxaca-Blinder-Kitagawa when "ATT"
#' @return fixest model object with robust standard errors (can be summarised again with different clusters)
#' @import fixest
#' @export
#' @examples
#' data(lalonde.psid); data(lalonde.exp);
#' y = "re78"; w = "treat"; xs = setdiff(colnames(lalonde.psid), c(y, w))
#' cat("ATE in experimental sample \n")
#' reg_adjust(lalonde.exp, 'treat', 're78', xs, "ATE")
#' cat(" --------------------------- \n")
#' cat("ATT in obs sample \n")
#' reg_adjust(lalonde.psid, 'treat', 're78', xs, "ATT")
#' cat(" --------------------------- \n")
reg_adjust = function(df, w, y, xs, estimand = c("ATT", "ATE")) {
  estimand = match.arg(estimand)
  # for ATE, sweep out overall means, else sweep out treatment means
  if (estimand == "ATE") {
    d = df[, xs]
  } else {
    d = df[df[[w]] == 1, xs]
  }
  # take out means
  Xbar = apply(d[, xs], 2, mean); Xdemeaned = sweep(df[, xs], 2, Xbar)
  colnames(Xdemeaned) = paste0(colnames(Xdemeaned), "_dem")
  # concat columns
  regdf = cbind(df[, c(y, w)], df[, xs], Xdemeaned)
  # interacted regression
  f = as.formula(paste0(
    y, "~", w, "+",
    paste(xs, collapse = "+"), "+", # controls effects
    w, ":(", paste(colnames(Xdemeaned), collapse = "+"), ")" # treat effects
  ))
  m = fixest::feols(f, data = regdf, vcov = 'hc1')
  m
}
