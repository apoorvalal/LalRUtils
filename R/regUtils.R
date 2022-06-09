# %%
#' Replaces the standard errors (t and p vals)in FELM model object with robust SE
#' @param felm object
#' @export
#' @keywords hc0 robust heteroskedasticity output
#' @examples
#' \dontrun{
#' robustify(felm(y~x,data=df))
#' }
# returns lm summary object with cluster-robust standard errors
robustify <- function(model){
    model$se    = model$rse
    model$tval  = model$rtval
    model$pval  = model$rpval
    return(model)
}

# %% ####################################################
#' return cross-fit predictions from glmnet object
#' @param m glmnet model object fit with keep = T
#' @return vector of cross-fit predictions
#' @export
fitGet = function(m){
  m$fit.preval[, !is.na(colSums(m$fit.preval))][, # slice to nonmissing cols
    m$lambda[!is.na(colSums(m$fit.preval))] == m$lambda.min] # match lambda
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
#'\dontrun{
#' residualise('mpg', 'wt', 'cyl', mtcars)
#'}
residualise = function(y, a, x = "1", d = "0", df){
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
#' g <- lm(stack.loss ~ .,stackloss)
#' prplot(g,1)
#'
#' @export prplot
prplot <- function(g,i) {
# Partial residuals plot for predictor i
  xl <- attributes(g$terms)$term.labels[i]
  yl <- paste("beta*",xl,"+res",sep="")
  x <- model.matrix(g)[,i+1]
  plot(x,g$coeff[i+1]*x+g$res,xlab=xl,ylab=yl)
  abline(0,g$coeff[i+1])
  invisible()
}

# %%
#' prepare X matrix with flexible interactions and polynomials
#' @description Prepare matrix with polynomial basis expansion and/or interactions
#
#' @param dat              data table / dataframe
#' @param dummies          Names of dummy vars
#' @param continuouses     Names of continuous vars to modify fn form
#' @param corr_cut         cutoff for correlation threshold to drop one of the vars (default = 0.9)
#' @param k                order of interactions: defaults to pairwise interactions
#' @param m                order of functions: defaults to quadratic functions
#' @param raw              raw or orthogonal bases
#' @return data.frame with base terms and interactions + basis
#' @export
#' @examples
#' data(lalonde.psid)
#' xn = setdiff(colnames(lalonde.psid) , c("treat", "re78"))
#' co = c("age", "education", "re74", "re75")
#' bi = c("black", "hispanic", "married", "nodegree", "u74", "u75")
#' Xx = prepBMatrix(lalonde.psid[, xn], co, bi)
#' colnames(lalonde.psid[, xn])  |> length()
#' colnames(Xx)                  |> length()
#' @importFrom glue glue
#' @importFrom caret findCorrelation

prepBMatrix = function(dat,
    continuouses,
    dummies = NULL,
    corr_cut = 0.90, k = 2, m = 2, raw = F) {
  dat = as.data.frame(dat)
  # concat names
  controls = c(dummies, continuouses)
  # functional form changes for continuous vars
  # polynomials (orthogonal by default)
  polyfml = paste(paste0("poly(", continuouses,",", m, ", raw=", raw, ")"),
          collapse = " + ")
  powmat = model.matrix(as.formula(paste0("~ -1 + ", polyfml)), dat[, continuouses])
  # check for negative values in continuous vars before logging
  n_pos = apply(dat[, continuouses], 2, \(x) sum(x>0))
  if (max(n_pos < nrow(dat)) == 0){ # log allowed
    logmat = log1p(dat[, continuouses])
    names(logmat) = paste0("log", continuouses)
    polynomials = cbind(logmat, powmat)
  } else{ # no log
    polynomials = powmat
  }
  # cbind base terms with polynomials
  data = cbind(dat[, controls], polynomials)
  if (k > 1){
    # all n-way interactions with polynomials and dummies
    X = model.matrix(as.formula(glue::glue("~.^{k} - 1")), data)
  } else {
    X = as.matrix(data)
  }
  # drop non-varying Xs
  varyvar = apply(X, 2, function(col) nunique(col) > 1)
  X = X[, varyvar]
  # drop highly correlated Xs - this drops polynomials of binary variables
  corm = cor(X)
  hc = caret::findCorrelation(corm, cutoff=corr_cut) # put any value as a "cutoff"
  hc = sort(hc)
  X[,-c(hc)]
}
# %%
