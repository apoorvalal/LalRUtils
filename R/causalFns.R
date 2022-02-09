#' Compute subclassification ATT and ATE
#' @param df data.table
#' @param x character vector of discrete covariates
#' @param y outcome
#' @param w treatment
#' @param debug boolean to return strata-specific estimates
#' @export
#' @examples
#' \dontrun{
#' subclassify(mtcars, x = "cyl", y = "mpg", w = "foreign")
#' }

subclassify = function(df, x, y = 're78', w = 'treat', debug = F){
  require(data.table)
  if (!data.table::is.data.table(df)) {
    df <- data.table::as.data.table(df)
  }
  N = nrow(df); N1 = sum(df[[w]])
  grpmeans = df[, list(grpmean = mean(get(y)), N = .N), by = c(x, w)]
  fml = as.formula(paste0(paste(x, collapse = "+"), "~", w))
  strata_level = dcast(grpmeans, fml, value.var = c("grpmean", "N"))
  strata_level[, `:=`(DiM = grpmean_1 - grpmean_0, N_k = N_1 + N_0)]
  ATE = strata_level[, sum(DiM * (N_k/N))]; ATT = strata_level[, sum(DiM * (N_1/N1))]
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
#' dreg <- function(x,d){ cv.glmnet(x, d, alpha = 1) }
#' yreg <- function(x,y){ cv.glmnet(x, y, alpha = 1) }
#' res <- DMLReg( x=x, d=d, y=y, dreg=dreg, yreg=yreg, nfold=5)
#' }
DMLReg <- function(x, d, y, dreg, yreg, nfold=5) {
  require(estimatr)
  # randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- rep(NA, nobs)
  # run the OOS orthogonalizations
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
    dhat <- predict(dfit, x[I[[b]],], type="response")
    yhat <- predict(yfit, x[I[[b]],], type="response")
    dtil[I[[b]]] <- drop(d[I[[b]]] - dhat)
    ytil[I[[b]]] <- drop(y[I[[b]]] - yhat)
    cat(b," ")
  }
  rfit <- lm_robust(ytil ~ dtil)
  effect_se = summary(rfit)$coefficients[2, 1:2]
  return( list(res = effect_se, dtil=dtil, ytil=ytil) )
}
