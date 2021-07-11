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
