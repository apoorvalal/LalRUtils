####################################################
#' Returns summary for lm object with robust standard errors
#' @param vcov matrix from sandwich library
#' @export
#' @keywords hc0 cluster robust heteroskedasticity
#' @examples
#' \dontrun{
#' se_maker(vcovCL(m, cluster = df$clustervar))
#' }

# converts complicated (multi/clustered) variance covariance matrices
# into standard errors to be attached to lm objects
se_maker <- function(vcovmat) {
    s = sqrt(diag(vcovmat))
    return(s)
}


####################################################
#' Returns summary for lm object with robust standard errors
#' @param lm object
#' @export
#' @keywords hc0 robust heteroskedasticity
#' @examples
#' robust_se(lm(mpg ~ wt, mtcars))

# returns vector of robust standard errors
robust_se <- function(model){
    suppressPackageStartupMessages(library(sandwich))
    s <- sqrt(diag(vcovHC(model)))
    return(s)
}


####################################################
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
