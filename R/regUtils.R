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
