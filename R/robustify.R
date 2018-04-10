####################################################
#' Replaces the standard errors (t and p vals)in FELM model object with robust
#' so that passing it on to stargazer spits out correct SEs
#' Only necessary for non-clustered SE; FELM automatically calculates
#' clustered standard errors if Y ~ X | FE | IV | Cluster specified
#' @param felm object
#' @export
#' @keywords hc0 robust heteroskedasticity output
#' @examples
#' robustify(felm(y~x,data=df))

# returns lm summary object with cluster-robust standard errors
robustify <- function(model){
    model$se    = model$rse
    model$tval  = model$rtval
    model$pval  = model$rpval
    return(model)
}
