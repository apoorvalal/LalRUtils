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
