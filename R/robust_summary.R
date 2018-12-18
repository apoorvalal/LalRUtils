####################################################
#' Returns summary for lm object with robust standard errors
#' @param lm object
#' @export
#' @keywords hc0 robust heteroskedasticity
#' @examples
#' robust_summary(lm(y~x,data=df))

# returns lm summary object with cluster-robust standard errors
robust_summary <- function(model){
    suppressPackageStartupMessages(library(sandwich))
    s <- summary(model)
    s$coefficients[, 2] <- sqrt(diag(vcovHC(model)))
    return(s)
}

