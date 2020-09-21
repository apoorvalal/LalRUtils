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
