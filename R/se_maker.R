####################################################
#' Returns summary for lm object with robust standard errors
#' @param vcov matrix from sandwich library
#' @export
#' @keywords hc0 cluster robust heteroskedasticity
#' @examples
#' se_maker(vcovCL(m, cluster = df$clustervar))

# converts complicated (multi/clustered) variance covariance matrices
# into standard errors to be attached to lm objects
se_maker <- function(vcovmat) {
    s = sqrt(diag(vcovmat))
    return(s)
}
