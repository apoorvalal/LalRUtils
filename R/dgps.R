#' Kang Schaefer (2007) DGP for 'mild' confounding
#' @param n nobs
#' @param τ true effect (10 by default)
#' @export
KS2007_dgp = function(n = 1000, τ = 10){
    Z = matrix(rnorm(4*n),ncol=4,nrow=n)
    d = as.data.table(Z)
    colnames(d) = paste0("z", 1:4)
    d[, id := 1:.N][,
      pscore_lin := z1 - 0.5 * z2 + 0.25*z3 + 0.1 * z4][,
      π := expit(pscore_lin)][,
      W := rbinom(1, 1, π), id][,
      `:=`(x1 = exp(z1)/2,
           x2 = z2/(1+exp(z1)),
           x3 = (z1*z3/25+0.6)^3,
           x4 = (z2+z4+20)^2)][,
      lin := 27.4*z1 + 13.7*z2 + 13.7*z3 + 13.7*z4][,
      Y0 :=  200 +      (-0.5) *    lin][,
      Y1 :=  200 + τ  + (1.5 - 0.5)*lin][,
      Y := W * Y1 + (1-W)*Y0]
    return(d)
}
