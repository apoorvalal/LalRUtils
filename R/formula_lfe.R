#' Stitches together formula for use in felm
#' @param y The dependent variable
#' @param X vector of Xs
#' @param w endogenous variable (only included if first stage is specified)
#' @param D vector of factor variables to be partialed out
#' @param Z vector of instruments
#' @param C vector of variables cluster standard errors
#' @keywords regression linear model variable name
#' @export
#' @examples
#' formula_lfe(y='mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' formula_lfe(y='mpg', X = c('hp', 'drat'), w = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'), C = c('cyl', 'wt'))
formula_lfe <- function(y, X, w = NULL, D = NULL, Z = NULL, C = NULL){
  felm_ss = paste(c(y,paste(X, collapse = '+')), collapse='~')
  # instrument
  if (!is.null(Z)){
  felm_fs = paste(c('(', paste(c(w, paste(Z, collapse='+')),
                  collapse='~'), ')'), collapse = '')
  } else {
    felm_fs = '0'
  }
  # dummies
  if (!is.null(D)){
    facs = paste(D, collapse = "+")
  } else {
    facs = "0"
  }
  # clusters
  if (!is.null(C)){
    clusts = paste(C, collapse = "+")
  } else {
    clusts = "0"
  }
  as.formula(paste(c(felm_ss, facs , felm_fs, clusts), collapse = '|'))
}
