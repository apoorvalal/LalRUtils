#' Stitches together formula for use in felm
#' @param y The dependent variable
#' @param X vector of controls
#' @param W treatment variable
#' @param D vector of factor variables to be partialed out
#' @param Z vector of instruments
#' @param C vector of variables cluster standard errors (multi-way permitted by LFE)
#' @keywords regression linear model variable name
#' @export
#' @examples
#' formula_lfe(y='mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' formula_lfe(y='mpg', X = c('hp', 'drat'), W = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'), C = c('cyl', 'wt'))
formula_lfe= function (y, X, W = NULL, D = NULL, Z = NULL, C = NULL) {
    # 'second stage' step
    if (!is.null(W) & is.null(Z)) { # separate treatment dummy only
      felm_ss = paste(c(y, paste(c(W, X), collapse = "+")), collapse = "~")
    } else { # no instrumented variable
      felm_ss = paste(c(y, paste(X, collapse = "+")), collapse = "~")
    }
    # first stage
    if (!is.null(Z)) {
        felm_fs = paste(c("(", paste(c(W, paste(Z, collapse = "+")),
            collapse = "~"), ")"), collapse = "")
    } else { # no instrument
        felm_fs = "0"
    }
    # FEs
    if (!is.null(D)) {
        facs = paste(D, collapse = "+")
    } else {
        facs = "0"
    }
    # clusters
    if (!is.null(C)) {
        clusts = paste(C, collapse = "+")
    } else {
        clusts = "0"
    }
    # return formula
    as.formula(paste(c(felm_ss, facs, felm_fs, clusts), collapse = "|"))
}
