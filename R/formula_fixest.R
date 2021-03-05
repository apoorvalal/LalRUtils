#' Stitches together formula for use in fixest
#' @param y The dependent variable
#' @param X vector of controls
#' @param W treatment variable
#' @param D vector of factor variables to be partialed out
#' @param Z vector of instruments
#' @keywords regression linear model variable name
#' @export
#' @examples
#' formula_fixest(y='mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' formula_fixest(y='mpg', X = c('hp', 'drat'), W = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'))

formula_fixest = function (y, X, W = NULL, D = NULL, Z = NULL) {
    # 'second stage' step
    if (!is.null(W) & is.null(Z)) { # separate treatment dummy only
      fixest_ss = paste(c(y, paste(c(W, X), collapse = "+")), collapse = "~")
    } else { # no instrumented variable
      fixest_ss = paste(c(y, paste(X, collapse = "+")), collapse = "~")
    }
    # FEs
    if (!is.null(D)) facs = paste(D, collapse = "+") else  facs = "0"
    # first stage
    if (!is.null(Z)) {
        fixest_fs = paste(c(paste(c(W, paste(Z, collapse = "+")),
            collapse = "~")), collapse = "")
      # return formula
      as.formula(paste(c(fixest_ss, facs, fixest_fs), collapse = "|"))
    } else {
      as.formula(paste(c(fixest_ss, facs), collapse = "|"))
    }
}
