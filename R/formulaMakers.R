#' Stitches together formula for use in fixest::feols
#' @param y The dependent variable
#' @param X vector of controls
#' @param W treatment variable
#' @param D vector of factor variables to be partialed out
#' @param Z vector of instruments
#' @keywords regression linear model variable name
#' @export
#' @examples
#' formula_fixest(y = 'mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' formula_fixest(y = 'mpg', X = c('hp', 'drat'), W = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'))
#'
formula_fixest = function(y, X, W = NULL, D = NULL, Z = NULL) {
  # 'second stage' step
  if (!is.null(W) & is.null(Z)) { # separate treatment dummy only
    fixest_ss = paste(c(y, paste(c(W, X), collapse = "+")), collapse = "~")
  } else { # no instrumented variable
    fixest_ss = paste(c(y, paste(X, collapse = "+")), collapse = "~")
  }
  # FEs
  if (!is.null(D)) facs = paste(D, collapse = "+") else facs = "0"
  # first stage
  if (!is.null(Z)) {
    fixest_fs = paste(c(paste(c(W, paste(Z, collapse = "+")),
      collapse = "~"
    )), collapse = "")
    # return formula
    as.formula(paste(c(fixest_ss, facs, fixest_fs), collapse = "|"))
  } else {
    as.formula(paste(c(fixest_ss, facs), collapse = "|"))
  }
}


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
#' formula_lfe(y = 'mpg', X = c('hp', 'drat'), D = c('wt', 'vs'))
#' formula_lfe(y = 'mpg', X = c('hp', 'drat'), W = 'gear', Z = c('cyl', 'carb'), D = c('wt', 'vs'), C = c('cyl', 'wt'))
formula_lfe = function(y, X, W = NULL, D = NULL, Z = NULL, C = NULL) {
  # 'second stage' step
  if (!is.null(W) & is.null(Z)) { # separate treatment dummy only
    felm_ss = paste(c(y, paste(c(W, X), collapse = "+")), collapse = "~")
  } else { # no instrumented variable
    felm_ss = paste(c(y, paste(X, collapse = "+")), collapse = "~")
  }
  # first stage
  if (!is.null(Z)) {
    felm_fs = paste(c("(", paste(c(W, paste(Z, collapse = "+")),
      collapse = "~"
    ), ")"), collapse = "")
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


#' Stitches together formula for use in lm/glm
#' @param Y The dependent variable
#' @param X List of independent variables (continuous/dummy variables)
#' @param factors List of factor variables (NULL by default)
#' @keywords dataframe variable name
#' @export
#' @examples
#' formula_stitcher('wage', c('age', 'experience', 'married'), c('ethnicity', 'sector'))
#'
formula_stitcher = function(Y, X, factors = NULL) {
  if (!is.null((factors))) {
    lapply(factors, as.factor)
    fml = as.formula(paste0(
      Y, '~',
      paste((X), collapse = '+'), '+',
      paste('factor(', factors, ')', collapse = '+', sep = '')
    ))
  } else {
    fml = as.formula(paste0(
      Y, '~',
      paste((X), collapse = '+')
    ))
  }
  return(fml)
}


# %% ####################################################
#' construct formula with outcome and polynomial terms
#' @param  y outcome
#' @param  b binary variables
#' @param  c continuous vars (for polynomials)
#' @param  k polynomial order
#' @return formula
#' @export

fmla_kth = function(y, b, c, k = 2) {
  f = paste0(
    y, " ~ ", # depvar
    paste0(b, collapse = " + "), " + ", # binary variables
    paste0("poly(", c, ",", k, ", raw = T)", collapse = " + ") # continuous variables with kth order term
  ) |> as.formula()
}
