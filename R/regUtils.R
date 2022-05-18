# %%
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

# %% ####################################################
#' return cross-fit predictions from glmnet object
#' @param m glmnet model object fit with keep = T
#' @return vector of cross-fit predictions
#' @export
fitGet = function(m){
  m$fit.preval[, !is.na(colSums(m$fit.preval))][, # slice to nonmissing cols
    m$lambda[!is.na(colSums(m$fit.preval))] == m$lambda.min] # match lambda
}

# %%
#' Partial out controls and fixed effects and return residualised outcome and treatment
#' @param y outcome
#' @param a primary rhs variable
#' @param x controls
#' @param d fixed effects
#' @param df dataframe
#' @keywords Frisch-Waugh-Lovell partial out
#' @export
#' @examples
#'\dontrun{
#' residualise('mpg', 'wt', 'cyl', mtcars)
#'}
residualise = function(y, a, x = "1", d = "0", df){
  require(fixest)
  y_tilde = feols(formula_fixest(y, X = x, D = d), df)$residuals
  a_tilde = feols(formula_fixest(a, X = x, D = d), df)$residuals
  data.frame(y_tilde, a_tilde)
}


#' Abbreviated Regression Summary
#' Generic summaries for lm, glm and mer objects
#' This generic function provides an abbreviated regression output containing
#' the more useful information. Users wanting to see more are advised to use
#' \code{summary()}
#'
#' @name sumary
#' @docType methods
#' @aliases sumary sumary,lm-method sumary,glm-method sumary,merMod-method
#' @param object An lm, glm or mer object returned from lm(), glm() or lmer()
#' respectively
#' @param ... further arguments passed to or from other methods.
#' @return returns the same as \code{summary()}
#' @author Julian Faraway
#' @seealso \code{\link[base]{summary}}, \code{\link[stats]{lm}},
#' \code{\link[stats]{glm}}, \code{\link[lme4]{lmer}}
#' @keywords regression
#' @examples
#'
#' data(stackloss)
#' object <- lm(stack.loss ~ .,stackloss)
#' sumary(object)
#'
#' @exportMethod  sumary
if (!isGeneric("sumary")) {
    setGeneric("sumary",
               function(object, ...)
               standardGeneric("sumary"))
}

#'
#' @docType methods
#' @rdname sumary-methods
#' @export
setMethod("sumary", signature(object = "lm"),
  \(object) {
    digits <- options()$digits
    summ <- summary (object)
    sigma.hat <- summ$sigma
    r.squared <- summ$r.squared
    coef <- summ$coef[,,drop=FALSE]
    n <- summ$df[1] + summ$df[2]
    p <- summ$df[1]
    if (nsingular <- summ$df[3] - summ$df[1]) cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", sep = "")
    printCoefmat(coef,signif.stars=FALSE)
    cat("\n")
    cat (paste ("n = ", n, ", p = ", p,
      ", Residual SE = ", format(round(sigma.hat, digits-2),nsmall=digits-2),
      ", R-Squared = ", format(round(r.squared, 2)), "\n", sep=""))
    invisible(summ)
  }
)

#' @docType methods
#' @rdname sumary-methods
#' @export
setMethod("sumary", signature(object = "glm"),
    \(object, dispersion=NULL) {
        digits <- options()$digits
        summ <- summary(object, dispersion = dispersion)
        n <- summ$df[1] + summ$df[2]
        p <- summ$df[1]
        coef <- summ$coef[,,drop=FALSE]
        printCoefmat(coef,signif.stars=FALSE)
        cat("\n")
        if (summ$dispersion != 1) {
            cat(paste0("Dispersion parameter = ", fround(summ$dispersion,digits-2),"\n"))
        }
        cat(paste0("n = ", n, " p = ", p,"\n"))
        cat(paste0("Deviance = ",fround(summ$deviance,digits-2),
                   " Null Deviance = ", fround(summ$null.deviance,digits-2),
                   " (Difference = ", fround(summ$null.deviance-summ$deviance,digits-2), ")"),"\n")
    return(invisible(summ))
  }
)


#' Partial Residual Plot
#'
#' Makes a Partial Residual plot
#'
#' @name prplot
#' @param g An object returned from lm()
#' @param i index of predictor
#' @return none
#' @author Julian Faraway
#' @keywords regression
#' @examples
#'
#' data(stackloss)
#' g <- lm(stack.loss ~ .,stackloss)
#' prplot(g,1)
#'
#' @export prplot
prplot <- function(g,i) {
# Partial residuals plot for predictor i
  xl <- attributes(g$terms)$term.labels[i]
  yl <- paste("beta*",xl,"+res",sep="")
  x <- model.matrix(g)[,i+1]
  plot(x,g$coeff[i+1]*x+g$res,xlab=xl,ylab=yl)
  abline(0,g$coeff[i+1])
  invisible()
}
