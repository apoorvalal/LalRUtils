#' Stitches together formula for use in lm/glm
#' @param depvar The dependent variable
#' @param rhs_vars List of independent variables (continuous/dummy variables)
#' @param factors List of factor variables (NULL by default)
#' @keywords dataframe variable name
#' @export
#' @examples
#' fml1 = formula_stitcher('wage',c('age','experience','married'),c('ethnicity','sector'))
#' lm1 <- lm(fml1,data=CPS1985)


formula_stitcher <- function(depvar,
                            rhs_vars,
                            factors=NULL){
    if (!is.null((factors))) {
        lapply(factors,as.factor)
        fml <- as.formula(paste0(depvar,'~',
                        paste((rhs_vars),collapse='+'),'+',
                        paste('factor(',factors,')',collapse='+',sep = '')))
    } else {
        fml <- as.formula(paste0(depvar,'~',
                        paste((rhs_vars),collapse='+')))
    }
    return(fml)
}
