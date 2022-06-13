#' Loads specific function or list of functions f from library l
#' @param l library name (needs to be installed)
#' @param f function name 
#' @export
fromLimportF = \(l, f) library(l, include.only = f, character.only = TRUE)


#' Loads a list of libraries, installs when necessary - Take 2
#' @param repo string containing repo to be passed to install.packages
#' @keywords libraries install
#' @export
#' @examples
#' \dontrun{
#' libreq(tidyverse, lfe, estimatr)
#' }

libreq <- function(..., repo='https://cloud.r-project.org/') {
    wants = as.character(match.call(expand.dots = FALSE)[[2]])
    has <- wants %in% rownames(installed.packages())
    if(any(!has)) install.packages(wants[!has], repos=repo,dependencies=TRUE)
    loaded = suppressMessages(lapply(wants,require,character.only=TRUE))
    print(cbind(wants, loaded))
}


#' Loads a list of libraries, installs when necessary
#' @param wants A list of R modules available on CRAN
#' @param repo string containing repo to be passed to install.packages
#' @keywords libraries install
#' @export
#' @examples
#' \dontrun{
#' load_or_install(c('tidyverse','Hmisc','glmnet'))
#' }

load_or_install <- function(wants, repo='https://cloud.r-project.org/') {
    # sample use :
    # load_or_install(c('tidyverse','glmnet','HMisc'))
    has <- wants %in% rownames(installed.packages())
    if(any(!has)) install.packages(wants[!has], repos=repo,dependencies=TRUE)
    loaded = suppressMessages(lapply(wants,require,character.only=TRUE))
    print(cbind(wants, loaded))
}
