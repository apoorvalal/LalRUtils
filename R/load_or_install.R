#' Loads a list of libraries, installs when necessary
#' @param wants A list of R modules available on CRAN
#' @param repo string containing repo to be passed to install.packages
#' @keywords libraries install
#' @export
#' @examples
#' load_or_install(c('tidyverse','Hmisc','glmnet'))

load_or_install <- function(wants, repo='https://cloud.r-project.org/') {
    # sample use :
    # load_or_install(c('tidyverse','glmnet','HMisc'))
    has <- wants %in% rownames(installed.packages())
    if(any(!has)) install.packages(wants[!has], repos=repo,dependencies=TRUE)
    loaded = suppressMessages(lapply(wants,require,character.only=TRUE))
    print(cbind(wants, loaded))
}
