#' Loads a list of libraries, installs when necessary
#' @param wants A list of R modules available on CRAN
#' @keywords libraries install
#' @export
#' @examples
#' load_or_install(c('tidyverse','Hmisc','glmnet'))

########################################################
## Loads a list of R libraries, install if necessary
########################################################
load_or_install <- function(wants) {
    # sample use :
    # load_or_install(c('tidyverse','glmnet','HMisc'))
    has <- wants %in% rownames(installed.packages())
    if(any(!has)) install.packages(wants[!has],
        repos='https://cloud.r-project.org/',dependencies=TRUE)
    suppressMessages(lapply(wants,require,character.only=TRUE))
}
