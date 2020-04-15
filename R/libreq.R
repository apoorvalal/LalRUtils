#' Loads a list of libraries, installs when necessary - Take 2
#' @param repo string containing repo to be passed to install.packages
#' @keywords libraries install
#' @export
#' @examples
#' libreq(tidyverse, lfe, estimatr)

libreq <- function(..., repo='https://cloud.r-project.org/') {
    wants = as.character(match.call(expand.dots = FALSE)[[2]])
    has <- wants %in% rownames(installed.packages())
    if(any(!has)) install.packages(wants[!has], repos=repo,dependencies=TRUE)
    loaded = suppressMessages(lapply(wants,require,character.only=TRUE))
    print(cbind(wants, loaded))
}
