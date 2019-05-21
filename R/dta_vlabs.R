#' Small summary table with stata variable labels from DTA file
#' @param df dataframe to check balance
#' @keywords stata summary labels dataframe
#' @export
#' @examples
#' dta_vlabs(df)
dta_vlabs <- function(dta) {
    labels <- sapply(dta, function(x) attr(x, "label"))
    tibble(name = names(labels),
             label = labels)
}
