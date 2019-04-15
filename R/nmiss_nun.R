#' Table of missing value counts and number of unique values
#' @param df A dataframe
#' @keywords dataframe variable name missing
#' @export
#' @examples
#' nmiss_nun(mtcars)
nmiss_nun <- function(df) {
    suppressMessages(library(purrr))
    cat('-------------------------\n')
    cat('--- Missing Values ------\n')
    print(map(df, ~sum(is.na(.))))
    cat('-------------------------\n')
    cat('---- Unique Values ------\n')
    print(map(df, ~length(unique(.))))
}
