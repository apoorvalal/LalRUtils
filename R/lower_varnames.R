#' Lower variable names in a dataframe
#' @param df A dataframe
#' @keywords dataframe variable name
#' @export
#' @examples
#' lower_varnames(dataframe_with_bad_varnames)

# Converts all varnames into lowercase - designed for shouty SAS datasets
lower_varnames <- function(df) {
    names(df)=tolower(names(df))
    return(df)
}
