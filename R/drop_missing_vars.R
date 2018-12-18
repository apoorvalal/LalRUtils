#' Drop variables with all NAs in dataframe
#' @param dataframe A dataframe
#' @return dataframe with empty variables dropped
#' @export
#' @keywords dataframe variable
#' @examples
#' clean_dataframe = drop_missing_vars(dataframe_with_empty_vars)

########################################################
## Drop variables with all missing values from dataframe
########################################################
drop_missing_vars <- function(df){
    # sample use :
    # clean_dataframe <- drop_missing_vars(raw_dataframe)
    missing_list <- sapply(df, function(x)all(is.na(x)))
    table(missing_list)
    clean_df <- df[!missing_list]
    return(clean_df)
}
