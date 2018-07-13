#' Summary table with percentages for categorical variables
#' @param df A dataframe , var A column in said dataframe
#' @keywords dataframe variable name categorical
#' @export
#' @examples
#' freq_table(dataframe, 'categorical_var')
freq_table = function(df, var) {
    suppressMessages(library(tidyverse))
    ft = df %>% count_(var) %>%
      mutate(prop=prop.table(n)) %>%
      arrange(desc(prop))
    return(ft)
}
