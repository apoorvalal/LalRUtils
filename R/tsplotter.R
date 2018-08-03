#' Canned subroutine to plot time series for several variables
#' @param df A dataframe
#' @param timevar a string with a colname for the time variable
#' @keywords dataframe variable name categorical
#' @export
#' @examples
#'     overall_ineq = tsplotter(overall) + labs(title='Overall Inequality')
tsplotter = function(df, timevar='year') {
    suppressMessages(library(tidyverse))
    meltdf <- reshape2::melt(df,id=timevar)
    p = ggplot(data = meltdf, aes(x=year, y = value,
            colour=variable, group = variable)) +
        geom_point() + geom_line() +
        theme(legend.position="bottom")
    return(p)
}
