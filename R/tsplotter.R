#' Canned subroutine to plot time series for several variables
#' @param df A dataframe
#' @param timevar a string with a colname for the time variable
#' @keywords dataframe variable name categorical
#' @export
#' @examples
#' overall_ineq = tsplotter(overall) + labs(title='Overall Inequality')
tsplotter = function(df, timevar='year') {
    suppressMessages(library(tidyverse))
    meltdf <- reshape2::melt(df,id=timevar)
    variable='variable'
    value='value'
    p = ggplot(data = meltdf, aes_string(x = timevar,
                y = value, colour = variable, group = variable)) +
          geom_point(size=0.5) + geom_line() +
        lal_plot_theme()
    return(p)
}
