####################################################
#' Scatterplot with regression line and densites by grouping variable. Use this to evaluate overlap.
#' @param df dataframe
#' @param xvar x variable
#' @param yvar y variable
#' @param zvar z variable (coerced to factor)
#' @param title plot title
#' @export
#' @keywords seaborn Scatterplot
#' @importFrom ggExtra ggMarginal
#' @examples
#' regplot_dens(mtcars, wt, mpg, am)
regplot_dens = function(df, xvar, yvar, zvar, title = ""){
  require(ggExtra); require(ggplot2)
  p0 = ggplot(df, aes(x = {{xvar}}, y = {{yvar}})) +
    geom_point(aes(colour = as.factor({{zvar}}))) +
    geom_smooth(method = 'lm', alpha = 0.2) +
    scale_colour_brewer(palette = "Set1") +
    ggtitle(title)
  p = ggMarginal(p0, type = "density", size = 7, groupColour = T, groupFill = T)
  return(p)
}
