#' plot binary treatment status for time series for panel dataset
#' @param data     dataframe
#' @param time     bare name of time (not in quotes)
#' @param id       bare name of unit
#' @param status   bare name of treatment
#' @keywords dataframe panel diff-in-diff
#' @export
#' @examples
#'\dontrun{
#' panel_treat_plot(california_prop99, Year, State, treated)
#'}

panel_treat_plot = function(data, time, id, status) {
  tiles_plot <- ggplot(data,
      aes(x = {{time}}, y = {{id}}, fill = as.factor({{status}}) ) ) +
    geom_tile(color = "white", size = 1) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = c("#4285F4", "#DB4437", "#F4B400")) +
    theme_classic(base_size = 14) +
    lal_plot_theme() + labs(fill = "Treatment   :")
  return(tiles_plot)
}
