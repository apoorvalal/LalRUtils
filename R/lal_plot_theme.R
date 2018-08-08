#' Theme for nice looking plots
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme()

lal_plot_theme <- function(...) {
  theme_bw() +
  theme(
    text = element_text(family = "Roboto Condensed"),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}
