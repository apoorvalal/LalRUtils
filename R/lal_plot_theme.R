#' Theme for nice looking plots
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme()

lal_plot_theme <- function(fontfam="Roboto Condensed",...) {
  theme_bw() +
  theme(
    text = element_text(family = fontfam),
    # axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}
