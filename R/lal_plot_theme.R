#' theme for nice looking plots with sensible defaults (no legend title, legend at the bottom)
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme()

lal_plot_theme <- function(fontfam = "IBM Plex Sans Condensed", fsize = 15, textangle = 0,...) {
  theme_minimal() +
  theme(
    text = element_text(family = fontfam, size = fsize),
    axis.text.x = element_text(angle = textangle),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}
