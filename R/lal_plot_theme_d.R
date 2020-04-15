#' Fork of hrbrthemes::theme_ft_rc() with altered legend options and dots
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme_d()

lal_plot_theme_d <- function(fontfam = "IBM Plex Sans Condensed", fsize = 15, textangle = 0,...) {
  suppressMessages(library(hrbrthemes))
  theme_ft_rc() +
  theme(
    text = element_text(family = fontfam, size = fsize),
    axis.text.x = element_text(angle = textangle),
    legend.title = element_blank(),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}
