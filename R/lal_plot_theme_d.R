#' Wrapper for hrbrthemes::theme_ft_rc() with bigger font, altered legend options and dots
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme_d()

lal_plot_theme_d <- function(fontfam = "IBM Plex Sans Condensed", fsize = 15, axsize = 13, textangle = 0, ...) {
  suppressMessages(library(hrbrthemes))
  theme_modern_rc(base_family = fontfam, subtitle_family = fontfam,
    axis_title_just = 'c', base_size = fsize) +
  theme(
    text = element_text(family = fontfam, size = fsize),
    axis.text.x = element_text(angle = textangle, size = axsize),
    axis.text.y = element_text(face = "bold", size = axsize),
    strip.text = element_text(color = 'white'),
    legend.position='bottom',
    panel.border = element_blank(),
    ...
  )
}
