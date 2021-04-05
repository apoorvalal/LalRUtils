#' theme for nice looking plots with sensible defaults (no legend title, legend at the bottom)
#' @param ggplot object
#' @keywords ggplot plots
#' @export
#' @examples
#' plot_ggp_object + lal_plot_theme()

lal_plot_theme = function (fontfam = "IBM Plex Sans Condensed",
      fsize = 14, textangle = 0, ...) {
  theme_minimal() +
    theme(
      text                  = element_text(family = fontfam, size = fsize),
      axis.text.x           = element_text(angle = textangle),
      axis.ticks            = element_line(color = "grey92"),
      plot.title            = element_text(size = 18, face = "bold"),           # title fsize
      plot.title.position   = "plot",                                           # left align
      plot.caption          = element_text(size = 9, margin = margin(t = 15)),  # caption fsize
      plot.caption.position = "plot",                                           # right align
      plot.subtitle         = element_text(size = 12, color = "grey30"),        # subtitle
      legend.position       = "top",
      legend.text           = element_text(color = "grey30"),
      legend.title          = element_text(size = 12),
      panel.border          = element_blank(),
      panel.grid.minor      = element_blank(),
      ...)
}
