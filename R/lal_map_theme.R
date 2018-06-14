#' Theme for maps
#' @param ggplot object
#' @keywords maps ggplot spatial
#' @export
#' @examples
#' map_ggp_object + lal_map_theme()
lal_map_theme <- function(...) {
  library(ggthemes)
  theme_fivethirtyeight() +
  theme(
    text = element_text(family = "Liberation Sans", color = "#22211d"),
    legend.position='bottom',
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    ...
  )
}
