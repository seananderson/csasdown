#' A ggplot2 theme
#'
#' A simple theme for ggplot2 that loosely resembles nicely themed plots from
#' base graphics.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param text_col Color for text.
#' @param panel_border_col Color for panel borders.
#'
#' @importFrom ggplot2 element_text element_rect element_blank theme_light theme
#'   rel
#' @importFrom grid unit
#'
#' @examples
#' p <- ggplot2::ggplot(mtcars) +
#'   ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg, colour = factor(gear))) +
#'   ggplot2::facet_wrap(~am)
#' p + theme_csas()
#'
#' @export
theme_csas <- function(base_size = 11, base_family = "", text_col = "grey20",
                      panel_border_col = "grey70") {
  half_line <- base_size / 2
  theme_light(base_size = base_size, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = text_col),
      strip.text.y = element_text(colour = text_col),
      axis.text = element_text(colour = text_col),
      axis.title = element_text(colour = text_col),
      legend.title = element_text(colour = text_col, size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = panel_border_col, linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = text_col),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = text_col, size = rel(1)),
      plot.subtitle = element_text(colour = text_col, size = rel(.85))
    )
}