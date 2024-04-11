#' SM plot with a theme appropriate for the slope chart
#' @description
#' In this plot, all aspects except for the left-handed spine are missing.
#' This format is appropriate for the slope chart.
#'
#' @param legends
#' #' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.

#' @import ggplot2 cowplot
#'
#'
#' @export
#' @examples
#' \dontrun{
#' library(tidyverse)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_slope_theme()
#' }

sm_slope_theme <- function(legends = TRUE) {
  if (legends == T) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      ggplot2::theme(
        panel.grid.minor.x=ggplot2::element_blank(),
        panel.grid.major.x=ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(size = 0.5, linetype = "solid",
                                   colour = "black"),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(color = "black", size = 12),
        axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
        axis.text.x = ggplot2::element_text(vjust = -1, size = 12),
        panel.background = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold')
      )

  } else if (legends == F) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      ggplot2::theme(
         panel.grid.minor.x = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_blank(),
         panel.grid.minor.y = ggplot2::element_blank(),
         panel.grid.major.y = ggplot2::element_blank(),
         panel.border = ggplot2::element_blank(),
         axis.line.y = ggplot2::element_line(size = 0.6, linetype = "solid",
                                    colour = "black"),
         axis.ticks.x = ggplot2::element_blank(),
         axis.text.y = ggplot2::element_text(color = "black"),
         axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
         axis.text.x = ggplot2::element_text(vjust = -1),
         panel.background = ggplot2::element_blank(),
         axis.title.x = ggplot2::element_blank(),
         axis.text = ggplot2::element_text(color = "black", size = 12),
         legend.position = 'none',
         plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold')
      )
  }
}
