#' A theme with horizontal and vertical major and minor grids
#'
#' @description
#'
#' This theme has vertical and horizontal grids.
#'
#' @export
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param borders
#' If the border needs to be displayed, the input should be `TRUE`.
#' If the border is not needed, the input should be `FALSE`.
#'
#' @import ggplot2 cowplot
#'
#' @export
#' @return
#' Returns a background theme that has both horizontal and vertical major and minor grids (ggplot2 output).
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_hvgrid_minor()
#'
sm_hvgrid_minor <- function(legends = TRUE, borders = TRUE) {

  if (legends == TRUE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_grid() +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0, size = ggplot2::rel(1.)),
          panel.grid.major =  ggplot2::element_line(size = 0.3),
          panel.grid.minor =  ggplot2::element_line(size = 0.3),
          axis.ticks = ggplot2::element_blank(),
          plot.title =  ggplot2::element_text(size = ggplot2::rel(.85), hjust = 0.5, face = 'bold'),
          strip.background = ggplot2::element_rect(fill = NA, colour = NA),
          strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
          strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
          legend.text=ggplot2::element_text(size = ggplot2::rel(.78)),
          legend.title=ggplot2::element_text(size = ggplot2::rel(.78))
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.3),
          panel.grid.minor =  ggplot2::element_line(size = 0.3),
          axis.ticks = ggplot2::element_blank(),
          plot.title =  ggplot2::element_text(hjust = 0.5, face = 'bold'),
          strip.background = ggplot2::element_rect(fill = NA, colour = NA),
          strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
          strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
          legend.text=ggplot2::element_text(size = ggplot2::rel(1.1)),
          legend.title=ggplot2::element_text(size = ggplot2::rel(1.1))
        )
    }
  } else if (legends == FALSE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_grid() +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.3),
          panel.grid.minor =  ggplot2::element_line(size = 0.3),
          axis.ticks = ggplot2::element_blank(),
          plot.title =  ggplot2::element_text(size = ggplot2::rel(.85), hjust = 0.5, face = 'bold'),
          strip.background = ggplot2::element_rect(fill = NA, colour = NA),
          strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
          strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
          legend.position = 'none'
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.3),
          panel.grid.minor =  ggplot2::element_line(size = 0.3),
          axis.ticks = ggplot2::element_blank(),
          plot.title =  ggplot2::element_text(hjust = 0.5, face = 'bold'),
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
          strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
          legend.position = 'none'
        )
    }
  }
}
