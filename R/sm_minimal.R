#' SM minimal theme (no grid).
#'
#' @description
#'
#' This theme has no major grid.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @import ggplot2 cowplot
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(tidyverse)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_minimal()
#' }
#'

sm_minimal <- function(legends = TRUE, borders = TRUE) {

  if (legends == TRUE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_hgrid()
        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y =  ggplot2::element_blank(),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black",vjust = -1),
          axis.text.x= ggplot2::element_text(vjust= 0),
          axis.line.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size= ggplot2::rel(.78)),
          legend.title = ggplot2::element_text(size=ggplot2::rel(.78)),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +

        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y =   ggplot2::element_blank(),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black",vjust = -1),
          axis.text.x = ggplot2::element_text(vjust= 0),
          legend.text = ggplot2::element_text(size= ggplot2::rel(1.1)),
          legend.title = ggplot2::element_text(size=ggplot2::rel(1.1)),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    }
  } else if (legends == FALSE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_hgrid()
        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black",vjust = -1),
          axis.text.x = ggplot2::element_text(vjust= 0),
          axis.line.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = 'none',
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +

        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black",vjust = -1),
          axis.text.x = ggplot2::element_text(vjust= 0),
          legend.position = 'none',
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    }
  }
}


