#' A minimal theme (no grid) with borders.
#'
#' @description
#'
#' This theme has no major grid.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @import ggplot2 cowplot
#' @return
#' Returns a background theme that has no grids (ggplot2 output).
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' library(smplot2)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_minimal()
#'

sm_minimal <- function(legends = FALSE) {

  if (legends == TRUE) {
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
        plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold')
      )
  } else if (legends == FALSE) {
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
        plot.title = ggplot2::element_text(hjust = 0.5, face = 'bold')
      )

  }
}


