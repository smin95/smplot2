#' A SM classical theme.
#'
#' @description
#' It has x and y axis but no grids.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#' @export
#' @import ggplot2 cowplot
#' @return
#' Returns a background theme as a ggplot2 object.
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(smplot2)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_classic()
#' }
#'
sm_classic <- function(legends = FALSE) {
  if (legends == TRUE) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      cowplot::theme_half_open() +
      theme(axis.text = element_text(size = rel(.85), color = "black"),
            axis.title.y = element_text(size = rel(.85), color = "black"),
            axis.title.x = element_text(size = rel(.85), color = "black", vjust = -1),
            axis.text.x=element_text(vjust= 0, size = rel(1)),
            plot.title =  ggplot2::element_text(hjust = 0.5, size = rel(0.85), face = 'bold'),
            legend.text = ggplot2::element_text(size= ggplot2::rel(.78)),
            legend.title = ggplot2::element_text(size=ggplot2::rel(.78)))
  } else if (legends == FALSE) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      cowplot::theme_half_open() +
      theme(axis.text = element_text(size = rel(.85), color = "black"),
            axis.title.y = element_text(size = rel(.85), color = "black"),
            axis.title.x = element_text(size = rel(.85), color = "black", vjust = -1),
            axis.text.x = element_text(vjust= 0, size = rel(1)),
            plot.title =  ggplot2::element_text(hjust = 0.5, size = rel(0.85), face = 'bold'),
            legend.position = 'none')
  }
}
