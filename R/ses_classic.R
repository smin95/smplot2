#' SES classical theme.
#'
#' @description
#' It has x and y axis but no grids.
#' For more information, please visit \url{https://www.ses21.com}.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#' @export
#' @import ggplot2 cowplot
#'
#'
ses_classic <- function(legends = FALSE) {
  if (legends == TRUE) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      cowplot::theme_half_open() +
      theme(axis.text = element_text(size = rel(.85), color = "black"),
            axis.title.y = element_text(size = rel(.85), color = "black"),
            axis.title.x = element_text(size = rel(.85), color = "black", vjust = -1),
            axis.text.x=element_text(vjust= 0, size = rel(1)),
            legend.text = ggplot2::element_text(size= ggplot2::rel(.78)),
            legend.title = ggplot2::element_text(size=ggplot2::rel(.78)))
  } else if (legends == FALSE) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      cowplot::theme_half_open() +
      theme(axis.text = element_text(size = rel(.85), color = "black"),
            axis.title.y = element_text(size = rel(.85), color = "black"),
            axis.title.x = element_text(size = rel(.85), color = "black", vjust = -1),
            axis.text.x = element_text(vjust= 0, size = rel(1)),
            legend.position = 'none')
  }
}
