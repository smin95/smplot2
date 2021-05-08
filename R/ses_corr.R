#' SES plot without a border (useful for correlation plots)
#'
#' @description
#'
#' A graph with no border is plotted. It has major vertical and horizontal grids. This is useful for plotting correlation.
#' For more information, please visit \url{https://www.ses21.com}.
#'
#' @export
#' @param ...
#' No input is required.
#'
#'
ses_corr <- function() {

  ggplot2::theme_bw(base_size = 10, base_family = '') +
    cowplot::theme_minimal_grid() +

    ggplot2::theme(
      axis.text =  ggplot2::element_text(size = rel(.85), color = "black"),
      axis.title.y =  ggplot2::element_text(size = rel(.85), color = "black"),
      axis.title.x =  ggplot2::element_text(size = rel(.85), color = "black", vjust = -1),
      axis.text.x=  ggplot2::element_text(vjust= 0, size = rel(1.)),
      panel.grid.major =  ggplot2::element_line(size = 0.4),
      plot.title =  ggplot2::element_text(hjust = 0.5)
      )
}
