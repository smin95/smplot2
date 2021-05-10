#' SES plot with a horizontal grid (useful for histogram)
#'
#' @description
#'
#' A graph with a horizontal grid is plotted. This is useful for plotting a histogram.
#' For more information, please visit \url{https://www.ses21.com}.
#' This function is slightly different from ses_bar().
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.

ses_hist_theme <- function(borders = TRUE) {

  if (borders == FALSE) {

  ggplot2::theme_bw(base_size = 10, base_family = '') +
    cowplot::theme_minimal_hgrid() +
    ggplot2::theme(

      axis.text = ggplot2::element_text(size = rel(0.85), color = "black"),
      axis.title.y =  ggplot2::element_text(size = rel(0.85), color = "black"),
      axis.title.x =  ggplot2::element_text(size = rel(0.85), color = "black", vjust = -1),
      axis.ticks =  ggplot2::element_blank(),
      axis.text.x=  ggplot2::element_text(vjust= 0),
      panel.grid.major =  ggplot2::element_line(size = 0.4),
    )

  } else if (borders == TRUE) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      ggplot2::theme(

        axis.text = ggplot2::element_text(size = rel(1.2), color = "black"),
        axis.title.y =  ggplot2::element_text(size = rel(1.2), color = "black"),
        axis.title.x =  ggplot2::element_text(size = rel(1.2), color = "black", vjust = -1),
        axis.ticks =  ggplot2::element_blank(),
        axis.text.x=  ggplot2::element_text(vjust= 0),
        panel.grid.major =  ggplot2::element_line(size = 0.4),
      )
  }
}


