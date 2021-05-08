#' SES plot with a horizontal grid and without a border (useful for bar graphs)
#'
#' @description
#'
#' A graph with a horizontal grid and no border is plotted. This is useful for plotting a bar graph
#' For more information, please visit \url{https://www.ses21.com}.
#'
#' This is slightly different from ses_hist().
#'
#' @param ...
#' No input is required.
#'

ses_bar <- function() {

  ggplot2::theme_bw(base_size = 10, base_family = '') +
    cowplot::theme_minimal_hgrid() +
    ggplot2::theme(
      panel.grid.minor.x=  ggplot2::element_blank(),
      panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = element_blank(),
      panel.grid.major.y =  ggplot2::element_line(size = 0.5),
      axis.text =  ggplot2::element_text(size = rel(1), color = "black"),
      axis.title.y =  ggplot2::element_text(size = rel(1), color = "black"),
      axis.title.x =  ggplot2::element_text(size = rel(1), color = "black"),
      axis.text.x=element_text(vjust= 0)
    )
}


