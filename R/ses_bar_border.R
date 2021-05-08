#' SES plot with a border (useful for bar plots)
#'
#' @description
#' A graph with a border is plotted. It is aesthetically appropriate for bar graphs.
#'
#' For more information, please visit \url{https://www.ses21.com}.
#'
#' @param ...
#' No input is required.
#' @export
#'
#
#'
ses_bar_border <- function() {

  ggplot2::theme_bw(base_size = 10, base_family = '') +

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
