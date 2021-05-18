#' SES theme with a horizontal grid (a theme for bar graphs).
#'
#' @description
#'
#' A graph with a horizontal grid is plotted. Border can be added or removed.
#' This is useful for plotting a bar graph.
#' For more information, please visit \url{https://www.ses21.com}.
#'
#' ses_bar_theme() is exactly the same as ses_hgrid().
#' @import ggplot2 cowplot
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#' @examples
#' df <- data.frame(group=c("One", "Two", "Three"),
#' score=c(5.2, 9.3, 12))
#' p1 <- ggplot(data=df, aes(x=group, y=score, fill = group)) +
#' geom_bar(stat="identity")
#' p1 + ses_bar_theme()
#' @export

ses_bar_theme <- function(legends = FALSE, borders = TRUE) {

  if (legends == TRUE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_hgrid() +
        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y =  ggplot2::element_line(size = 0.5),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black",
                                                vjust=-1),
          axis.text.x=ggplot2::element_text(vjust= 0),
          legend.text=ggplot2::element_text(size = ggplot2::rel(.78)),
          legend.title=ggplot2::element_text(size = ggplot2::rel(.78)),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +

        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y =  ggplot2::element_line(size = 0.5),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black",
                                                vjust=-1),
          axis.text.x=ggplot2::element_text(vjust= 0),
          legend.text=ggplot2::element_text(size = ggplot2::rel(1.1)),
          legend.title=ggplot2::element_text(size = ggplot2::rel(1.1)),
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    }
  } else if (legends == FALSE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_hgrid() +
        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.y =  ggplot2::element_line(size = 0.5),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black",
                                                vjust=-1),
          axis.text.x=ggplot2::element_text(vjust= 0),
          legend.position = 'none',
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +

        ggplot2::theme(
          panel.grid.minor.x=  ggplot2::element_blank(),
          panel.grid.major.x=  ggplot2::element_blank(), panel.grid.minor.y = element_blank(),
          panel.grid.major.y =  ggplot2::element_line(size = 0.5),
          axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black",
                                                vjust=-1),
          axis.text.x=ggplot2::element_text(vjust= 0),
          legend.position = 'none',
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    }
  }
}

