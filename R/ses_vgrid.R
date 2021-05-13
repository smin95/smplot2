#' SES theme with vertical grid
#'
#' @description
#'
#' This theme has major vertical grids.
#' For more information, please visit \url{https://www.ses21.com}.
#'
#'
#' @param legends
#' If the legend needs to be displayed, the input should be `TRUE`.
#' If the legend is not needed, the input should be `FALSE`.
#'
#' @param borders
#' If the border needs to be displayed, the input should be `TRUE`.
#' If the border is not needed, the input should be `FALSE`.
#'
#' @examples
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#' geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) +
#' ses_vgrid()

ses_vgrid <- function(legends = TRUE, borders = TRUE) {
  if (legends == TRUE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_grid() +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = rel(.85), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0, size = rel(1.)),
          panel.grid.major =  ggplot2::element_line(size = 0.4),
          plot.title =  ggplot2::element_text(hjust = 0.5),
          panel.grid.minor.x =  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text.x = element_text(colour = "black", size = rel(.85)),
          strip.text.y = element_text(colour = "black", size = rel(.85)),
          legend.text=element_text(size= rel(.78)),
          legend.title=element_text(size=rel(.78))
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = rel(1.2), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.4),
          plot.title =  ggplot2::element_text(hjust = 0.5),
          panel.grid.minor.x =  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text.x = element_text(colour = "black", size = rel(1.2)),
          strip.text.y = element_text(colour = "black", size = rel(1.2)),
          legend.text=element_text(size= rel(1.1)),
          legend.title=element_text(size=rel(1.1))
        )
    }
  } else if (legends == FALSE) {
    if (borders == FALSE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        cowplot::theme_minimal_grid() +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = rel(.85), color = "black"),
          axis.title.y =  ggplot2::element_text(size = rel(.85), color = "black"),
          axis.title.x =  ggplot2::element_text(size = rel(.85), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.4),
          plot.title =  ggplot2::element_text(hjust = 0.5),
          panel.grid.minor.x =  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text.x = element_text(colour = "black", size = rel(.85)),
          strip.text.y = element_text(colour = "black", size = rel(.85)),
          legend.position = 'none'
        )
    } else if (borders == TRUE) {
      ggplot2::theme_bw(base_size = 10, base_family = '') +
        ggplot2::theme(
          axis.text =  ggplot2::element_text(size = rel(1.2), color = "black"),
          axis.title.y =  ggplot2::element_text(size = rel(1.2), color = "black"),
          axis.title.x =  ggplot2::element_text(size = rel(1.2), color = "black", vjust = -1),
          axis.text.x=  ggplot2::element_text(vjust= 0),
          panel.grid.major =  ggplot2::element_line(size = 0.4),
          plot.title =  ggplot2::element_text(hjust = 0.5),
          panel.grid.minor.x =  ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text.x = element_text(colour = "black", size = rel(1.2)),
          strip.text.y = element_text(colour = "black", size = rel(1.2)),
          legend.position = 'none'
        )
    }
  }
}
