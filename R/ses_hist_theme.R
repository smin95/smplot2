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
      plot.title = element_text(hjust = 0.5)
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
        plot.title = element_text(hjust = 0.5)
      )
  }
}


