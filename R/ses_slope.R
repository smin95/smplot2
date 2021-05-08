#' SES plot with a theme appropriate for the slope chart
#' @description
#' In this plot, all aspects except for the left-handed spine are missing.
#' This format is appropriate for the slope chart. For more information,
#' please visit \url{https://www.ses21.com}.
#'
#' @param legends
#' #' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.

#' @export
#'
#' @examples
ses_slope <- function(legends = TRUE) {
  if (legends == T) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      ggplot2::theme(
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid",
                                   colour = "black"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(vjust = -1, size = 12),
        panel.background = element_blank(),
        axis.title.x = element_blank()
      )

  } else if (legends == F) {
    ggplot2::theme_bw(base_size = 10, base_family = '') +
      ggplot2::theme(
         panel.grid.minor.x=element_blank(),
         panel.grid.major.x=element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.y = element_blank(),
         panel.border = element_blank(),
         axis.line.y = element_line(size = 0.5, linetype = "solid",
                                    colour = "black"),
         axis.ticks.x = element_blank(),
         axis.text.y = element_text(color = "black"),
         axis.text.x = element_text(vjust = -1),
         panel.background = element_blank(),
         axis.title.x = element_blank(),
         axis.text = element_text(color = "black", size = 12),
         legend.position = 'none'
      )
  }
}
