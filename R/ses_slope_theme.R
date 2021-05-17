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
#' library(tidyverse)
#' library(sesplot)
#' # Generate random data first
#' set.seed(1)
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Time <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' DataFrame <- cbind(Subject, Data, Time)
#'
#' p5 <- ggplot(data = DataFrame,
#' aes(x = Time, y = Value,
#'    group = Subject,Fill = Time))  +
#'   geom_line(color = "gray53", size = .4) +
#'  geom_point(size = 3, shape= 21,
#'  fill = ses_palette(1, color = 'green'),
#'  color = 'white') +
#'   scale_x_discrete(position = 'top',
#'   expand = c(0.15, .1), drop=FALSE)
#'
#' p5 + ses_slope_theme()

ses_slope_theme <- function(legends = TRUE) {
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
        axis.title.y =  ggplot2::element_text(size = rel(1.2), color = "black"),
        axis.text.x = element_text(vjust = -1, size = 12),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)
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
         axis.title.y =  ggplot2::element_text(size = rel(1.2), color = "black"),
         axis.text.x = element_text(vjust = -1),
         panel.background = element_blank(),
         axis.title.x = element_blank(),
         axis.text = element_text(color = "black", size = 12),
         legend.position = 'none',
         plot.title = element_text(hjust = 0.5)
      )
  }
}
