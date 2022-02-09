#' A slope chart
#'
#' @description
#' This function generates a slope chart.
#' This is very useful for comparing the effect between two time points.
#'
#' For this function to work properly,
#' ggplot()'s mapping has to be quite specific.
#' 1. Each observation has to be grouped.
#' @param labels
#' Labels for the ticks of the x-axis. This is a required argument.
#' It has to be a single vector containing either one
#' or multiple elements. ex: c('Day 1', 'Day 2')
#' @param line_color
#' Color of the lines
#' @param line_size
#' Size of the lines
#' @param point_size
#' Size of the points
#' @param line_type
#' Type of the line. Default is 'solid'; other options include:
#' 'twodash', 'longdash', 'dotted', 'dotdash', 'dashed', and
#' 'blank'.
#' @param line_alpha
#' Transparency of the lines (0 to 1).
#' @param xtick_high
#' If it is set to TRUE, the ticks of the x-axis will be above. The argument
#' for 'labels' is required.
#' If it is set to FALSE, they will be at the bottom. Also the argument
#' for 'labels' will be ignored in this case. To change the labels, you
#' will need to add a geom object with scale_x_discrete(labels = c('A','B')).
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param ...
#' Other parameters of geom_point(), such as 'shape', fill', and 'color.
#' For more information, please type ?geom_point.
#' @import ggplot2 cowplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day) # final dataframe
#'
#' #use the dataframe to generate a bar plot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, group = Subject)) +
#' sm_slope(labels = c('Day 1', 'Day 2'))
#' }
sm_slope <- function(labels,
                      line_color = 'gray53',
                      line_size = 0.4,
                      point_size = 2.5,
                      line_type = 'solid',
                      line_alpha = 1,
                      xtick_high = TRUE,
                      legends = FALSE,
                      ...) {

  if (xtick_high == TRUE) {
    list(ggplot2::geom_line(color = line_color, size = line_size, linetype = line_type,
                            alpha = line_alpha),
         ggplot2::geom_point(size = point_size*1.8, fill = "white", color = 'white'),
         ggplot2::geom_point(size = point_size, ...),
         ggplot2::scale_x_discrete(position = 'top', expand = c(0.17, .1),
                                   drop = FALSE, labels = labels),
         sm_slope_theme(legends = legends))
  } else if (xtick_high == FALSE)
    list(ggplot2::geom_line(color = line_color, size = line_size, linetype = line_type,
                            alpha = line_alpha),
         ggplot2::geom_point(size = point_size*1.8, fill = "white", color = 'white'),
         ggplot2::geom_point(size = point_size, ...),
         sm_slope_theme(legends = legends))

}
