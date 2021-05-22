#' A SES slope chart
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
#' @param ...
#' Other parameters of geom_point(), such as 'shape', fill', and 'color.
#' For more information, please type ?geom_point.
#' @import ggplot2 cowplot
#'
#'
#' @export
ses_slope <- function(labels,
                      line_color = 'gray53',
                      line_size = 0.4,
                      point_size = 2.5,
                      ...) {

  list(ggplot2::geom_line(color = line_color, size = line_size),
       ggplot2::geom_point(size = point_size*1.8, fill = "white", color = 'white'),
       ggplot2::geom_point(size = point_size, ...),
       ggplot2::scale_x_discrete(position = 'top', expand = c(0.17, .1),
                        drop = FALSE, labels = labels),
       sesplot::ses_slope_theme(legends = F))
}
