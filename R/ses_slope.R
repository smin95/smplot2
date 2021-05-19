#' A SES slope chart
#'
#' @description
#' This function generates a slope chart.
#' This is very useful for comparing the effect between two time points.
#'
#' For this function to work properly,
#' ggplot()'s mapping has to be quite specific.
#' 1. Each observation has to be grouped.
#' 2. In aes(), "fill" argument has to be filled in order to generate any filled color.
#'
#'
#' @param line_color
#' Color of the lines
#' @param line_size
#' Size of the lines
#' @param point_border_color
#' Color of the point's border
#' @param point_size
#' Size of the points
#' @param point_shape
#' Shape of the points (21-25)
#' @param point_stroke
#' Border width of the points
#' @param ...
#' Other parameters of scale_x_discrete. For more information,
#' please type ?scale_x_discrete.
#' @import ggplot2 cowplot
#'
#'
#' @export
ses_slope <- function(line_color = 'gray53',
                      line_size = 0.4, point_border_color = 'white',
                      point_size = 2.5, point_shape = 21, point_stroke = 0.5,
                      ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  list(ggplot2::geom_line(color = line_color, size = line_size),
       ggplot2::geom_point(size = point_size*1.8, fill = "white", color = 'white'),
       ggplot2::geom_point(size = point_size, shape = point_shape,
                  color = point_border_color, stroke = point_stroke),
       ggplot2::scale_x_discrete(position = 'top', expand = c(0.15, .1),
                        drop = FALSE,  ...),
       sesplot::ses_slope_theme(legends = F))
}
