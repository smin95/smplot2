#' A SES bar plot with individual points (jittered)
#'
#' @description
#' A bar plot superimposed by jittered individual points
#' and the error bar for standard error or standard deviation.
#'
#' @param bar_fill_color
#' Color of the bar.
#' @param width
#' Width of the bar.
#' @param errSize
#' The line width of the error bar.
#' @param point_size
#' Size of the individual jittered points.
#' @param point_alpha
#' Transparency of the jittered points.
#'
#' @param point_border_color
#' Color of the points' border.
#' @param point_shape
#' Shape of the jittered points.
#' Only shapes (21-25) with borders are allowed.
#' @param se
#' When the error bar should represent standard error,
#' it should be set to TRUE. If the error bar should
#' represent standard deviation, it should be set to FALSE.
#' @param ...
#' Other parameters  to specify the properties of the points.
#' For more information, type ?geom_point
#'
#'
#' @import ggplot2 cowplot
#'
#' @importFrom stats sd
#' @export
#'
ses_bar <- function(bar_fill_color = 'gray85',
                    width = 0.4,
                    errSize = 1,
                    point_size = 2.5,
                    point_alpha = 0.65,
                    point_border_color = 'white',
                    point_shape = 21, se = T, ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (se == T) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_point(position = ggplot2::position_jitter(width = .12,
                                                                 height = 0,
                                                                 seed = 10),
                             shape = point_shape,
                             color = point_border_color,
                             size = point_size,
                             alpha = point_alpha, ...),
         stat_summary(fun = mean, geom = "bar", width = width,
                      fill = bar_fill_color, color = 'transparent'),
         stat_summary(fun.data = mean_se, geom = "linerange",
                      size = errSize),
         sesplot::ses_hgrid())
  } else {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_point(position = ggplot2::position_jitter(width = .12,
                                                                 height = 0,
                                                                 seed = 10),
                             shape = point_shape,
                             color = point_border_color,
                             size = point_size,
                             alpha = point_alpha, ...),
         stat_summary(fun = mean, geom = "bar", width = width,
                      fill = bar_fill_color, color = 'transparent'),
         stat_summary(fun = mean, geom = "linerange",
                      fun.min = function(x) mean(x) - sd(x),
                      fun.max = function(x) mean(x) + sd(x),
                      size = errSize),
         sesplot::ses_hgrid())
  }

}

