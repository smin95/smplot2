#' A bar plot with individual points (jittered)
#'
#' @description
#' A bar plot superimposed by jittered individual points
#' and the error bar for standard error or standard deviation.
#'
#' @param bar_fill_color
#' Color of the all bars in the plot. It only accepts one color.
#' This argument is OPTIONAL.
#' If this argument is included, then all bars will have the same color.
#'
#' @param bar_width
#' Width of the bar.
#'
#' @param bar_border_color
#' Color of the bar borders. It only accepts one color.
#' @param bar_alpha
#' Transparency of the bars (0 to 1).
#' @param errColor
#' The color of the error bar
#' @param errSize
#' The line width of the error bar.
#' @param point_size
#' Size of the individual jittered points.
#' @param point_alpha
#' Transparency of the jittered points.
#'
#' @param se
#' When the error bar should represent standard error,
#' it should be set to TRUE. If the error bar should
#' represent standard deviation, it should be set to FALSE.
#'
#' @param point_jitter_width
#' Width of the jitter
#'
#' @param ...
#' Other parameters, such as 'color', 'shape', 'fill',
#' to specify the properties of the points.
#' For more information, type ?geom_point
#'
#' @import ggplot2 cowplot
#'
#' @importFrom stats sd
#' @export
#'
sm_bar <- function(bar_fill_color,
                    bar_width = 0.7,
                    bar_border_color = 'transparent',
                    bar_alpha = 1,
                    errSize = 1,
                    errColor = 'black',
                    point_size = 2.5,
                    point_alpha = 0.65,
                    se = T,
                    point_jitter_width = 0.12, ...) {

  if (missing(bar_fill_color)) {
    if (se == T) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_se, geom = "linerange",
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    } else {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun = mean, geom = "linerange",
                        fun.min = function(x) mean(x) - sd(x),
                        fun.max = function(x) mean(x) + sd(x),
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    }
  } else {
    if (se == T) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha,
                        fill = bar_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_se, geom = "linerange",
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    } else {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha,
                        fill = bar_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun = mean, geom = "linerange",
                        fun.min = function(x) mean(x) - sd(x),
                        fun.max = function(x) mean(x) + sd(x),
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    }
  }
}
