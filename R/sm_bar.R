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
#'
#' @param errSize
#' The line width of the error bar.
#' @param errColor
#' The color of the error bar

#' @param point_size
#' Size of the individual jittered points.
#' @param point_alpha
#' Transparency of the jittered points.
#'
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se' (default), standard error bar will be shown.
#' If it is set to 'sd', the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param point_jitter_width
#' Width of the jitter. The default value is 0.12.
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
#' ggplot(data = df, mapping = aes(x = Day, y = Value, fill = Day)) +
#' sm_bar(shape = 21, color = 'white', bar_fill_color = 'gray80') +
#' scale_fill_manual(values = sm_color('blue','orange'))
#' }
#'
sm_bar <- function(bar_fill_color,
                   bar_width = 0.7,
                   bar_border_color = 'transparent',
                   bar_alpha = 1,
                   errSize = 1,
                   errColor = 'black',
                   point_size = 2.5,
                   point_alpha = 0.65,
                   errorbar_type = 'se',
                   point_jitter_width = 0.12, ...) {

  suppressWarnings() # suppress warnings

  if (missing(bar_fill_color)) {
    if (errorbar_type == 'se') {
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
    } else if (errorbar_type == 'sd') {
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
    } else if (errorbar_type == 'ci') {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_cl_boot, geom = "linerange",
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    } else {
      stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
    }
  } else {
    if (errorbar_type == 'se') {
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
    } else if (errorbar_type == 'sd') {
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
    } else if (errorbar_type == 'ci') {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           stat_summary(fun = mean, geom = "bar", width = bar_width,
                        color = bar_border_color, alpha = bar_alpha,
                        fill = bar_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = point_jitter_width,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_cl_boot, geom = "linerange",
                        size = errSize,
                        color = errColor),
           sm_hgrid())
    } else {
      stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
    }
  }
}
