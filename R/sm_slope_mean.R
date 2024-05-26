#' A slope chart (with mean) of one group
#'
#' This function provides an easy way to plot slope chart with mean. This can
#' also be reproduced using sm_slope().
#'
#' This is very useful for comparing the effect between two time points of
#' one group.
#'
#' ggplot()'s mapping has to be quite specific: each observation has to be grouped.
#'
#' Error bar types can be specified (ci, sd, and se).
#'
#' @param ...
#' List of parameters for individual points and lines across different elements
#' (except for except for xTick.params), such as color, alpha, fill etc.
#'
#' @param labels
#' Labels for the ticks of the x-axis. This is a required argument.
#' It has to be a single vector containing either one
#' or multiple elements. ex: c('Day 1', 'Day 2')
#'
#' @param group
#' Name of the variable by which the individual data should be grouped
#'
#' @param main_color
#' Main color of the slope chart. Shared across points, lines and error bars.
#' @param main_shape
#' Main shape of the points in the slope chart.
#' @param back_alpha
#' Transparency of the shadow (individual lines and points) from the back.
#' @param line_width
#' Line width of the line that connects points in the back shadow
#' @param avgline_width
#' Average's linewidth of the line that connects points in the back shadow
#' @param point_size
#' Size of the points in the back from the shadow
#' @param avgpoint_size
#' Size of the points representing the mean of the data
#' @param err_width
#' Linewidth of the errorbars
#' @param xTick.params
#' List of parameters for the x tick from the average plot, such as color, alpha etc
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#' @param show_err
#' If the error bar needs to be displayed, the input should be TRUE.
#' If the error bar is not needed, the input should be FALSE.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @return Returns a slope chart which is a ggplot2 object.
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#' @examples
#' library(ggplot2)
#' library(smplot2)
#'
#' set.seed(1) # generate random data
#' day1 = rnorm(16,2,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' ggplot(data=df, aes(x = Day, y = Value)) +
#'  sm_slope_mean(labels = c('Day 1', 'Day 2'), group = Subject, back_alpha = .3,
#' main_color = sm_color('green'))
sm_slope_mean <- function(...,
                          labels,
                          group,
                          main_color=sm_color('blue'),
                          main_shape=21,
                          back_alpha = 0.25,
                          line_width = 0.25,
                          avgline_width = 1,
                          point_size = 2.5,
                          avgpoint_size = 4,
                          err_width = 1,
                          xTick.params = list(position = 'top',
                                              expand = c(0.17,.1),
                                              drop=FALSE),
                          errorbar_type = 'sd',
                          show_err = FALSE,
                          legends = FALSE) {

  if (missing(group)) stop('group (of the shadow) must be specified because each observation has to be paired.')
  if (missing(labels)) labels <- rev(letters)

  line.params = list(); point.params = list(); avgLine.params = list();
  avgPoint.params = list(); err.params = list()

  # color
  line.params$color <- main_color
  err.params$color <- main_color
  avgLine.params$color <- main_color
  if (main_shape > 20) {
    point.params$color <- 'white'
    avgPoint.params$color <- 'white'
    point.params$fill <- main_color
    avgPoint.params$fill <- main_color
  } else {
    point.params$color <- main_color
    avgPoint.params$color <- main_color
  }

  #shape
  point.params$shape <- main_shape; avgPoint.params$shape <- main_shape

  #point size
  point.params$size <- point_size; avgPoint.params$size <- avgpoint_size

  #alpha
  line.params$alpha <- back_alpha; point.params$alpha <- (back_alpha * 0.65)

  #linewidth
  line.params$linewidth <- line_width; avgLine.params$linewidth <- avgline_width
  err.params$linewidth <- err_width

  params <- list(...)
  line.params <- modifyList(params, line.params)
  point.params <- modifyList(params, point.params)
  avgLine.params <- modifyList(params, avgLine.params)
  avgPoint.params <- modifyList(params, avgPoint.params)
  err.params <- modifyList(params, err.params)

  if (errorbar_type == 'se') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun.data = mean_se,
                                       geom = 'linerange'), err.params))
  } else if (errorbar_type == 'sd') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun = mean,
                                       fun.min = function(x) mean(x) - sd(x),
                                       fun.max = function(x) mean(x) + sd(x),
                                       geom = 'linerange'),
                                  err.params))
  } else if (errorbar_type == 'ci') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun.data = mean_cl_boot,
                                       geom = 'linerange'), err.params))
  } else {
    stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
  }

  linePlot <- do.call('geom_line',
                      modifyList(list(aes(group = {{group}})), line.params))

  pointPlot <- do.call('geom_point',
                       modifyList(list(aes(group={{group}})), point.params))

  avgLinePlot <- do.call('stat_summary',
                         modifyList(list(aes(group=1), fun = mean,
                                         geom = 'line'), avgLine.params))
  avgPointPlot <- do.call('stat_summary',
                          modifyList(list(fun = mean,
                                          geom = 'point'), avgPoint.params))

  scaleX <- do.call('scale_x_discrete',
                    modifyList(list(labels = labels), xTick.params))

  if (show_err == FALSE) {
    errPlot <- NULL
  }

  list(linePlot,pointPlot,avgLinePlot,
       avgPointPlot,errPlot, scaleX,
       sm_slope_theme(legends=legends))
}
