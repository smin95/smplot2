#' Slope Chart with Mean for a Single Group
#'
#' @description
#' Generates a slope chart with mean for a single group. This is useful for comparing
#' the effect between two time points. The function includes options for shadow lines
#' and points, mean lines, points, and error bars, all of which can be customized.
#'
#' Note: This functionality can also be reproduced using `sm_slope()` with appropriate
#' customization. In `ggplot()`, the mapping requires grouping each observation to
#' correctly pair points.
#'
#' @param ...
#' Additional aesthetic parameters applied across points, lines, and error bars. Optional.
#'
#' @param labels
#' A vector specifying the labels for the x-axis ticks. This is a required argument.
#' For example: \code{c('Day 1', 'Day 2')}.
#'
#' @param group
#' The name of the variable used to group individual data points. This is a required argument.
#'
#' @param main_color
#' The main color of the slope chart, shared across:
#' \itemize{
#'   \item Points
#'   \item Lines
#'   \item Error bars
#' }
#' Default: \code{sm_color('blue')}.
#'
#' @param main_shape
#' The shape of the points in the slope chart:
#' \itemize{
#'   \item Shapes above \code{20} use \code{color = 'white'} and a fill color matching \code{main_color}.
#' }
#' Default: \code{21}.
#'
#' @param back_alpha
#' Transparency (alpha) for the shadow lines and points:
#' \itemize{
#'   \item Lines: controlled by \code{back_alpha}.
#'   \item Points: controlled by \code{back_alpha * 0.65}.
#' }
#' Default: \code{0.25}.
#'
#' @param line_width
#' Line width for the shadow lines connecting points. Default: \code{0.25}.
#'
#' @param avgline_width
#' Line width for the average line. Default: \code{1}.
#'
#' @param point_size
#' Size of the points in the shadow. Default: \code{2.5}.
#'
#' @param avgpoint_size
#' Size of the points representing the mean of the data. Default: \code{4}.
#'
#' @param err_width
#' Line width for the error bars. Default: \code{1}.
#'
#' @param xTick.params
#' A list of parameters for customizing the x-axis ticks. Options include:
#' \itemize{
#'   \item \code{position}: Location of the ticks (default: \code{'top'}).
#'   \item \code{expand}: Space around the ticks (default: \code{c(0.17, 0.1)}).
#'   \item \code{drop}: Whether to drop unused factor levels (default: \code{FALSE}).
#' }
#'
#' @param errorbar_type
#' A string specifying the type of error bars to display:
#' \itemize{
#'   \item \code{'se'}: Standard error.
#'   \item \code{'sd'}: Standard deviation (default).
#'   \item \code{'ci'}: 95% confidence interval.
#' }
#'
#' @param show_err
#' Logical. Determines whether to display error bars:
#' \itemize{
#'   \item \code{TRUE}: Display error bars.
#'   \item \code{FALSE}: Hide error bars (default).
#' }
#'
#' @param legends
#' Logical. Determines whether to display legends:
#' \itemize{
#'   \item \code{TRUE}: Display legends.
#'   \item \code{FALSE}: Hide legends (default).
#' }
#'
#' @return
#' A slope chart with mean, represented as a ggplot2 object.
#'
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
