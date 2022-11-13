#' A slope chart (updated in smplot2)
#'
#' @description
#' This function generates a slope chart.
#' This is very useful for comparing the effect between two time points.
#'
#' For this function to work properly,
#' ggplot()'s mapping has to be quite specific.
#' 1. Each observation has to be grouped.
#'
#' With smplot2, average plot can be displayed along with error bars, both of which
#' are optional.
#'
#' @param ...
#' List of parameters for individual points and lines across different elements
#' (except for except for xTick.params), such as color, alpha, fill etc.

#' @param labels
#' Labels for the ticks of the x-axis. This is a required argument.
#' It has to be a single vector containing either one
#' or multiple elements. ex: c('Day 1', 'Day 2')
#'
#' @param group
#' Name of the variable by which the individual data should be grouped
#'
#' @param line.params
#' List of parameters for the individual lines, such as color, alpha etc
#'
#' @param point.params
#' List of parameters for the individual points, such as color, alpha, fill etc
#'
#' @param avgLine.params
#' List of parameters for the average line, such as color, alpha etc
#'
#' @param avgPoint.params
#' List of parameters for the average point, such as color, alpha, fill etc
#'
#' @param err.params
#' List of parameters for the error bar from the average plot, such as color, alpha etc
#'
#' @param xTick.params
#' List of parameters for the x tick from the average plot, such as color, alpha etc
#'
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param errorBar
#' If the error bar needs to be displayed, the input should be TRUE.
#' If the error bar is not needed, the input should be FALSE.
#'
#' @param show_mean
#‘ If the average plot needs to be displayed, the input should be TRUE.
#' If the average plot is not needed, the input should be FALSE.
#'
#' @param legends
#’ If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#'
#' @return
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1) # generate random data
#' day1 = rnorm(16,2,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' df %>% ggplot(aes(x = Day, y = Value, fill = Day)) +
#'  sm_slope(labels = c('Day 1', 'Day 2'), group = Subject) +
#'  scale_fill_manual(values=  sm_color('blue','orange'))

#' ggplot(data = df, aes(x = Day, y = Value, fill = Day)) +
#'  sm_slope(labels = c('Day 1','Day 2'),group = Subject,
#'           point.params = list(alpha = 0.3, size = 2.5, color = 'white',
#'                               shape = 21, fill = sm_color('skyblue')),
#'           line.params = list(color = sm_color('skyblue'),
#'                              alpha = 0.3),
#'           avgPoint.params = list(color='transparent', shape = 21,
#'                                  size = 4, fill = sm_color('blue')),
#'           avgLine.params = list(color = sm_color('blue'), size = 1),
#'           show_mean = TRUE)
#’
#‘ }
sm_slope <- function(...,
                     labels,
                     group,
                     line.params = list(color = 'gray53',
                                        size = 0.4,
                                        alpha = 0.4),
                     point.params = list(size = 2.5, shape = 21,
                                         color = 'white'),
                     avgLine.params = list(size = line.params$size*2),
                     avgPoint.params = list(size = point.params$size*1.4),
                     err.params = list(),
                     xTick.params = list(position = 'top',
                                         expand = c(0.17,.1),
                                         drop=FALSE),
                     errorbar_type = 'sd',
                     errorBar = FALSE,
                     show_mean = FALSE,
                     legends = FALSE) {

  if (missing(group)) {
    stop('group (of the shadow) must be specified because each observation has to be paired.')
  }

  if (missing(labels)) {
    labels <- rev(letters)
  }

  params <- list(...)
  line.params <- modifyList(params, line.params)
  point.params <- modifyList(params, point.params)
  avgLine.params <- modifyList(params, avgLine.params)
  avgPoint.params <- modifyList(params, avgPoint.params)
  err.params <- modifyList(params, err.params)
  #xTick.params <- modifyList(params, xTick.params)

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


  if (errorBar == FALSE) {
    errPlot <- NULL
  }

  if (show_mean == FALSE) {
    avgLinePlot <- NULL
    avgPointPlot <- NULL
    errPlot <- NULL
  }

  list(linePlot,pointPlot,avgLinePlot,
       avgPointPlot,errPlot, scaleX,
       sm_slope_theme(legends=legends))


}
