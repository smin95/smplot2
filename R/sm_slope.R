#' A slope chart
#'
#' @description
#' This function generates a slope chart.
#' This is very useful for comparing the effect between two time points.
#'
#' ggplot()'s mapping has to be quite specific: each observation has to be grouped.
#'
#' Error bar types can be specified (ci, sd, and se).
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
#' @param many_groups
#' This argument determines whether the average line can be plotted for each group when
#' multiple groups are plotted at once.
#' If the average line needs to be plotted across all data presented, set this as FALSE.
#' If there are many groups that are presented and that each average line has to be plotted,
#' then set this as TRUE.
#'
#' @param show_err
#' If the error bar needs to be displayed, the input should be TRUE.
#' If the error bar is not needed, the input should be FALSE.
#'
#' @param show_mean
#' If the average plot needs to be displayed, the input should be TRUE.
#' If the average plot is not needed, the input should be FALSE.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param forget
#' Forget the defaults when list() is called for a specific parameter (ex. point.params).
#' Set to TRUE when when users want to map aesthetics to different groups more flexibly..
#' Set to FALSE by default.
#'
#' @return Returns a slope chart which is a ggplot2 object.
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#'
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
#' ggplot(data=df, aes(x = Day, y = Value, fill = Day)) +
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
#'           avgLine.params = list(color = sm_color('blue'), linewidth = 1),
#'           show_mean = TRUE)
#'
#'
sm_slope <- function(...,
                     labels,
                     group,
                     line.params = list(color = 'gray53',
                                        linewidth = 0.4,
                                        alpha = 0.4),
                     point.params = list(size = 2.5, shape = 21,
                                         color = 'white'),
                     avgLine.params = list(linewidth = 1),
                     avgPoint.params = list(size = 4),
                     err.params = list(linewidth = 1),
                     xTick.params = list(position = 'top',
                                         expand = c(0.17,.1),
                                         drop=FALSE),
                     errorbar_type = 'sd',
                     many_groups = FALSE,
                     show_err = FALSE,
                     show_mean = FALSE,
                     legends = FALSE,
                     forget = FALSE) {

  if (missing(group)) {
    stop('group (of the shadow) must be specified because each observation has to be paired.')
  }

  if (missing(labels)) {
    labels <- rev(letters)
  }

  params <- list(...)

  if (forget == FALSE) {
    line.params0 = list(color = 'gray53', linewidth = 0.4,
                        alpha = 0.4)
    line.params0 <- modifyList(line.params0, params)

    point.params0 = list(size = 2.5, shape = 21,
                         color = 'white')
    point.params0 <- modifyList(point.params0, params)

    avgLine.params0 = list(linewidth = 1)
    avgLine.params0 <- modifyList(avgLine.params0, params)

    avgPoint.params0 = list(size = 4)
    avgPoint.params0 <- modifyList(avgPoint.params0, params)

    err.params0 = list(linewidth = 1)
    err.params0 <- modifyList(err.params0, params)

    xTick.params0 = list(position = 'top', expand = c(0.17,.1), drop=FALSE)


    line.params <- modifyList(line.params0, line.params)
    point.params <- modifyList(point.params0, point.params)
    avgLine.params <- modifyList(avgLine.params0, avgLine.params)
    avgPoint.params <- modifyList(avgPoint.params0, avgPoint.params)
    err.params <- modifyList(err.params0, err.params)
    xTick.params <- modifyList(xTick.params0, xTick.params)
  } else if (forget == TRUE) {
    line.params <- modifyList(params, line.params)
    point.params <- modifyList(params, point.params)
    avgLine.params <- modifyList(params, avgLine.params)
    avgPoint.params <- modifyList(params, avgPoint.params)
    err.params <- modifyList(params, err.params)
    #xTick.params <- modifyList(params, xTick.params)
  }


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

  if (many_groups == FALSE) {
    avgLinePlot <- do.call('stat_summary',
                           modifyList(list(aes(group=1), fun = mean,
                                           geom = 'line'), avgLine.params))
  } else if (many_groups == TRUE) {
    avgLinePlot <- do.call('stat_summary',
                           modifyList(list(fun = mean,
                                           geom = 'line'), avgLine.params))
  } else {
    stop('many_groups has to be either TRUE or FALSE.')
  }

  avgPointPlot <- do.call('stat_summary',
                          modifyList(list(fun = mean,
                                          geom = 'point'), avgPoint.params))

  scaleX <- do.call('scale_x_discrete',
                    modifyList(list(labels = labels), xTick.params))


  if (show_err == FALSE) {
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
