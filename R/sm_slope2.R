sm_slope <- function(...,
                     labels,
                     line.params = list(color = 'gray53',
                                        size = 0.4,
                                        alpha = 0.4),
                     point.params = list(size = 2.5),
                     avgLine.params = list(size = line.params$size*2),
                     avgPoint.params = list(size = point.params$size*1.4),
                     err.params = list(),
                     xTick.params = list(position = 'top',
                                         expand = c(0.17,.1),
                                         drop=FALSE),
                     errorbar_type = 'sd',
                     errorBar = TRUE,
                     avgShow = FALSE,
                     legends = FALSE) {

  params <- list(...)
  line.params <- modifyList(params, line.params)
  point.params <- modifyList(params, point.params)
  avgLine.params <- modifyList(params, avgLine.params)
  avgPoint.params <- modifyList(params, avgPoint.params)
  err.params <- modifyList(params, err.params)
  xTick.params <- modifyList(params, xTick.params)

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
                      modifyList(list(aes(group = Subject)), line.params))

  pointPlot <- do.call('geom_point',
                       modifyList(list(), point.params))

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

  if (avgShow == FALSE) {
    avgLinePlot <- NULL
    avgPointPlot <- NULL
  }

  list(linePlot,pointPlot,avgLinePlot,
       avgPointPlot,errPlot, scaleX,
       sm_slope_theme(legends=legends))


}
