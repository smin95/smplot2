sm_forest <- function(...,
                      point.params = list(size=  2.5, alpha = 0.3),
                      avgPoint.params = list(size = point.params$size * 2.2,
                                             shape = 18),
                      err.params = list(color = 'black'),
                      ref.params = list(size = 0.4, color = 'gray80',
                                        linetype='dashed'),
                      xintercept = 0,
                      sep_level = 2,
                      point_jitter_width = 0,
                      errorbar_type = 'ci',
                      points = TRUE,
                      refLine = TRUE,
                      borders = TRUE,
                      legends = FALSE
) {

  if (point_jitter_width == 0) {
    point_jitter_width <- 1e-10
  }

  params <- list(...)
  point.params <- modifyList(params, point.params)
  avgPoint.params <- modifyList(params, avgPoint.params)
  err.params <- modifyList(params, err.params)
  ref.params <- modifyList(params, ref.params)

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


  position_nudge_vector <- c(-sep_level/10,0)

  refLinePlot <- do.call('geom_vline',
                         modifyList(list(xintercept=xintercept), ref.params))


  pointPlot <- do.call('geom_point',
                       modifyList(list(position = position_jitternudge(jitter.width=point_jitter_width,
                                                                       jitter.height=point_jitter_width,
                                                                       seed=10,
                                                                       nudge.y = 0,
                                                                       nudge.x = position_nudge_vector[1])),
                                  point.params))

  avgPointPlot <- do.call('stat_summary',
                          modifyList(list(fun = mean,
                                          geom = 'point',
                                          position = position_nudge(y = position_nudge_vector[2])), avgPoint.params))



  if (points == FALSE) {
    pointPlot <- NULL
  }

  if (refLine == FALSE) {
    refLinePlot <- NULL
  }

  list(refLinePlot,pointPlot,avgPointPlot,errPlot,
       sm_hgrid(borders=borders,legends=legends))

}
