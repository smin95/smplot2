sm_corr_avgErr <- function(data, x, y,
                           point.params = list(size=2.5),
                           errh.params = list(height = 0.1),
                           errv.params = list(width = 0.1),
                           error_type = 'se',
                           ...) {

  params <- list(...)
  point.params <- modifyList(params, point.params)
  errh.params <- modifyList(params, errh.params)
  errv.params <- modifyList(params, errv.params)


  if (error_type == 'se') {
    data <- data %>%
      dplyr::summarise(x_err = sm_stdErr({{x}}),
                       y_err = sm_stdErr({{y}}),
                       x_avg = mean({{x}}),
                       y_avg = mean({{y}})
    )
  } else if (error_type == 'sd') {

    data <- data %>%
      dplyr::summarise(x_err = sd({{x}}),
                     y_err = sd({{y}}),
                     x_avg = mean({{x}}),
                     y_avg = mean({{y}}))

  } else if (error_type == 'ci') {
    data <- data %>%
      dplyr::summarise(x_err = qt(p=0.05/2, df=length({{x}})-1, lower.tail=F) *
                         sm_stdErr({{x}}),
                       y_err = qt(p=0.05/2, df=length({{x}})-1, lower.tail=F) *
                         sm_stdErr({{y}}),
                       x_avg = mean({{x}}),
                       y_avg = mean({{y}}))
  }


  pointPlot <- do.call('geom_point',
                     modifyList(list(data = data,
                                     aes(y = y_avg, x = x_avg)), point.params))

  errhPlot <- do.call('geom_errorbarh',
                     modifyList(list(data = data,
                                     aes(y = y_avg,
                                         xmin = x_avg - x_err,
                                         xmax = x_avg + x_err), inherit.aes = F),
                                errh.params))

  errvPlot <- do.call('geom_errorbar',
                      modifyList(list(data = data,
                                      aes(x = x_avg,
                                          ymin = y_avg - y_err,
                                          ymax = y_avg + y_err), inherit.aes = F),
                                 errv.params))

  list(pointPlot,errhPlot,errvPlot)
}
