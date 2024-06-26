#' Superimposition of the average point with horizontal and vertical error bars
#' in the correlation plot
#'
#' @param data
#' Data frame variable that is used for plotting.
#' @param x
#' Column of the data frame that represents the x-axis.
#' @param y
#' Column of the data frame that represents the y-axis.
#' @param point.params
#' List of parameters for the mean point, such as color, alpha, fill etc
#'
#' @param errh.params
#' List of parameters for the horizontal error bar, such as color, alpha, fill etc
#'
#' @param errv.params
#' List of parameters for the vertical points, such as color, alpha, fill etc
#'
#' @param errorbar_type
#' This argument determines the error bar type.
#' If it is set to 'se' , standard error bar will be shown.
#' If it is set to 'sd', the error bar will display standard deviation.
#' If it is set to 'ci' (default), the error bar will display 95\% confidence interval.
#'
#' @param ...
#' A generic aesthetic parameter across points and error bars. This is optional.
#'
#'
#' @return A point with error bars representing the average will be returned.
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @importFrom dplyr summarise
#' @export
#'
#' @examples
#' library(smplot2)
#' library(ggplot2)
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#' geom_point(shape = 21, size = 3) +
#'  sm_corr_avgErr(mtcars, drat,mpg, errorbar_type = 'se',
#'                 color = sm_color('red'))

sm_corr_avgErr <- function(data, x, y,
                           point.params = list(size = 2.5),
                           errh.params = list(height = 0),
                           errv.params = list(width = 0),
                           errorbar_type = 'se',
                           ...) {

  params <- list(...)
  point.params <- modifyList(params, point.params)
  errh.params <- modifyList(params, errh.params)
  errv.params <- modifyList(params, errv.params)


  if (errorbar_type == 'se') {
    data <- dplyr::summarise(.data = data, x_err = sm_stdErr({{x}}),
                       y_err = sm_stdErr({{y}}),
                       x_avg = mean({{x}}),
                       y_avg = mean({{y}})
      )
  } else if (errorbar_type == 'sd') {

    data <- dplyr::summarise(.data = data, x_err = sd({{x}}),
                       y_err = sd({{y}}),
                       x_avg = mean({{x}}),
                       y_avg = mean({{y}}))

  } else if (errorbar_type == 'ci') {
    data <- dplyr::summarise(.data = data, x_err = qt(p=0.05/2, df=length({{x}})-1, lower.tail=F) *
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

  list(errhPlot,errvPlot, pointPlot)
}

globalVariables(c('x_err', 'y_err', 'x_avg','y_avg'))
