#' A bar plot with jittered individual points
#'
#' @param ...
#' A generic aesthetic parameter across points and the boxplot. This is optional.
#'
#' @param bar.params
#' List of parameters for the bar graph, such as color, alpha, fill etc
#'
#' @param err.params
#' List of parameters for the error bar, such as color, size, alpha etc
#'
#' @param point.params
#' List of parameters for individual points, such as color, alpha, fill etc
#'
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param point_jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param seed
#' Random seed. Requires a number.
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
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @return A bar graph generated using ggplot2
#' @export
#'
#' @examples
#' library(smplot2)
#' library(ggplot2)
#' set.seed(1) # generate random data
#' day1 = rnorm(16,2,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' # with aesthetic defaults of smplot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_bar() +
#' scale_color_manual(values = sm_color('blue','orange'))
#'

sm_bar <- function(...,
                   bar.params = list(width = 0.7, alpha = 1, color = 'transparent',
                                     fill = 'gray80'),
                   err.params = list(linewidth = 1, color = 'black'),
                   point.params = list(size = 2.5, alpha = 0.65, shape = 16),
                   errorbar_type = 'se',
                   point_jitter_width = 0.12,
                   points = TRUE,
                   borders = TRUE,
                   legends = FALSE, seed = NULL, forget = FALSE) {


  if (length(seed)) set.seed(seed)
  params <- list(...)

  if (forget == FALSE) {
    bar.params0 <- list(width = 0.7, alpha = 1, color = 'transparent',
                        fill = 'gray80') # default for bar
    bar.params0 <- modifyList(bar.params0, params)

    err.params0 <- list(size = 1, color = 'black')
    err.params0 <- modifyList(err.params0, params)

    point.params0 <- list(size = 2.5, alpha = 0.65)
    point.params0 <- modifyList(point.params0, params)

    bar.params <- modifyList(bar.params0, bar.params)
    err.params <- modifyList(err.params0, err.params)
    point.params <- modifyList(point.params0, point.params)

  } else if (forget == TRUE){
    bar.params <- modifyList(params, bar.params)
    err.params <- modifyList(params, err.params)
    point.params <- modifyList(params, point.params)
  }


  barPlot <- do.call('stat_summary',
                     modifyList(list(fun = 'mean',
                                     geom = 'bar'), bar.params))

  pointPlot <- do.call('geom_point',
                       modifyList(list(position = position_jitter(height=0,
                                                                  width=point_jitter_width)), point.params))

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


  if (points == FALSE) {
    pointPlot <- NULL
  }

  list(barPlot,pointPlot,errPlot,
       sm_hgrid(borders=borders, legends=legends))


}
