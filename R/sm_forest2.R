#' Forest plot (updated in smplot2)
#'
#' @param ...
#' A generic aesthetic parameter across points, lines and error bars. This
#' is optional.
#'
#' @param point.params
#' List of parameters for individual points, such as color, alpha, fill etc
#'
#' @param avgPoint.params
#' List of parameters for the average point, such as color, alpha, fill etc
#'
#' @param err.params
#' List of parameters for the error bar from the average point, such as color, alpha etc
#'
#' @param ref.params
#' List of parameters for the vertical reference line, such as color, alpha etc
#'
#' @param xintercept
#' Location of the vertical reference line along the x coordinate.
#'
#' @param sep_level
#' A numerical value that controls the level of the separation between
#' the individual points and the average point.
#' If it's 0, all of these are clustered together. If it's higher (and more positive),
#' the text annotations will increasingly go below the mean point. Default is set to 2. The values
#' can be negative so that the points can be above the mean point. There is no limit of
#' the range for this argument.
#'
#' @param point_jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param errorbar_type
#' This argument determines the error bar type.
#' If it is set to 'se' , standard error bar will be shown.
#' If it is set to 'sd', the error bar will display standard deviation.
#' If it is set to 'ci' (default), the error bar will display 95\% confidence interval.
#'
#' @param points
#' If points is set TRUE, individual points are shown. If FALSE,
#' they are not shown.
#'
#' @param refLine
#' If it is set TRUE, the reference line at a specified location along the x-axis is shown.
#' If it is set FALSE, it is not shown.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#'
#' @return
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @importFrom sdamr position_jitternudge
#' @export
#'
#' @examples
#' \dontrun{
#' library(smplot2)
#' set.seed(2) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' day3 = rnorm(20,6,1.5)
#' day4 = rnorm(20,7,2)
#' Subject <- rep(paste0('S',seq(1:20)), 4)
#' Data <- data.frame(Value = matrix(c(day1,day2,day3,day4),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2', 'Day 3', 'Day 4'), each = length(day1))
#' df2 <- cbind(Subject, Data, Day)
#'
#' df2 %>% ggplot(aes(x = Value, y = Day, color = Day, fill = Day)) +
#'  sm_forest(sep_level = 2, point_jitter_width = .12,
#'            errorbar_type = 'ci',
#'            point.params = list(alpha=0.2, size=  2.5)) +
#'   scale_color_manual(values = sm_palette(4))
#' }
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
  #ref.params <- modifyList(params, ref.params)

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
