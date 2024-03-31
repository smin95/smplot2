#' A violin plot with jittered individual points (updated in smplot2)
#'
#' @param ...
#' A generic aesthetic parameter across points and the violin plot. This is optional.
#'
#' @param violin.params
#' List of parameters for the violin, such as color, alpha, fill etc
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
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @return Violin plot generated using ggplot2
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

#' # with aesthetic defaults of smplot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_violin() +
#' scale_color_manual(values = sm_color('blue','orange'))
#'
#' # without aesthetic defaults of smplot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_violin(violin.params = list()) +
#' scale_color_manual(values = sm_color('blue','orange'))
#' }
#'
sm_violin <- function(...,
                      violin.params = list(fill = 'gray90',
                                           color = 'transparent'),
                      err.params = list(size = 1.2),
                      point.params = list(alpha = 0.2),
                      errorbar_type = 'sd',
                      point_jitter_width = 0.17,
                      points = TRUE,
                      borders = TRUE,
                      legends =  FALSE) {

  params <- list(...)
  violin.params <- modifyList(params, violin.params)
  err.params <- modifyList(params, err.params)
  point.params <- modifyList(params, point.params)

  violinPlot <- do.call('geom_violin',
                        modifyList(list(), violin.params))

  pointPlot <- do.call('geom_point',
                       modifyList(list(position = position_jitter(height=0,
                                                                  seed=10,
                                                                  width=point_jitter_width)), point.params))

  if (errorbar_type == 'se') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun.data = mean_se,
                                       geom = 'pointrange'), err.params))
  } else if (errorbar_type == 'sd') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun.data = mean_sdl,
                                       fun.args = list(mult = 1),
                                       geom = 'pointrange',
                                       fatten = point.params$size*1.3),err.params))
  } else if (errorbar_type == 'ci') {
    errPlot <- do.call('stat_summary',
                       modifyList(list(fun.data = mean_cl_boot,
                                       geom = 'pointrange'), err.params))
  } else {
    stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
  }

  if (points == FALSE) {
    pointPlot <- NULL
  }

  list(violinPlot,pointPlot,errPlot,
       sm_hgrid(borders=borders, legends=legends))

}
