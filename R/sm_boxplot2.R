#' A boxplot with jittered individual points
#'
#'
#' @param ...
#' A generic aesthetic parameter across points and the boxplot. This is optional.
#'
#' @param boxplot.params
#' List of parameters for boxplot, such as color, alpha, fill etc
#'
#' @param point.params
#' List of parameters for individual points, such as color, alpha, fill etc
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
#' @param seed
#' Random seed
#'
#' @import ggplot2 cowplot
#' @importFrom utils modifyList
#' @return A boxplot generated using ggplot2
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(smplot2)
#' set.seed(1) # generate random data
#' day1 = rnorm(16,2,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' # with the default aesthetics of smplot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_boxplot() +
#'   scale_color_manual(values = sm_color('blue','orange'))
#'
#' # Without the default aesthetics of smplot
#'
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_boxplot(boxplot.params = list()) +
#' scale_color_manual(values = sm_color('blue','orange'))
#' }
sm_boxplot <- function(...,
                       boxplot.params = list(notch = FALSE, fill = 'gray95', color ='black',
                                             size = 0.5, width=0.5, outlier.shape = NA),
                       point.params = list(alpha = 0.65),
                       point_jitter_width = 0.12, points = TRUE,
                       borders = TRUE, legends = FALSE, seed = NULL) {

  if (length(seed)) set.seed(seed)

  params <- list(...)
  boxplot.params <- modifyList(params, boxplot.params)
  point.params <- modifyList(params, point.params)


  boxPlot <- do.call('geom_boxplot',
                     modifyList(list(), boxplot.params))

  pointPlot <- do.call('geom_point',
                       modifyList(list(position = position_jitter(height=0,
                                                                  width=point_jitter_width)), point.params))
  if (points == FALSE) {
    pointPlot <- NULL
  }

  list(boxPlot,pointPlot,
       sm_hgrid(borders=borders, legends=legends))

}
