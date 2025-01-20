#' Bar Plot with Jittered Individual Points
#'
#' @description
#' Generates a bar plot with optional jittered individual points and error bars.
#' The function supports flexible customization of the bars, points, and error bars,
#' and allows for displaying standard deviation, standard error, or confidence intervals.
#'
#' @param ...
#' Additional aesthetic parameters applied across points, bars, and error bars. Optional.
#'
#' @param bar.params
#' A list of parameters for customizing the bar graph. Common parameters include:
#' \itemize{
#'   \item \code{fill}: Fill color of the bars.
#'   \item \code{color}: Outline color of the bars.
#'   \item \code{alpha}: Transparency level of the bars.
#'   \item \code{width}: Width of the bars.
#' }
#' Default: \code{list(width = 0.7, alpha = 1, color = 'transparent', fill = 'gray80')}.
#'
#' @param err.params
#' A list of parameters for customizing the error bars. Common parameters include:
#' \itemize{
#'   \item \code{color}: Color of the error bars.
#'   \item \code{size}: Size of the error bar endpoints.
#'   \item \code{linewidth}: Width of the error bar lines.
#' }
#' Default: \code{list(linewidth = 1, color = 'black')}.
#'
#' @param point.params
#' A list of parameters for customizing individual points. Common parameters include:
#' \itemize{
#'   \item \code{size}: Size of the points.
#'   \item \code{alpha}: Transparency level of the points.
#'   \item \code{shape}: Shape of the points.
#'   \item \code{color}: Color of the points.
#' }
#' Default: \code{list(size = 2.5, alpha = 0.65, shape = 16)}.
#'
#' @param errorbar_type
#' A string specifying the type of error bars to display:
#' \itemize{
#'   \item \code{'se'}: Standard error.
#'   \item \code{'sd'}: Standard deviation (default).
#'   \item \code{'ci'}: 95% confidence interval.
#' }
#'
#' @param point_jitter_width
#' A numeric value specifying the degree of horizontal jitter applied to individual points.
#' \itemize{
#'   \item If set to \code{0}, points are aligned along the y-axis without jitter.
#'   \item Default: \code{0.12}.
#' }
#'
#' @param points
#' Logical. Determines whether individual points are displayed:
#' \itemize{
#'   \item \code{TRUE}: Display points (default).
#'   \item \code{FALSE}: Hide points.
#' }
#'
#' @param borders
#' Logical. Determines whether grid borders are displayed:
#' \itemize{
#'   \item \code{TRUE}: Display borders (default).
#'   \item \code{FALSE}: Remove borders.
#' }
#'
#' @param legends
#' Logical. Determines whether legends are displayed:
#' \itemize{
#'   \item \code{TRUE}: Display legends.
#'   \item \code{FALSE}: Hide legends (default).
#' }
#'
#' @param seed
#' A numeric value to set a random seed for reproducible jittered points.
#' Default: \code{NULL} (no seed).
#'
#' @param forget
#' Logical. Determines whether to apply the default aesthetic parameters:
#' \itemize{
#'   \item \code{TRUE}: Ignore default aesthetic parameters (\code{bar.params},
#'         \code{err.params}, and \code{point.params}) and apply only user-supplied customizations.
#'   \item \code{FALSE}: Merge user-supplied customizations with the defaults (default).
#' }
#'
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @return
#' A ggplot2 object representing a bar graph with optional jittered points and error bars.
#'
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

    err.params0 <- list(linewidth = 1, color = 'black')
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
