#' Slope Chart
#'
#' @description
#' Generates a slope chart, which is particularly useful for comparing effects between two time points.
#' The function supports grouped data and provides various customization options for lines, points, error bars,
#' and x-axis ticks. Users can specify the type of error bars and control whether to display mean and error bars.
#'
#' The mapping in `ggplot()` requires grouping each observation to ensure correct pairing of points.
#'
#' @param ...
#' Additional aesthetic parameters applied across points and lines, such as \code{color}, \code{alpha}, and \code{fill}. Optional.
#'
#' @param labels
#' A vector specifying the labels for the x-axis ticks. This is a required argument.
#' For example: \code{c('Day 1', 'Day 2')}.
#'
#' @param group
#' The name of the variable used to group individual data points. This is a required argument.
#'
#' @param line.params
#' A list of parameters for individual lines. Common parameters include:
#' \itemize{
#'   \item \code{color}: Color of the lines.
#'   \item \code{alpha}: Transparency of the lines.
#'   \item \code{linewidth}: Width of the lines.
#' }
#' Default: \code{list(color = 'gray53', linewidth = 0.4, alpha = 0.4)}.
#'
#' @param point.params
#' A list of parameters for individual points. Common parameters include:
#' \itemize{
#'   \item \code{size}: Size of the points.
#'   \item \code{shape}: Shape of the points.
#'   \item \code{color}: Color of the points.
#' }
#' Default: \code{list(size = 2.5, shape = 21, color = 'white')}.
#'
#' @param avgLine.params
#' A list of parameters for the average line. Common parameters include:
#' \itemize{
#'   \item \code{color}: Color of the average line.
#'   \item \code{linewidth}: Width of the average line.
#' }
#' Default: \code{list(linewidth = 1)}.
#'
#' @param avgPoint.params
#' A list of parameters for the average points. Common parameters include:
#' \itemize{
#'   \item \code{size}: Size of the average points.
#'   \item \code{fill}: Fill color of the average points.
#' }
#' Default: \code{list(size = 4)}.
#'
#' @param err.params
#' A list of parameters for error bars. Common parameters include:
#' \itemize{
#'   \item \code{color}: Color of the error bars.
#'   \item \code{linewidth}: Width of the error bars.
#' }
#' Default: \code{list(linewidth = 1)}.
#'
#' @param xTick.params
#' A list of parameters for customizing the x-axis ticks. Common options include:
#' \itemize{
#'   \item \code{position}: Location of the ticks (default: \code{'top'}).
#'   \item \code{expand}: Space around the ticks (default: \code{c(0.17, 0.1)}).
#'   \item \code{drop}: Whether to drop unused factor levels (default: \code{FALSE}).
#' }
#'
#' @param errorbar_type
#' A string specifying the type of error bars to display:
#' \itemize{
#'   \item \code{'se'}: Standard error.
#'   \item \code{'sd'}: Standard deviation (default).
#'   \item \code{'ci'}: 95% confidence interval.
#' }
#'
#' @param many_groups
#' Logical. Determines whether the average line is plotted for each group:
#' \itemize{
#'   \item \code{TRUE}: An average line is plotted for each group.
#'   \item \code{FALSE}: A single average line is plotted across all data.
#' }
#' Default: \code{FALSE}.
#'
#' @param show_err
#' Logical. Determines whether to display error bars:
#' \itemize{
#'   \item \code{TRUE}: Display error bars.
#'   \item \code{FALSE}: Hide error bars (default).
#' }
#'
#' @param show_mean
#' Logical. Determines whether to display the average line and points:
#' \itemize{
#'   \item \code{TRUE}: Display the average line and points.
#'   \item \code{FALSE}: Hide the average line and points (default).
#' }
#'
#' @param legends
#' Logical. Determines whether to display legends:
#' \itemize{
#'   \item \code{TRUE}: Display legends.
#'   \item \code{FALSE}: Hide legends (default).
#' }
#'
#' @param forget
#' Logical. Determines whether to apply the default aesthetic parameters:
#' \itemize{
#'   \item \code{TRUE}: Ignore default aesthetic parameters (\code{line.params}, \code{point.params}, etc.)
#'         and apply only user-supplied customizations.
#'   \item \code{FALSE}: Merge user-supplied customizations with the defaults (default).
#' }
#'
#' @return
#' A list of ggplot2 layers for creating a slope chart.
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
#' day1 <- rnorm(16, 2, 1)
#' day2 <- rnorm(16, 5, 1)
#' Subject <- rep(paste0("S", seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1, day2), ncol = 1))
#' Day <- rep(c("Day 1", "Day 2"), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' ggplot(data = df, aes(x = Day, y = Value, fill = Day)) +
#'   sm_slope(labels = c("Day 1", "Day 2"), group = Subject) +
#'   scale_fill_manual(values = sm_color("blue", "orange"))

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
                     line.params = list(
                       color = "gray53",
                       linewidth = 0.4,
                       alpha = 0.4
                     ),
                     point.params = list(
                       size = 2.5, shape = 21,
                       color = "white"
                     ),
                     avgLine.params = list(linewidth = 1),
                     avgPoint.params = list(size = 4),
                     err.params = list(linewidth = 1),
                     xTick.params = list(
                       position = "top",
                       expand = c(0.17, .1),
                       drop = FALSE
                     ),
                     errorbar_type = "sd",
                     many_groups = FALSE,
                     show_err = FALSE,
                     show_mean = FALSE,
                     legends = FALSE,
                     forget = FALSE) {
  if (missing(group)) {
    stop("group (of the shadow) must be specified because each observation has to be paired.")
  }

  if (missing(labels)) {
    labels <- rev(letters)
  }

  params <- list(...)

  if (forget == FALSE) {
    line.params0 <- list(
      color = "gray53", linewidth = 0.4,
      alpha = 0.4
    )
    line.params0 <- modifyList(line.params0, params)

    point.params0 <- list(
      size = 2.5, shape = 21,
      color = "white"
    )
    point.params0 <- modifyList(point.params0, params)

    avgLine.params0 <- list(linewidth = 1)
    avgLine.params0 <- modifyList(avgLine.params0, params)

    avgPoint.params0 <- list(size = 4)
    avgPoint.params0 <- modifyList(avgPoint.params0, params)

    err.params0 <- list(linewidth = 1)
    err.params0 <- modifyList(err.params0, params)

    xTick.params0 <- list(position = "top", expand = c(0.17, .1), drop = FALSE)


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
    # xTick.params <- modifyList(params, xTick.params)
  }


  if (errorbar_type == "se") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun.data = mean_se,
        geom = "linerange"
      ), err.params)
    )
  } else if (errorbar_type == "sd") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(
        list(
          fun = mean,
          fun.min = function(x) mean(x) - sd(x),
          fun.max = function(x) mean(x) + sd(x),
          geom = "linerange"
        ),
        err.params
      )
    )
  } else if (errorbar_type == "ci") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun.data = mean_cl_boot,
        geom = "linerange"
      ), err.params)
    )
  } else {
    stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
  }


  linePlot <- do.call(
    "geom_line",
    modifyList(list(aes(group = {{ group }})), line.params)
  )

  pointPlot <- do.call(
    "geom_point",
    modifyList(list(aes(group = {{ group }})), point.params)
  )

  if (many_groups == FALSE) {
    avgLinePlot <- do.call(
      "stat_summary",
      modifyList(list(aes(group = 1),
        fun = mean,
        geom = "line"
      ), avgLine.params)
    )
  } else if (many_groups == TRUE) {
    avgLinePlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun = mean,
        geom = "line"
      ), avgLine.params)
    )
  } else {
    stop("many_groups has to be either TRUE or FALSE.")
  }

  avgPointPlot <- do.call(
    "stat_summary",
    modifyList(list(
      fun = mean,
      geom = "point"
    ), avgPoint.params)
  )

  scaleX <- do.call(
    "scale_x_discrete",
    modifyList(list(labels = labels), xTick.params)
  )


  if (show_err == FALSE) {
    errPlot <- NULL
  }

  if (show_mean == FALSE) {
    avgLinePlot <- NULL
    avgPointPlot <- NULL
    errPlot <- NULL
  }

  list(
    linePlot, pointPlot, avgLinePlot,
    avgPointPlot, errPlot, scaleX,
    sm_slope_theme(legends = legends)
  )
}
