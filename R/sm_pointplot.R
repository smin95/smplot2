#' Point plot with optional shadow
#'
#' This is a common plot with mean point, standard error (se, sd or 95% CI) and
#' uniquely shadow, which is a faint display of individual points behind the mean.
#'
#' @param ...
#'
#' A generic aesthetic parameter across points, lines and errorbars. This is optional.
#' This will be extremely useful for dodging each line using position_dodge().
#'
#' @param avgPoint.params
#' List of parameters for the average point, such as color, alpha, fill etc
#'
#' @param avgLine.params
#' List of parameters for the average line, such as color, alpha etc
#'
#' @param point.params
#' List of parameters for the points in the shadow, such as color, alpha, fill etc
#'
#' @param line.params
#' List of parameters for the lines in the shadow, such as color, alpha etc
#'
#' @param err.params
#' List of parameters for the error bar from the average plot, such as color, alpha etc
#'
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param show_shadow
#' If it is TRUE, it will show the shadow.
#' If it is FALSE, it will not show the shadow (default).
#'
#' @param group
#' If show_shadow = TRUE, this argument is required. This is the variable that
#' each plot from the shadow should be grouped along aesthetically.
#' It should be grouped for each individual observation,
#' ex. sm_pointplot(group = Subject), whereby Subject is the column that holds identifers
#' for each observation.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
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
#' @return Returns a pointplot generated using ggplot2.
#' @export
#'
#' @examples
#' library(smplot2)
#' library(ggplot2)
#' ggplot(data = mtcars, mapping = aes(x = cyl, y = mpg)) +
#'   sm_pointplot()
#'
sm_pointplot <- function(...,
                         avgPoint.params = list(size = 2.5),
                         avgLine.params = list(linewidth = 1),
                         point.params = list(
                           alpha = 0.35, color = "gray",
                           fill = "gray"
                         ),
                         line.params = list(alpha = 0.35, color = "gray"),
                         err.params = list(linewidth = 1),
                         errorbar_type = "se",
                         show_shadow = FALSE,
                         group = NULL,
                         borders = TRUE,
                         legends = FALSE,
                         forget = FALSE) {
  if (show_shadow == TRUE) {
    if (missing(group)) {
      stop("When show_shadow = TRUE, group (of the shadow) must be specified")
    }
  }

  params <- list(...)

  if (forget == FALSE) {
    avgPoint.params0 <- list(size = 3)
    avgPoint.params0 <- modifyList(avgPoint.params0, params)
    avgLine.params0 <- list(linewidth = 1)
    avgLine.params0 <- modifyList(avgLine.params0, params)
    point.params0 <- list(alpha = 0.35, color = "gray", fill = "gray")
    point.params0 <- modifyList(point.params0, params)
    line.params0 <- list(alpha = 0.35, color = "gray")
    line.params0 <- modifyList(line.params0, params)
    err.params0 <- list(linewidth = 1)
    err.params0 <- modifyList(err.params0, params)

    avgPoint.params <- modifyList(avgPoint.params0, avgPoint.params)
    avgLine.params <- modifyList(avgLine.params0, avgLine.params)
    line.params <- modifyList(line.params0, line.params)
    point.params <- modifyList(point.params0, point.params)
    err.params <- modifyList(err.params0, err.params)
  } else if (forget == TRUE) {
    avgPoint.params <- modifyList(params, avgPoint.params)
    avgLine.params <- modifyList(params, avgLine.params)
    line.params <- modifyList(params, line.params)
    point.params <- modifyList(params, point.params)
    err.params <- modifyList(params, err.params)
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

  avgLinePlot <- do.call(
    "stat_summary",
    modifyList(list(
      fun = mean,
      geom = "line"
    ), avgLine.params)
  )
  avgPointPlot <- do.call(
    "stat_summary",
    modifyList(list(
      fun = mean,
      geom = "point"
    ), avgPoint.params)
  )

  if (show_shadow == FALSE) {
    linePlot <- NULL
    pointPlot <- NULL
  }

  list(
    linePlot, pointPlot, avgLinePlot, errPlot, avgPointPlot,
    sm_hgrid(borders = borders, legends = legends)
  )
}
