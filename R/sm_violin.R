#' Violin Plot with Jittered Individual Points
#'
#' @description
#' Generates a violin plot with optional jittered individual points and error bars.
#' This function allows for flexible customization of the violin, points, and error bars,
#' and supports displaying standard deviation, standard error, or confidence intervals as error bars.
#'
#' @param ...
#' Additional aesthetic parameters applied across points, the violin plot, and error bars. Optional.
#'
#' @param violin.params
#' A list of parameters for customizing the violin plot. Common parameters include:
#' \itemize{
#'   \item \code{fill}: Fill color of the violin plot.
#'   \item \code{color}: Outline color of the violin plot.
#'   \item \code{alpha}: Transparency level of the violin plot.
#' }
#' Default: \code{list(fill = 'gray90', color = 'transparent')}.
#'
#' @param err.params
#' A list of parameters for customizing the error bars. Common parameters include:
#' \itemize{
#'   \item \code{size}: Size of the error bar endpoints.
#'   \item \code{linewidth}: Width of the error bar lines.
#'   \item \code{color}: Color of the error bars.
#' }
#' Default: \code{list(size = 1.2, linewidth = 1.2)}.
#'
#' @param point.params
#' A list of parameters for customizing individual points. Common parameters include:
#' \itemize{
#'   \item \code{alpha}: Transparency level of the points.
#'   \item \code{size}: Size of the points.
#'   \item \code{shape}: Shape of the points.
#' }
#' Default: \code{list(alpha = 0.25, size = 2)}.
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
#'   \item Default: \code{0.17}.
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
#'   \item \code{TRUE}: Ignore default aesthetic parameters (\code{violin.params},
#'         \code{err.params}, and \code{point.params}) and apply only user-supplied customizations.
#'   \item \code{FALSE}: Merge user-supplied customizations with the defaults (default).
#' }
#'
#' @return
#' A ggplot2 object containing a violin plot with optional jittered points and error bars.
#'
#'
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#' set.seed(1) # generate random data
#' day1 <- rnorm(16, 2, 1)
#' day2 <- rnorm(16, 5, 1)
#' Subject <- rep(paste0("S", seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1, day2), ncol = 1))
#' Day <- rep(c("Day 1", "Day 2"), each = length(day1))
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
#'
#'
sm_violin <- function(...,
                      violin.params = list(
                        fill = "gray90",
                        color = "transparent"
                      ),
                      err.params = list(size = 1.2, linewidth = 1.2),
                      point.params = list(alpha = 0.25, size = 2),
                      errorbar_type = "sd",
                      point_jitter_width = 0.17,
                      points = TRUE,
                      borders = TRUE,
                      legends = FALSE, seed = NULL, forget = FALSE) {
  if (length(seed)) set.seed(seed)
  params <- list(...)

  if (forget == FALSE) {
    violin.params0 <- list(fill = "gray90", color = "transparent")
    violin.params0 <- modifyList(violin.params0, params)
    err.params0 <- list(size = 1.2, linewidth = 1.2)
    err.params0 <- modifyList(err.params0, params)
    point.params0 <- list(alpha = 0.25, size = 2)
    point.params0 <- modifyList(point.params0, params)

    violin.params <- modifyList(violin.params0, violin.params)
    err.params <- modifyList(err.params0, err.params)
    point.params <- modifyList(point.params0, point.params)
  } else if (forget == TRUE) {
    violin.params <- modifyList(params, violin.params)
    err.params <- modifyList(params, err.params)
    point.params <- modifyList(params, point.params)
  }


  violinPlot <- do.call(
    "geom_violin",
    modifyList(list(), violin.params)
  )

  pointPlot <- do.call(
    "geom_point",
    modifyList(list(position = position_jitter(
      height = 0,
      width = point_jitter_width
    )), point.params)
  )

  if (errorbar_type == "se") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun.data = mean_se,
        geom = "pointrange"
      ), err.params)
    )
  } else if (errorbar_type == "sd") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun.data = mean_sdl,
        fun.args = list(mult = 1),
        geom = "pointrange",
        fatten = point.params$size * 1.3
      ), err.params)
    )
  } else if (errorbar_type == "ci") {
    errPlot <- do.call(
      "stat_summary",
      modifyList(list(
        fun.data = mean_cl_boot,
        geom = "pointrange"
      ), err.params)
    )
  } else {
    stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
  }

  if (points == FALSE) {
    pointPlot <- NULL
  }

  list(
    violinPlot, pointPlot, errPlot,
    sm_hgrid(borders = borders, legends = legends)
  )
}
