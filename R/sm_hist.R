#' Histogram with kernel density estimation (Gaussian) and rugs
#'
#' @param ...
#' A generic aesthetic parameter across points and the boxplot. This is optional.
#'
#' @param hist.params
#' List of parameters for the histogram, such as binwidth, color, alpha, fill etc.
#'
#' @param density.params
#' List of parameters for the density estimation, such as color, size, alpha etc
#'
#' @param rug.params
#' List of parameters for the rugs, such as color, size, alpha etc
#'
#' @param histogram
#' TRUE if the histogram needs to be shown.
#' FALSE if the histogram needs to be hidden.
#'
#' @param density
#' TRUE if the density plot needs to be shown.
#' FALSE if the density plot needs to be hidden.
#'
#' @param rug
#' TRUE if the rugs need to be shown.
#' FALSE if the rugs need to be hidden.
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
#' Forget the defaults when list() is called for a specific parameter.
#' Set to TRUE when when users want to map aesthetics to different groups more flexibly..
#' Set to FALSE by default.
#'
#'
#' @return Returns a histogram generated using ggplot2.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#' #
#' set.seed(2)
#' data <- data.frame(value = rnorm(1000))
#' data2 <- data.frame(value = rnorm(1000, 5, 1))
#'
#' data$day <- "day1"
#' data2$day <- "day2"
#' rbind(data, data2) -> df
#'
#' ggplot(data = data, aes(x = value)) +
#'   sm_hist()
#'
#' ggplot(data = df, aes(x = value, fill = day, color = day)) +
#'   sm_hist(
#'     hist.params = list(binwidth = 1 / 2, alpha = 0.3),
#'     density.params = list(fill = "transparent", size = 0.8),
#'     rug.params = list(alpha = 0.8)
#'   ) +
#'   scale_color_manual(values = sm_palette(2)) +
#'   scale_fill_manual(values = sm_palette(2))
#'
sm_hist <- function(...,
                    hist.params = list(
                      binwidth = 1 / 2, fill = sm_color("blue"),
                      color = "white",
                      alpha = 0.4
                    ),
                    density.params = list(
                      color = sm_color("blue"), size = 0.8,
                      fill = "transparent"
                    ),
                    rug.params = list(
                      color = sm_color("blue"), alpha = 0.8,
                      size = 0.4
                    ),
                    histogram = TRUE,
                    density = TRUE,
                    rug = TRUE,
                    borders = FALSE,
                    legends = FALSE,
                    forget = FALSE) {
  params <- list(...)

  if (forget == FALSE) {
    hist.params0 <- list(
      binwidth = 1 / 2, fill = sm_color("blue"),
      color = "white",
      alpha = 0.4
    )
    hist.params0 <- modifyList(hist.params0, params)
    density.params0 <- list(
      color = sm_color("blue"), size = 0.8,
      fill = "transparent"
    )
    density.params0 <- modifyList(density.params0, params)
    rug.params0 <- list(
      color = sm_color("blue"), alpha = 0.8,
      size = 0.4
    )
    rug.params0 <- modifyList(rug.params0, params)

    hist.params <- modifyList(hist.params0, hist.params)
    density.params <- modifyList(density.params0, density.params)
    rug.params <- modifyList(rug.params0, rug.params)
  } else if (forget == TRUE) {
    hist.params <- modifyList(params, hist.params)
    density.params <- modifyList(params, density.params)
    rug.params <- modifyList(params, rug.params)
  }


  histPlot <- do.call(
    "geom_histogram",
    modifyList(list(), hist.params)
  )

  densityPlot <- do.call(
    "geom_density",
    modifyList(
      list(aes(y = hist.params$binwidth * after_stat(count))),
      density.params
    )
  )

  rugPlot <- do.call(
    "geom_rug",
    modifyList(list(), rug.params)
  )

  if (density == FALSE) {
    densityPlot <- NULL
  }

  if (rug == FALSE) {
    rugPlot <- NULL
  }

  if (histogram == FALSE) {
    histPlot <- NULL
  }

  list(
    histPlot, densityPlot, rugPlot,
    sm_hgrid(borders = borders, legends = legends),
    ggplot2::ylab("Count")
  )
}

globalVariables(c("count"))
