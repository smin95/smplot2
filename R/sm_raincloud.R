#' Raincloud Plot
#'
#' @description
#' Creates a raincloud plot, a combination of jittered points, boxplots, and violin plots.
#' Inspired by the 'raincloudplots' R package by Jordy van Langen, this function offers
#' enhanced customization and automatic sorting of data based on the x-axis factor levels.
#'
#' This function allows detailed control over the aesthetics of individual components
#' (boxplot, violin, and points) and provides options to adjust layout and orientation.
#'
#' @param ...
#' Additional aesthetic parameters applied across all elements (points, boxplot, and violin plot). Optional.
#'
#' @param boxplot.params
#' A list of parameters for customizing the boxplot. Common options include:
#' \itemize{
#'   \item \code{fill}: Fill color of the boxplot.
#'   \item \code{color}: Outline color of the boxplot.
#'   \item \code{alpha}: Transparency level of the boxplot.
#'   \item \code{width}: Width of the boxplot.
#' }
#' Default: \code{list()}.
#'
#' @param violin.params
#' A list of parameters for customizing the violin plot. Common options include:
#' \itemize{
#'   \item \code{alpha}: Transparency level of the violin plot.
#'   \item \code{color}: Outline color of the violin plot.
#'   \item \code{fill}: Fill color of the violin plot.
#' }
#' Default: \code{list(alpha = 0.3, color = 'transparent')}.
#'
#' @param point.params
#' A list of parameters for customizing individual points. Common options include:
#' \itemize{
#'   \item \code{alpha}: Transparency level of the points.
#'   \item \code{size}: Size of the points.
#'   \item \code{shape}: Shape of the points.
#'   \item \code{color}: Outline color of the points.
#' }
#' Default: \code{list(alpha = 1, size = 3, shape = 21, color = 'transparent')}.
#'
#' @param which_side
#' Specifies the side of the violin and boxplots. Options:
#' \itemize{
#'   \item \code{'right'}: Displays elements on the right side (default).
#'   \item \code{'left'}: Displays elements on the left side.
#' }
#' The \code{'mixed'} option has been removed due to limited usage.
#'
#' @param sep_level
#' A numeric value (0-4) controlling the separation level among the boxplot, violin plot, and points:
#' \itemize{
#'   \item \code{0}: All elements are clustered together.
#'   \item \code{4}: All elements are maximally separated.
#'   \item Intermediate values (\code{1-3}) provide varying levels of separation.
#' }
#' Default: \code{2}.
#'
#' @param point_jitter_width
#' A numeric value determining the horizontal jitter for individual points:
#' \itemize{
#'   \item If set to \code{0}, points are aligned along the y-axis without jitter.
#'   \item Default: \code{0.12}.
#' }
#'
#' @param vertical
#' Logical. Specifies the orientation of the plot:
#' \itemize{
#'   \item \code{TRUE}: Vertical orientation (default).
#'   \item \code{FALSE}: Horizontal orientation.
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
#'   \item \code{TRUE}: Ignore default aesthetic parameters (\code{boxplot.params},
#'         \code{violin.params}, and \code{point.params}) and apply only user-supplied customizations.
#'   \item \code{FALSE}: Merge user-supplied customizations with the defaults (default).
#' }
#'
#' @return
#' A list of ggplot2 layers for creating a raincloud plot.
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#'
#' set.seed(2) # generate random data
#' day1 <- rnorm(20, 0, 1)
#' day2 <- rnorm(20, 5, 1)
#' day3 <- rnorm(20, 6, 1.5)
#' day4 <- rnorm(20, 7, 2)
#' Subject <- rep(paste0("S", seq(1:20)), 4)
#' Data <- data.frame(Value = matrix(c(day1, day2, day3, day4), ncol = 1))
#' Day <- rep(c("Day 1", "Day 2", "Day 3", "Day 4"), each = length(day1))
#' df2 <- cbind(Subject, Data, Day)
#'
#' ggplot(data = df2, aes(x = Day, y = Value, color = Day, fill = Day)) +
#'   sm_raincloud() +
#'   xlab("Day") +
#'   scale_fill_manual(values = sm_palette(4))
#'
sm_raincloud <- function(...,
                         boxplot.params = list(),
                         violin.params = list(alpha = 0.3, color = "transparent"),
                         point.params = list(
                           alpha = 1, size = 3, shape = 21,
                           color = "transparent"
                         ),
                         which_side = "r",
                         sep_level = 2,
                         point_jitter_width = 0.12,
                         vertical = TRUE,
                         points = TRUE,
                         borders = TRUE,
                         legends = FALSE,
                         seed = NULL,
                         forget = FALSE) {
  if (length(seed)) set.seed(seed)
  if (which_side == "right") {
    which_side <- "r"
  } else if (which_side == "left") {
    which_side <- "l"
  }

  params <- list(...)



  if (forget == FALSE) {
    boxplot.params0 <- list()
    boxplot.params0 <- modifyList(boxplot.params0, params)

    violin.params0 <- list(alpha = 0.3, color = "transparent")
    violin.params0 <- modifyList(violin.params0, params)

    point.params0 <- list(alpha = 1, size = 3, shape = 21, color = "transparent")
    point.params0 <- modifyList(point.params0, params)

    point.params <- modifyList(point.params0, point.params)
    boxplot.params <- modifyList(boxplot.params0, boxplot.params)
    violin.params <- modifyList(violin.params0, violin.params)
  } else if (forget == TRUE) {
    point.params <- modifyList(params, point.params)
    boxplot.params <- modifyList(params, boxplot.params)
    violin.params <- modifyList(params, violin.params)
  }



  if (which_side == "r") {
    if (sep_level == 4) {
      position_nudge_vector <- c(-0.2, 0, 0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(-0.15, 0, 0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(-0.15, 0, 0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(-0.08, 0, 0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0, 0, 0)
    }
  } else if (which_side == "l") {
    if (sep_level == 4) {
      position_nudge_vector <- c(0.2, 0, -0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(0.15, 0, -0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(0.15, 0, 0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(0.08, 0, 0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0, 0, 0)
    }
  }

  pointPlot <- do.call(
    "geom_point",
    modifyList(
      list(position = position_jitternudge(
        jitter.width = point_jitter_width,
        jitter.height = 0,
        nudge.x = position_nudge_vector[1]
      )),
      point.params
    )
  )



  boxPlot <- do.call(
    "geom_half_boxplot",
    modifyList(
      list(
        position = position_nudge(x = position_nudge_vector[2]),
        side = which_side,
        errorbar.draw = FALSE, width = 0.2,
        color = "black"
      ),
      boxplot.params
    )
  )

  violinPlot <- do.call(
    "geom_half_violin",
    modifyList(
      list(
        position = position_nudge(x = position_nudge_vector[3]),
        side = which_side
      ),
      violin.params
    )
  )


  if (points == FALSE) {
    pointPlot <- NULL
  }



  if (vertical == FALSE) {
    fig <- list(
      violinPlot, boxPlot, pointPlot,
      sm_hgrid(borders = borders, legends = legends), coord_flip()
    )
  } else if (vertical == TRUE) {
    fig <- list(
      violinPlot, boxPlot, pointPlot,
      sm_hgrid(borders = borders, legends = legends)
    )
  } else {
    stop("vertical argument must be TRUE or FALSE.")
  }

  return(fig)
}
