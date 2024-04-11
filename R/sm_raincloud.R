#' Raincloud plot
#'
#' @description
#' This function visualizes a raincloud plot, which is a combination of jittered points, boxplots
#' and violin plots. The creation of this function
#' has been inspired by the R package called 'raincloudplots' by Jordy van
#' Langen.
#'
#' This function has been created to allow more customisation than the functions
#' in the raincloudplots package. Also, this function automatically sorts the data given the
#' condition that the x-axis factor levels have been sorted properly.
#'
#'
#' @param ...
#' A generic aesthetic parameter across points, boxplot and violin. This
#' is optional.
#'
#' @param boxplot.params
#' List of parameters for boxplot, such as color, alpha, fill etc
#'
#' @param violin.params
#' List of parameters for violin, such as color, alpha, fill etc
#'
#' @param point.params
#' List of parameters for individual points, such as color, alpha, fill etc
#'
#' @param which_side
#' String argument to specify the side of the boxplots and violinplots.
#' The options are: 'right' and 'left'. 'mixed' has been removed from smplot due
#' to its lack of usage.
#'
#' @param sep_level
#' A numerical value that controls the level of the separation among
#' the boxplot, violin plot and the points. The value can be 0-4.
#' If it's 0, all of these are clustered together. If it's 3, they are all
#' separated. 1 and 2 are somewhere in the middle. Default is set to 2.
#'
#' @param point_jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param vertical
#' The orientation of the plots. The default is set to TRUE.
#' If you want the horizontal orientation of the plot, set this argument
#' as FALSE.
#'
#' @param points
#' If the points need to be displayed, the input should be TRUE.
#' If the points are not needed, the input should be FALSE.
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
#' @return Returns a raincloud plot generated using ggplot2.
#' @import ggplot2 cowplot Hmisc
#' @importFrom stats sd
#' @importFrom utils modifyList
#' @importFrom gghalves geom_half_boxplot geom_half_violin
#' @importFrom sdamr position_jitternudge
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' library(smplot2)
#'
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
#' ggplot(data=df2, aes(x = Day, y = Value, color = Day, fill = Day)) +
#' sm_raincloud() +
#'  xlab('Day')  +
#'  scale_fill_manual(values = sm_palette(4))
#'
#' }
sm_raincloud <- function(...,
                         boxplot.params = list(),
                         violin.params = list(alpha = 0.3, color = 'transparent'),
                         point.params = list(alpha = 1, size = 3, shape = 21,
                                             color = 'transparent'),
                         which_side = 'r',
                         sep_level = 2,
                         point_jitter_width = 0.12,
                         vertical = TRUE,
                         points = TRUE,
                         borders = TRUE,
                         legends = FALSE,
                         seed = NULL) {

  if (length(seed)) set.seed(seed)
  if (which_side == 'right') {
    which_side <- 'r'
  } else if (which_side == 'left') {
    which_side <- 'l'
  }


  params <- list(...)
  point.params <- modifyList(params, point.params)
  boxplot.params <- modifyList(params, boxplot.params)
  violin.params <- modifyList(params, violin.params)


  if (which_side == 'r') {
    if (sep_level == 4) {
      position_nudge_vector <- c(-0.2, 0,0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(-0.15, 0,0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(-0.15,0,0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(-0.08,0,0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,0)
    }
  } else if (which_side == 'l') {
    if (sep_level == 4) {
      position_nudge_vector <- c(0.2, 0,-0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(0.15, 0,-0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(0.15,0,0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(0.08,0,0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,0)
    }
  }

  pointPlot <- do.call('geom_point',
                       modifyList(list(position = sdamr::position_jitternudge(jitter.width=point_jitter_width,
                                                                              jitter.height=0,
                                                                              nudge.x = position_nudge_vector[1])),
                                  point.params))



  boxPlot <- do.call('geom_half_boxplot',
                     modifyList(list(position = position_nudge(x = position_nudge_vector[2]),
                                     side = which_side,
                                     errorbar.draw = FALSE, width = 0.2,
                                     color = 'black'),
                                boxplot.params))

  violinPlot <- do.call('geom_half_violin',
                        modifyList(list(position = position_nudge(x = position_nudge_vector[3]),
                                        side = which_side),
                                   violin.params))


  if (points == FALSE) {
    pointPlot <- NULL
  }



  if (vertical == FALSE) {
    fig <-  list(violinPlot,boxPlot,pointPlot,
                 sm_hgrid(borders=borders, legends=legends), coord_flip())
  } else if (vertical == TRUE) {
    fig <- list(violinPlot,boxPlot,pointPlot,
                sm_hgrid(borders=borders, legends=legends))

  } else {
    stop('vertical argument must be TRUE or FALSE.')
  }

  return(fig)


}
