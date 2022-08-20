#' A raincloud plot - a combination of jittered points, boxplots
#' and violin plots.
#'
#' @description
#' This function visualizes a raincloud plot. The creation of this package
#' has been inspired by the R package called 'raincloudplots' by Jordy van
#' Langen (https://github.com/jorvlan/raincloudplots).
#'
#' This function has been created to allow more customisation than the functions
#' in the raincloudplots package. Also, this function automatically sorts the data given the
#' condition that the x-axis factor levels have been sorted properly.
#'
#' @param data
#' Data frame. Each column has to be a variable.
#' Each row has to be an observation. This data will get used as
#' argument in ggplot().
#'
#' @param x
#' Column variable of the data that defines the level of the x-axis.
#' This is used to define the aes() in ggplot().
#' @param y
#' Column variable of the data that defines the level of the y-axis.
#' This is used to define the aes() in ggplot().
#'
#' @param which_side
#' String argument to specify the side of the boxplots and violinplots.
#' The options are: 'right', 'left', and 'mixed'. 'mixed' only works
#' when there are 2 levels in the x-axis. Otherwise, it will return an error.
#'
#' @param vertical
#' The orientation of the plots. The default is set to TRUE.
#' If you want the horizontal orientation of the plot, please set this argument
#' as FALSE.
#'
#' @param sep_level
#' A numerical value that controls the level of the separation among
#' the boxplot, violin plot and the points. The value can be 0-4.
#' If it's 0, all of these are clustered together. If it's 3, they are all
#' separated. 1 and 2 are somewhere in the middle. Default is set to 2.
#'
#' @param jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param point_size
#' Size of the points.
#'
#' @param violin_alpha
#' Transparency of the violin (0 to 1).
#'
#' @param boxplot_alpha
#' Transparency of the boxplot (0 to 1).
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @param ...
#' Parameters for the points' aesthetics, such as 'fill', 'shape', 'color',
#' and 'alpha'. For more information, please type ?geom_point
#'
#' @import ggplot2 cowplot
#' @importFrom gghalves geom_half_boxplot geom_half_violin
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate
#' @importFrom utils globalVariables
#' @export
#'
#' @references
#' Allen M, Poggiali D, Whitaker K et al. Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved]. Wellcome Open Res 2021, 4:63. DOI: 10.12688/wellcomeopenres.15191.2
#'
#' @examples
#' \dontrun{
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
#' # use the random data to generate a raincloud plot
#' sm_raincloud(data = df2, x = Day, y = Value)
#' }


sm_raincloud <- function(data, x, y,
                         which_side = 'right',
                         sep_level = 2,
                         vertical = TRUE,
                         jitter_width = 0.09,
                         point_size = 3,
                         violin_alpha = 0.3,
                         boxplot_alpha = 1,
                         borders = TRUE,
                         legends = FALSE,
                         ...) {

  df <- data %>% dplyr::mutate(x_axis = as.numeric(factor({{x}}))) %>%
    dplyr::mutate(jit = jitter(x_axis, amount = jitter_width))

  nLevels = length(unique(df$x_axis))

  if ((which_side == 'mixed')) {
    if (nLevels == 2) {
      if (sep_level == 4) {
        position_nudge_vector <- c(-0.3,0.2,-0.4,0.4)
      } else if (sep_level == 3) {
        position_nudge_vector <- c(-0.3,0.2,-0.35,0.35)
      } else if (sep_level == 2) {
        position_nudge_vector <- c(-0.3,0.2,-0.2,0.2)
      } else if (sep_level == 1) {
        position_nudge_vector <- c(-0.2,0.1,-0.1,0.1)
      } else if (sep_level == 0) {
        position_nudge_vector <- c(-0.1,0,0,0)
      }
    } else {
      stop('For mixed sides, the number of levels in the x-axis must be 2.')
    }
  } else if (which_side == 'right') {
    side = 'r'
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
  } else if (which_side == 'left') {
    side = 'l'
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

  if ((which_side == 'mixed') & (nLevels == 2)) {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 1),
                       aes(x = x_axis, y = {{y}}), side = 'l',
                       position = position_nudge(x = position_nudge_vector[3]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 2),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[4]), alpha = violin_alpha) +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 1),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[1]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 2),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[2]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df %>% dplyr::filter(x_axis == 1),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 2),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +
      xlab('x-axis label') + sm_hgrid(borders = borders, legends = legends)

  } else {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_half_violin(data = df,
                       aes(x = x_axis, y = {{y}}), side = side,
                       position = position_nudge(x = position_nudge_vector[3]), alpha = violin_alpha) +

      geom_half_boxplot(data = df,
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[2]),
                        side = side, outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df,
                 aes(x = jit, y = {{y}}), size = point_size,
                 position = position_nudge(x = position_nudge_vector[1]),...) +

      xlab('Group label') + sm_hgrid(borders = borders, legends = legends)
  }

  if (vertical == FALSE) {
    fig <- fig + coord_flip()
  }
  return(fig)

}

globalVariables(c('ggplot', 'geom_point', 'geom_line',
                  'geom_half_boxplot', 'geom_half_violin',
                  'position_nudge', '%>%', 'position',
                  'geom_flat_violin', 'x_axis', 'jit'))
