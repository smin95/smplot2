#' A SES raincloud plot - a combination of jittered points, boxplots
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
#' @param group
#' Group argument for aesthetics. It has to be set for the variable
#' that defines each observation (ex. id, subject, patient, etc).
#' This is used to define the aes() in geom_line().
#' This argument is used to draw a line that pairs each observation
#' across different levels of the x-axis.
#'
#' If this argument is empty, no line will be drawn.
#'
#' @param sep_level
#' A numerical value that controls the level of the separation among
#' the boxplot, violin plot and the points. The value can be 0-3.
#' If it's 0, all of these are clustered together. If it's 3, they are all
#' separated. 1 and 2 are somewhere in the middle.
#'
#' @param jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param point_size
#' Size of the points.
#'
#' @param line_color
#' Color of the line. This argument will be ignored
#' if group argument is missing.
#'
#'
#' @param violin_alpha
#' Transparency of the violin (0 to 1).
#'
#' @param boxplot_alpha
#' Transparency of the boxplot (0 to 1).
#'
#' @param line_alpha
#' Transparency of the lines (0 to 1). This argument will be ignored
#' if group argument is missing.
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
globalVariables(c('ggplot', 'geom_point', 'geom_line',
'geom_half_boxplot', 'geom_half_violin',
'position_nudge', '%>%', 'position',
'geom_flat_violin', 'x_axis', 'jit'))


ses_raincloud <- function(data, x, y, group,
                          sep_level = 3,
                          jitter_width = 0.09,
                          point_size = 3,
                          line_color = 'gray80',
                          violin_alpha = 0.3,
                          boxplot_alpha = 1,
                          line_alpha = 0.6,
                          ...) {



  if (!missing(group)) {
    line_color = line_color
  } else if (missing(group)) {
    line_color <- 'transparent'
    line_alpha <- 0
  }

  df <- data %>% dplyr::mutate(x_axis = as.numeric(factor({{x}}))) %>%
    dplyr::mutate(jit = jitter(x_axis, amount = jitter_width))

  nLevels = length(unique(df$x_axis))

if (nLevels == 2) {
  if (sep_level == 3) {
    position_nudge_vector <- c(-0.3,0.2,-0.35,0.35)
  } else if (sep_level == 2) {
    position_nudge_vector <- c(-0.3,0.2,-0.2,0.2)
  } else if (sep_level == 1) {
    position_nudge_vector <- c(-0.2,0.1,-0.1,0.1)
  } else if (sep_level == 0) {
    position_nudge_vector <- c(-0.1,0,0,0)
  }
} else if (nLevels > 2) {
  if (sep_level == 3) {
    position_nudge_vector <- rep(c(0.2,0.35), each = nLevels)
  } else if (sep_level == 2) {
    position_nudge_vector <- rep(c(0.2,0.2), each = nLevels)
  } else if (sep_level == 1) {
    position_nudge_vector <- rep(c(0.1,0.1), each = nLevels)
  } else if (sep_level == 0) {
    position_nudge_vector <- rep(c(0,0), each = nLevels)
  }
}

  if (nLevels == 2) {
  fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

    geom_line(aes(x = jit, y = {{y}}, group = {{group}}), color = line_color,
              alpha = line_alpha) +

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
    xlab('x-axis label') + ses_minimal(legends = F)

  } else if (nLevels == 3) {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_line(aes(x = jit, y = {{y}}, group = {{group}}), color = line_color,
                alpha = line_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 1),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[4]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 2),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[5]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 3),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

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

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 3),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df %>% dplyr::filter(x_axis == 1),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 2),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 3),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +
      xlab('x-axis label') + ses_minimal(legends = F)
  } else if (nLevels == 4) {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_line(aes(x = jit, y = {{y}}, group = {{group}}), color = line_color,
                alpha = line_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 1),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[4]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 2),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[5]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 3),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 4),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

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

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 3),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 4),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df %>% dplyr::filter(x_axis == 1),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 2),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 3),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 4),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      xlab('x-axis label') + ses_minimal(legends = F)
  } else if (nLevels == 5) {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_line(aes(x = jit, y = {{y}}, group = {{group}}), color = line_color,
                alpha = line_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 1),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[4]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 2),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[5]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 3),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 4),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 5),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

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

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 3),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 4),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 5),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df %>% dplyr::filter(x_axis == 1),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 2),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 3),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 4),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 5),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      xlab('x-axis label') + ses_minimal(legends = F)
  } else if (nLevels == 6) {
    fig <- ggplot(data = df, aes(fill = {{x}}, color = {{x}})) +

      geom_line(aes(x = jit, y = {{y}}, group = {{group}}), color = line_color,
                alpha = line_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 1),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[4]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 2),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[5]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 3),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 4),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 5),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

      geom_half_violin(data = df %>% dplyr::filter(x_axis == 6),
                       aes(x = x_axis, y = {{y}}), side = 'r',
                       position = position_nudge(x = position_nudge_vector[6]), alpha = violin_alpha) +

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

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 3),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 4),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 5),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_half_boxplot(data = df %>% dplyr::filter(x_axis == 6),
                        aes(x = x_axis, y = {{y}}),
                        position = position_nudge(x = position_nudge_vector[3]),
                        side = 'r', outlier.shape = NA, center = TRUE,
                        errorbar.draw = FALSE, width = 0.2, notch = F, alpha = boxplot_alpha, color = 'black') +

      geom_point(data = df %>% dplyr::filter(x_axis == 1),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 2),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 3),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 4),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 5),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      geom_point(data = df %>% dplyr::filter(x_axis == 6),
                 aes(x = jit, y = {{y}}), size = point_size, ...) +

      xlab('x-axis label') + ses_minimal(legends = F)
  } else {
    stop('Sorry, the number of x-axis levels can be from 2 to 6.
         Please use other R packages that draw raincloud plots.')
  }

  return(fig)

}
