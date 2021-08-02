#' Title
#'
#' @param data
#' @param x
#' @param y
#'
#' @return
#' @import ggplot2 cowplot dplyr gghalves
#' @export
#'
#' @examples
ses_raincloud <- function(data, x, y, group,
                          sep_level = 3,
                          jitter_width = 0.09,
                          point_size = 3,
                          line_color = 'gray80',
                          violin_alpha = 0.3,
                          boxplot_alpha = 0.6,
                          line_alpha = 0.6,
                          ...) {

  if (sep_level == 3) {
    position_nudge_vector <- c(-0.3,0.2,-0.35,0.35)
  } else if (sep_level == 2) {
    position_nudge_vector <- c(-0.3,0.2,-0.2,0.2)
  } else if (sep_level == 1) {
    position_nudge_vector <- c(-0.2,0.1,-0.1,0.1)
  } else if (sep_level == 0) {
    position_nudge_vector <- c(-0.1,0,0,0)
  }

  if (!missing(group)) {
    line_color = line_color
  } else if (missing(group)) {
    line_color <- 'transparent'
    line_alpha <- 0
  }


  df <- data %>% mutate(x_axis = as.numeric(factor({{x}}))) %>%
    mutate(jit = jitter(x_axis, amount = jitter_width))

  df_split <- df %>% group_split(x_axis)
  names(df_split) <- unique(df$x_axis)

  fig_output <- vector('list', length(df_split))

  lapply(seq(df_split), function(i) {

  })

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

  fig1 <- fig + ylab('adfa')
  #return(fig1)

}
