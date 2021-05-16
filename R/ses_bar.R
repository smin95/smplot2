#' A SES bar plot with individual points (jittered)
#'
#' @description
#' A bar plot superimposed by jittered individual points.
#' ggplot() should contain the summarised data with
#' the mean for each group/condition.
#'
#' @param point_fill
#' Color of the individual jittered points.
#'
#' @param data
#' Entire dataset for plotting the jittered points.
#' Row has to be subject.
#' Column has to be group/variable/condition.
#'
#' @param barplot_fill_color
#' Color of the bar.
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#' @param width
#' Width of the bar.
#' @param point_size
#' Size of the individual jittered points.
#' @param point_border_color
#' Color of the points' border.
#' @param point_shape
#' Shape of the jittered points.
#' Only shapes (21-25) with borders are allowed.
#'
#'
#' @examples
#' library(tidyverse)
#' library(sesplot)
#' set.seed(2) # generate random data
#' day1 = rnorm(20,0,2)
#' day2 = rnorm(20,6,2)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Time <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' DataFrame <- cbind(Subject, Data, Time)
#'
#' # summarise the data with dplyr
#' summary_df <- DataFrame %>% group_by(Time) %>%
#' summarise(mean_value = mean(Value), se = se(Value))
#'
#' # plot the data with ses_bar()
#' ggplot(data = summary_df, aes(x = Time, y = mean_value,
#'                              fill = Time)) +
#' ses_bar(data = DataFrame, aes_x =
#' Time, aes_y = Value) + ses_bar_theme() +
#' geom_linerange(aes(ymin = mean_value-se,
#' ymax = mean_value+se)) # errorbar
#'
#'
ses_bar <- function(point_fill, data=.data, aes_x, aes_y,
                    bar_fill_color = 'gray85',
                           width = 0.4,
                           point_size = 2.2,
                           point_border_color = 'white',
                           point_shape = 21) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (!missing(data)) {
    aes_x <- data[[deparse(substitute(aes_x))]]
    aes_y <- data[[deparse(substitute(aes_y))]]
  } else {
    aes_x <- aes_x
    aes_y <- aes_y
  }

  if (missing(point_fill)) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
          ggplot2::geom_bar(stat="identity",
                           fill = bar_fill_color,
                           width = width),
         ggplot2::geom_point(data = data,
                             aes(x=aes_x,y=aes_y),
                             position = position_jitter(width = .12,
                                                        height = 0,
                                                        seed = 10),
                             shape = point_shape,
                             color = point_border_color,
                             size = point_size))
  } else {

    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
          ggplot2::geom_bar(stat="identity",
                           fill = bar_fill_color,
                               width = width),
         ggplot2::geom_point(data = data,
                             aes(x=aes_x,y=aes_y),
                             position = position_jitter(width = .12,
                                                        height = 0,
                                                        seed = 10),
                             fill = point_fill,
                             shape = point_shape,
                             color = point_border_color,
                             size = point_size))
  }
}

