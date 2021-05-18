#' A SES bar plot with individual points (jittered)
#'
#' @description
#' A bar plot superimposed by jittered individual points.
#' ggplot() should contain the summarised data with
#' the mean for each group/condition.
#'
#' @param data
#' Entire dataset for plotting the jittered points.
#' Each row has to be each subject.
#' Column has to be group/variable/condition.
#'
#' @param x
#' Variable that defines x-axis. It should be independent variable.
#' Ideally, it should be identical to the x-axis mapped in aes() from ggplot().
#'
#' @param y
#' Variable that defines y-axis. It should be dependent variable.
#' Ideally, it should be identical to the y-axis mapped in aes() from ggplot().
#' @param bar_fill_color
#' Color of the bar.
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
#' @param ...
#'
#' @params ...
#' Other parameters for geom_point(). For more information
#' check out ?geom_point.
#'
#' @import ggplot2 cowplot
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
#' ses_bar(data = DataFrame, x =
#' Time, y = Value) + ses_bar_theme() +
#' geom_linerange(aes(ymin = mean_value-se,
#' ymax = mean_value+se)) # errorbar
#' @export
#'
ses_bar <- function(data=data, x, y,
                    bar_fill_color = 'gray85',
                    width = 0.4,
                    point_size = 2.5,
                    point_border_color = 'white',
                    point_shape = 21, ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (!missing(data)) {
    x <- data[[deparse(substitute(x))]]
    y <- data[[deparse(substitute(y))]]
  } else {
    x <- x
    y <- y
  }

  list(ggplot2::theme_bw(base_size = 10, base_family = ''),
       ggplot2::geom_bar(stat="identity",
                         fill = bar_fill_color,
                         width = width),
       ggplot2::geom_point(data = data,
                           ggplot2::aes(x=x,y=y),
                           position = ggplot2::position_jitter(width = .12,
                                                      height = 0,
                                                      seed = 10),
                           shape = point_shape,
                           color = point_border_color,
                           size = point_size, ...),
       sesplot::ses_hgrid())
}

