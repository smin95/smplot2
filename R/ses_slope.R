#' A SES slope chart
#'
#' @description
#' This function generates a slope chart.
#' This is very useful for comparing the effect between two time points.
#'
#' For this function to work properly,
#' ggplot()'s mapping has to be quite specific.
#' 1. Each observation has to be grouped.
#' 2. In aes(), "fill" argument has to be filled in order to generate any filled color.
#'
#'
#' @param line_color
#' Color of the line
#' @param line_size
#' Size of the line
#' @param point_border_color
#' Color of the point's border
#' @param point_size
#' Size of the point
#' @param point_shape
#'
#' @param ...
#' Other parameters of scale_x_discrete. For more information,
#' please type ?scale_x_discrete.
#'
#' @examples
#'
#' set.seed(1) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' ggplot(data = df,
#'       aes(x = Day, y = Value,
#'       group = Subject, fill = Day)) +
#'   ses_slope() +
#'   scale_fill_manual(values = ses_color('blue','blue'))
#'
#'
#' ggplot(data = df,
#'       aes(x = Day, y = Value,
#'       group = Subject, fill = Day)) +
#'   ses_slope() +
#'   scale_fill_manual(values = ses_palette(2))
#'
#'
ses_slope <- function(line_color = 'gray53',
                      line_size = 0.4, point_border_color = 'white',
                      point_size = 3, point_shape = 21, ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  list(geom_line(color = line_color, size = line_size),
       geom_point(size = point_size, shape = point_shape,
                  color = point_border_color),
       scale_x_discrete(position = 'top', expand = c(0.15, .1),
                        drop = FALSE,  ...),
       sesplot::ses_slope_theme(legends = F))
}
