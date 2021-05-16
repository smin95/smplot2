#' A boxplot with individual points (jittered)
#'
#' @description
#' A boxplot superimposed by jittered individual points.

#' @param point_fill_color
#' Color of the individual jittered points.
#' @param boxplot_fill_color
#' Color of the boxplot.
#' @param boxplot_border_color
#' Color of the boxplot's border.
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#' @param notch
#' TRUE to notch the shape of the boxplot.
#' FALSE to keep the boxplot's shape.
#' @param width
#' Width of the boxplot.
#' @param point_size
#' Size of the individual jittered points.
#' @param point_shape
#' Shape of the jittered points.
#' Only shapes (21-25) with borders are allowed.
#' @param point_border_color
#' Color of the points' border.
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(sesplot)
#' set.seed(1) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' # a boxplot with the random data, all black points
#' ggplot(data = df, mapping = aes(x = Day, y = Value)) +
#' ses_boxplot(point_fill_color = 'black' )
#'
#' # a boxplot with different colored points
#' ggplot(data = df, mapping = aes(x = Day, y = Value, fill = Day)) +
#' ses_boxplot()
#'
ses_boxplot <- function(point_fill_color, boxplot_fill_color = 'gray95',
                        boxplot_border_color = 'black',
                       points = TRUE, notch = F, width = 0.5,
                       point_size = 2.2,
                       point_border_color = 'white',
                       point_shape = 21) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (missing(point_fill_color)) {

    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                notch = notch, width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               shape = point_shape,
                               color = point_border_color,
                               size = point_size))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(notch = notch, width = width))
    }

  } else {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill_color,
                               shape = point_shape,
                               color = point_border_color,
                               size = point_size))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(notch = notch, width = width))
    }
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill_color,
                               shape = point_shape,
                               color = point_border_color,
                               size = point_size))
    } else if(points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_boxplot(notch = notch, width = width))
    }
    else {
      stop('Such a shape does not exist in geom_point().')
    }
  }
}
