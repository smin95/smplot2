#' violin plot with individual points (jittered)
#'
#'#' @description
#' A violin plot superimposed by jittered individual points.
#'
#' @param point_fill_color
#' Color of the individual jittered points.
#' @param violin_fill_color
#' Color of the violin plot
#' @param violin_border_color
#' Color of the violin's width (Border)
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#' @param point_size
#' Size of the individual jittered points.
#' @param point_shape
#' Shape of the jittered points.
#' Only shapes (21-25) with borders are allowed.
#' @param point_border_color
#'Color of the points' border.
#'
#' @examples
#' library(tidyverse)
#' library(sesplot)
#' # generate random data first
#' set.seed(1)
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Time <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Time)
#'
#' # boxplot with all black points
#' ggplot(data = df, mapping = aes(x = Time, y = Value)) +
#' ses_violin(point_fill_color = 'black')
#'
#' # boxplot with different colors of points across Time
#' ggplot(data = df, mapping = aes(x = Time, y = Value,
#' fill = Time)) + ses_violin()

ses_violin <- function(point_fill_color, violin_fill_color = 'gray90',
                       violin_border_color = 'transparent',
                       points = TRUE,
                       point_size = 2.5,
                       point_shape = 21,
                       point_border_color = 'white', ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (missing(point_fill_color)) {

    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .15,
                                                          height = 0,
                                                          seed = 10),
                               size = point_size,
                               shape = point_shape,
                               color = point_border_color, ...))
    } else if (points == FALSE) {
      ggplot2::geom_violin()
    }

  } else {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               seed = 10),
           fill = point_fill_color,
           shape = point_shape,
           color = point_border_color,
           size = point_size, ...)
    } else if (points == FALSE) {
      ggplot2::geom_violin()
    }
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
            ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill_color,
                               shape = point_shape,
                               color = point_border_color,
                               size = point_size, ...))
    } else if(points == FALSE) {
      ggplot2::geom_violin()
    }
    else {
      stop('Such a shape does not exist in geom_point().')
    }
  }
}
