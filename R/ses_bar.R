#' A SES bar plot with individual points (jittered)
#'
#' @description
#' A bar plot superimposed with jittered individual points.

#' @param point_fill
#' Color of the individual jittered points.
#'
#' @param data
#' Entire dataset for plotting the jittered points.
#' Row has to be subject.
#' Column has to be group/variable/condition.
#'
#' @param barplot_fill_color
#' Color of the bar
#' @param points
#' 'TRUE' if points need to be shown.
#' 'FALSE' if points need to be hidden.
#' @param width
#' Width of the bar
#' @param point_size
#' Size of the individual jittered points.
#' @param point_width_color
#' Color of the points' width.
#' @param point_shape
#' Shape of the jittered points.
#'
#' @return
#' @export
#'
#' @examples
#'
ses_bar <- function(point_fill, data=.data, aes_x, aes_y,
                    bar_fill_color = 'gray85',
                           width = 0.4,
                           point_size = 2.2,
                           point_width_color = 'white',
                           point_shape = 21) {

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
                             color = point_width_color,
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
                             color = point_width_color,
                             size = point_size))
  }
}

