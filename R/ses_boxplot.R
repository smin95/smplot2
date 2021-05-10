#' A boxplot with individual points (jittered)
#'
#' @description
#' A boxplot plot superimposed with jittered individual points.

#' @param point_fill
#' Color of the individual jittered points.
#' @param boxplot_fill_color
#' Color of the boxplot.
#' @param boxplot_width_color
#' Color of the boxplot's width.
#' @param points
#' 'TRUE' if points need to be shown.
#' 'FALSE' if points need to be hidden.
#' @param notch
#' 'TRUE' to notch the shape of the boxplot.
#' 'FALSE' to keep the boxplot's shape.
#' @param width
#' Width of the boxplot.
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
ses_boxplot <- function(point_fill, boxplot_fill_color = 'gray95',
                        boxplot_width_color = 'black',
                       points = TRUE, notch = F, width = 0.4,
                       point_size = 2,
                       point_width_color = 'white',
                       point_shape = 21) {

  if (missing(point_fill)) {

    if (points == TRUE) {
      list(ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                notch = notch, width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               shape = point_shape,
                               color = point_width_color,
                               size = point_size))
    } else if (points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, width = width)
    }

  } else {
    if (points == TRUE) {
      list(ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill,
                               shape = point_shape,
                               color = point_width_color,
                               size = point_size))
    } else if (points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, width = width)
    }
    if (points == TRUE) {
      list(ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                width = width),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill,
                               shape = point_shape,
                               color = point_width_color,
                               size = point_size))
    } else if(points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, width = width)
    }
    else {
      stop('Such a shape does not exist in geom_point().')
    }
  }
}
