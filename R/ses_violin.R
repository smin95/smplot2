#' violin plot with individual points (jittered)
#'
#' @description
#' A violin plot superimposed with jittered individual points.
#'
#' @param violin_fill_color
#' Color of the violin.
#' @param violin_width_color
#' Color of the violin's width.
#' @param points
#' Logical argument is required. TRUE if points need to be displayed.
#' FALSE if points need to be not shown.
#' @param point_color
#' Color of the points.
#' @param point_size
#' Size of the points.

#' @param point_width_color
#' Color of the point's width when shapes of the points are 21-25.
#' @param point_fill
#' Color of the points.
#' @param point_shape
#' Shape of the points: 1-25.
#' @return
#' @export
#'
#' @examples
#'
ses_violin <- function(violin_fill_color = 'gray90',
                       violin_width_color = 'transparent',
                       points = TRUE, point_color = 'black',
                       point_size = 1,
                       point_width_color = 'black',
                       point_fill = 'black', point_shape = 21) {

  if (point_shape %in% seq(from=0, to=20)) {
    if (points == TRUE) {
      ggplot2::geom_violin(fill = violin_fill_color,
                           color = violin_width_color) +
        ggplot2::geom_point(position = position_jitter(width = .15,
                                                       height = 0,
                                                       seed = 10),
                            size = point_size,
                            color = point_fill,
                            shape = point_shape)
    } else if (points == FALSE) {
      ggplot2::geom_violin(fill = violin_fill_color,
                           color = violin_width_color)
    }
  } else if (point_shape %in% c(21,22,23,24,25)) {
    if (points == TRUE) {
      ggplot2::geom_violin(fill = violin_fill_color,
                           color = violin_width_color) +
        ggplot2::geom_point(position = position_jitter(width = .15,
                                                       height = 0,
                                                       seed = 10),
                            size = point_size,
                            fill = point_fill,
                            color = point_width_color,
                            shape = point_shape)
    } else if(points == FALSE) {
      ggplot2::geom_violin(fill = violin_fill_color,
                           color = violin_width_color)
    }
  } else {
    stop('Such a shape does not exist in geom_point().')
  }

}
