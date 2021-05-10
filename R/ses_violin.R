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
#' @return
#' @export
#'
#' @examples
#'
ses_violin <- function(point_fill, violin_fill_color = 'gray90',
                       violin_width_color = 'transparent',
                       points = TRUE,
                       point_size = 2,
                       point_shape = 21,
                       point_width_color = 'white') {

  if (missing(point_fill)) {

    if (points == TRUE) {
      list(ggplot2::geom_violin(color = violin_width_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               size = point_size,
                               shape = point_shape,
                               color = point_width_color))
    } else if (points == FALSE) {
      ggplot2::geom_violin()
    }

  } else {
    if (points == TRUE) {
      list(ggplot2::geom_violin(color = violin_width_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               seed = 10),
           fill = point_fill,
           shape = point_shape,
           color = point_width_color,
           size = point_size)
    } else if (points == FALSE) {
      ggplot2::geom_violin()
    }
    if (points == TRUE) {
      list(ggplot2::geom_violin(color = violin_width_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               fill = point_fill,
                               shape = point_shape,
                               color = point_width_color,
                               size = point_size))
    } else if(points == FALSE) {
      ggplot2::geom_violin()
    }
    else {
      stop('Such a shape does not exist in geom_point().')
    }
  }
}
