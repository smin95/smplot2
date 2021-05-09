#' violin plot with individual points (jittered)
#'
#' @description
#' A violin plot superimposed with jittered individual points.
#'
#' @param boxplot_fill_color
#' Color of the boxplot.
#' @param boxplot_width_color
#' Color of the boxplot's width.
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
ses_boxplot <- function(point_fill, boxplot_fill_color = 'gray95',
                        boxplot_width_color = 'black',
                       points = TRUE, notch = F, notchwidth = 0.4,
                       point_size = 1,
                       point_width_color = 'white') {

  if (missing(point_fill)) {

    if (points == TRUE) {
      list(ggplot2::geom_boxplot(color = boxplot_width_color,
                                fill = boxplot_fill_color,
                                notch = notch, notchwidth = notchwidth),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10)))
    } else if (points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, notchwidth = notchwidth)
    }

  } else {
    if (points == TRUE) {
      list(ggplot2::geom_boxplot(color = boxplot_width_color,
                                fill = boxplot_fill_color,
                                notchwidth = notchwidth),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               color = point_fill))
    } else if (points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, notchwidth = notchwidth)
    }
    if (points == TRUE) {
      list(ggplot2::geom_boxplot(color = boxplot_width_color,
                                fill = boxplot_fill_color,
                                notchwidth = notchwidth),
           ggplot2::geom_point(position = position_jitter(width = .12,
                                                          height = 0,
                                                          seed = 10),
                               color = point_fill))
    } else if(points == FALSE) {
      ggplot2::geom_boxplot(notch = notch, notchwidth = notchwidth)
    }
    else {
      stop('Such a shape does not exist in geom_point().')
    }
  }
}
