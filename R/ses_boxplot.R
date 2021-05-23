#' A boxplot with individual points (jittered)
#'
#' @description
#' A boxplot superimposed by jittered individual points.
#'
#' @param boxplot_fill_color
#' Color of the boxplot.
#'
#' @param boxplot_border_color
#' Color of the boxplot's border.
#'
#' @param points
#' #' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#' @param notch
#' TRUE to notch the shape of the boxplot.
#' FALSE to keep the boxplot's shape.
#' @param width
#' Width of the boxplot.
#' @param point_size
#' Size of the individual jittered points.
#'
#' @param ...
#' Other parameters for geom_point(), such as "fill", "color" and "shape".
#' For more information check out ?geom_point.
#'
#' @import ggplot2 cowplot
#'
#' @export
#'
ses_boxplot <- function(boxplot_fill_color = 'gray95',
                        boxplot_border_color = 'black',
                        points = TRUE, notch = F, width = 0.5,
                        point_size = 2.5,...) {

  if (points == TRUE) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_boxplot(fill = boxplot_fill_color,
                               notch = notch, width = width),
         ggplot2::geom_point(position = ggplot2::position_jitter(width = .12,
                                                        height = 0,
                                                        seed = 10),
                             size = point_size, ...),
         ses_hgrid())
  } else if (points == FALSE) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_boxplot(notch = notch, width = width),
         ses_hgrid())
  }

}
