#' violin plot with individual points (jittered)
#'
#'#' @description
#' A violin plot superimposed by jittered individual points.
#' The dot represents the mean, and the sprouting lines indicate +/- 1 standard deviation.
#'
#' @param violin_fill_color
#' Color of the violin plot
#'
#' @param violin_border_color
#' Color of the violin's border.
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#' @param sd_length
#' length of the +/- standard deviation.
#' The default is set to +/- 1 SD.
#' @param point_size
#' Size of the individual jittered points.
#'
#' @param point_alpha
#' Transparency of the jittered points.
#' This argument is ignored when points = FALSE.
#' @param point_shape
#' Shape of the jittered points.
#' Only shapes (21-25) with borders are allowed.
#' @param point_border_color
#' Color of the points' border.
#' @param ...
#' Other parameters for geom_point(), such as "fill".
#' For more information check out ?geom_point.
#' @import ggplot2 cowplot
#'
#'
#' @export

ses_violin <- function(violin_fill_color = 'gray90',
                       violin_border_color = 'transparent',
                       points = TRUE,
                       sd_length = 1,
                       point_size = 2.5,
                       point_alpha = 0.2,
                       point_shape = 21,
                       point_border_color = 'white', ...) {

  if (!(point_shape %in% c(21,22,23,24,25))){
    stop('only shapes (21-25) with borders can be used.')
  }

  if (points == TRUE) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_violin(color = violin_border_color,
                              fill = violin_fill_color),
         ggplot2::geom_point(position = ggplot2::position_jitter(width = .17,
                                                        height = 0,
                                                        seed = 10),
                             size = point_size,
                             shape = point_shape,
                             color = point_border_color,
                             alpha = point_alpha, ...),
         stat_summary(fun.data = mean_sdl, fun.args = list(mult = sd_length),
                      geom = 'pointrange', fatten = point_size*1.2, size = 1.2),
         sesplot::ses_hgrid())
  } else if (points == FALSE) {
    list(ggplot2::theme_bw(base_size = 10, base_family = ''),
         ggplot2::geom_violin(color = violin_border_color,
                              fill = violin_fill_color),
         stat_summary(fun.data = mean_sdl, fun.args = list(mult = sd_length),
                                             geom = 'pointrange'),
         sesplot::ses_hgrid())
  }

}


