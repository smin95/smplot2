#' violin plot with individual points (jittered)
#'
#' @description
#' A violin plot superimposed by jittered individual points.
#' The dot represents the mean, and the sprouting lines indicate +/- 1 standard deviation.
#'
#' @param violin_fill_color
#' Color of the violin plot
#'
#' @param violin_border_color
#' Color of the violin's border.
#'
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param points
#' TRUE if points need to be shown.
#' FALSE if points need to be hidden.
#'
#' @param sd_length
#' Length of the +/- standard deviation / standard error.
#' The default is set to +/- 1 SD. If sd_length is set to 2, it will be +/1 2 SD.
#' This input argument is ignored when errorbar_type = 'ci'.
#'
#' @param point_size
#' Size of the individual jittered points.
#'
#' @param point_alpha
#' Transparency of the jittered points.
#' This argument is ignored when points = FALSE.
#'
#' @param stroke_width
#' The width of the lines that show standard deviation.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @param ...
#' Other parameters for geom_point(), such as "fill".
#' For more information check out ?geom_point.
#'
#' @import ggplot2 cowplot
#'
#' @importFrom stats sd
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' # use the dataframe to generate a violin plot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_violin() +
#' scale_color_manual(values = sm_color('blue','orange'))
#' }

sm_violin <- function(violin_fill_color = 'gray90',
                      violin_border_color = 'transparent',
                      errorbar_type = 'sd',
                      points = TRUE,
                      sd_length = 1,
                      point_size = 2.5,
                      point_alpha = 0.25,
                      stroke_width = 1.2,
                      legends = FALSE,
                      borders = TRUE,
                      ...) {

  if (errorbar_type == 'sd') {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = .17,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_sdl, fun.args = list(mult = sd_length),
                        geom = 'pointrange', fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           stat_summary(fun.data = mean_sdl, fun.args = list(mult = sd_length),
                        geom = 'pointrange',
                        fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    }
  } else if (errorbar_type == 'se') {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = .17,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_se, fun.args = list(mult = sd_length),
                        geom = 'pointrange', fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           stat_summary(fun.data = mean_se, fun.args = list(mult = sd_length),
                        geom = 'pointrange',
                        fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    }
  } else if (errorbar_type == 'ci') {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = .17,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size,
                               alpha = point_alpha, ...),
           stat_summary(fun.data = mean_cl_boot, fun.args=list(conf.int=.95),
                        geom = 'pointrange',
                        fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_violin(color = violin_border_color,
                                fill = violin_fill_color),
           stat_summary(fun.data = mean_cl_boot, fun.args=list(conf.int=.95),
                        geom = 'pointrange', fatten = point_size*1.2, size = stroke_width),
           sm_hgrid(borders = borders, legends = legends))
    }
  } else {
    stop('Wrong input argument for errorbar_type. Please write either "se", "sd" or "ci"')
  }

}

