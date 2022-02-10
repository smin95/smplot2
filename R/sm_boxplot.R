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
#' @param outlier_label
#' Set to FALSE as default. If TRUE, another point that denotes the outlier
#' (as shown in the jittered points) will appear. In the TRUE condition,
#' two points that refer to the same individual point will appear
#' (one jittered, one outlier label).
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#'
#' @param ...
#' Other parameters for geom_point(), such as "fill", "color" and "shape".
#' For more information check out ?geom_point.
#'
#' @import ggplot2 cowplot
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
#' df <- cbind(Subject, Data, Day) # final dataframe
#'
#' #use the dataframe to generate a boxplot
#'
#' ggplot(data = df, mapping = aes(x = Day, y = Value, fill = Day)) +
#' sm_boxplot(shape = 21, color = 'white') +
#' scale_fill_manual(values = sm_color('blue','orange'))
#' }
#'
sm_boxplot <- function(boxplot_fill_color = 'gray95',
                        boxplot_border_color = 'black',
                        points = TRUE, notch = F, width = 0.5,
                        point_size = 2.5, outlier_label = FALSE,
                        borders = TRUE, legends = FALSE, ...) {

  if (outlier_label == FALSE) {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                 color = boxplot_border_color,
                                 notch = notch, width = width,
                                 outlier.shape = NA),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = .12,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size, ...),
           sm_hgrid(borders = borders, legends = legends))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_boxplot(color = boxplot_border_color,
                                 notch = notch, width = width,
                                 outlier.shape = NA),
           sm_hgrid(borders = borders, legends = legends))
    }
  } else if (outlier_label == TRUE) {
    if (points == TRUE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_boxplot(fill = boxplot_fill_color,
                                 color = boxplot_border_color,
                                 notch = notch, width = width),
           ggplot2::geom_point(position = ggplot2::position_jitter(width = .12,
                                                                   height = 0,
                                                                   seed = 10),
                               size = point_size, ...),
           sm_hgrid(borders = borders, legends = legends))
    } else if (points == FALSE) {
      list(ggplot2::theme_bw(base_size = 10, base_family = ''),
           ggplot2::geom_boxplot(color = boxplot_border_color,
                                 notch = notch, width = width),
           sm_hgrid(borders = borders, legends = legends))
    }
  }
}
