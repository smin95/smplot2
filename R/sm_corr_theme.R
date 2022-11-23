#' SM theme for correlation plots
#'
#' @description
#'
#' This theme has major vertical and horizontal grids. This is useful for plotting correlation.
#' sm_corr_theme() is exactly the same as sm_hvgrid(). For more information please
#' look up `?sm_hvgrid`.
#'
#'
#' @param ...
#' This is the parameter for `sm_slope_all`.
#'
#' @import ggplot2 cowplot
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' ggplot(data = mpg) +
#' geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#' sm_corr_theme()
#' }
#'
sm_corr_theme <- function(...) {
  message('This is equivalent to sm_hvgrid.')
  sm_hvgrid(...)
}
