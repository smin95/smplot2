#' Add a point annotation onto the combined plot
#'
#' @param x
#' Location of the point annotation along the x-axis of the combined figure. Default is the middle origin (0.5). Values from 0 to 1.
#' @param y
#' Location of the point annotation along the y-axis of the combined figure. Default is the middle origin (0.5). Values from 0 to 1.
#' @param size
#' Size of the point
#' @param shape
#' Shape of the point. Default is set to circle without border (16).
#' @param color
#' Color of the point. Default is set to black.
#' @param ...
#' Other parameters of point that get passed to geom_point().
#'
#' @return
#' Prints a point in the combined plot.
#' @export
#' @importFrom cowplot draw_plot theme_nothing
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#' geom_point(shape = 21, fill = '#0f993d', color = 'white',
#'           size = 3) -> p1
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#'  geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) +
#'  sm_hvgrid() -> p2
#'
#' combined_fig <- sm_put_together(list(p1,p2), ncol=2,nrow=1)
#' combined_fig + sm_add_point(color='red', size = 10, x = .5, y= .5)
sm_add_point <- function(x, y, size=10, shape=16, color = 'black', ...) {

  ggplot(NULL) +
    theme_nothing() +
    annotate('point', shape = shape, x=.5, y=.5, color=color,
             size = size/.pt, ...) +
    scale_x_continuous(limits=c(0,1), expand=c(0,0)) +
    scale_y_continuous(limits=c(0,1), expand=c(0,0))-> annot

  x <- x - 0.5
  y <- y - 0.5

  output <- draw_plot(annot, x, y)
  return(output)
}
