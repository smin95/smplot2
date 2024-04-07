#' Add a point annotation onto the combined plot
#'
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
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
#' @importFrom cowplot ggdraw draw_plot theme_nothing
#'
#' @examples
#' \dontrun{
#' combined_figure2 <- sm_add_point(combined_figure, color='red', x = .5, y= .5)
#' }
sm_add_point <- function(combined_plot, x, y, size, shape=16, color = 'black', ...) {

  ggplot(NULL) +
    theme_nothing() +
    annotate('point', shape = shape, x=.5, y=.5, color=color,
             size = size/.pt, ...) +
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(limits=c(0,1))-> annot

  x <- x - 0.5
  y <- y - 0.5

  output <- ggdraw(combined_plot) + draw_plot(annot, x, y)
  return(output)
}
