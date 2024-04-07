#' Adding a polygon patch on the combined figure
#'
#' Areas captured by the coorinates of x and y will be filled.
#' The lengths of x and y have no limitations as long as their
#' lengths are matched.
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
#' @param x
#' A vector containing x coordinates from each point of the polygon.
#' The first element of x is matched to the firt element of y. Values from 0 to 1,
#' where 0.5 is the origin.
#' @param y
#' A vector containing y coordinates from each point of the polygon.
#' The first element of x is matched to the firt element of y. Values from 0 to 1,
#' where 0.5 is the origin.
#' @param fill
#' Fill color for the rectangle to be used
#' @param color
#' Border line of the rectangle
#' @param linewidth
#' Linewidth of border of the rectangle
#' @param ...
#' Other parameters to be used in annotate() from ggplot2.
#'
#' @return
#' Prints a patch of rectangle onto a combined plot.
#' @export
#' @importFrom cowplot ggdraw draw_plot theme_nothing
#'
#' @examples
#' \dontrun{
#' combined_figure2 <- sm_add_polygon(combined_figure, x = c(.2,.8,.8), y = c(.2,.8,.2))
#' }
sm_add_polygon <- function(combined_plot, x, y, fill = 'gray80',
                           color = 'black', linewidth = 0.5, ...) {

  xmin <- min(x); xmax <- max(x)
  ymin <- min(y); ymax <- max(y)

  ggplot(NULL) +
    theme_nothing() +
    annotate('polygon', x, y, fill=fill,color=color,
             linewidth=linewidth, ...) +
    scale_x_continuous(limits=c(xmin, xmax)) +
    scale_y_continuous(limits=c(ymin, ymax)) -> annot

  width <- abs(xmin-xmax)
  height <- abs(ymin-ymax)
  x <- min(xmin,xmax)
  y <- min(ymin,ymax)

  output <- ggdraw(combined_plot) + draw_plot(annot, x, y, width, height)
  return(output)
}
