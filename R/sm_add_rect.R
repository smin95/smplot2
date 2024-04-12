#' Adding a rectangle patch on the combined figure
#'
#' Areas captured by xmin, ymin, xmax and ymax will become a patch
#' once the function gets called.
#'
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
#' @param xmin
#' Starting x value of the rectangle patch. Values from 0 to 1.
#' @param ymin
#' Starting y value of the rectangle patch. Values from 0 to 1.
#' @param xmax
#' Ending x value of the rectangle patch. Values from 0 to 1.
#' @param ymax
#' Ending x value of the rectangle patch. Values from 0 to 1.
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
#'
#' @examples
#' library(smplot2)
#' library(ggplot2)
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#'  geom_point(shape = 21, fill = '#0f993d', color = 'white',
#'             size = 3) -> p1
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#'  geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) +
#'  sm_hvgrid() -> p2
#'
#' combined_fig <- sm_put_together(list(p1,p2), ncol=2,nrow=1)
#' sm_add_rect(combined_fig, xmin = .5, ymin = .5, xmax =.6, ymax =.6)
#'
sm_add_rect <- function(combined_plot, xmin, ymin, xmax, ymax, fill = 'gray80',
                        color = 'black', linewidth = 0.5, ...) {

  ggplot(NULL) +
    theme_nothing() +
    annotate('rect', xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax, fill=fill,color=color,
             linewidth=linewidth, ...) +
    scale_x_continuous(limits=c(min(xmin,xmax),max(xmin,xmax)),
                       expand = c(0,0)) +
    scale_y_continuous(limits=c(min(ymin,ymax),max(ymin,ymax)),
                       expand = c(0,0)) -> annot

  width <- abs(xmin-xmax)
  height <- abs(ymin-ymax)
  x <- min(xmin,xmax)
  y <- min(ymin,ymax)

  output <- ggdraw(combined_plot) + draw_plot(annot, x, y, width, height)
  return(output)
}

