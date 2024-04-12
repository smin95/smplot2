#' Adding a line annotation in the combined plot
#'
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
#' @param x
#' Starting location of the line along the x-axis of the combined figure. The middle origin is at 0.5. Values from 0 to 1.
#' @param y
#' Starting location of the line along the y-axis of the combined figure. The middle origin is at 0.5. Values from 0 to 1.
#' @param xend
#' Final location of the line along the x-axis of the combined figure. The middle origin is at 0.5. Values from 0 to 1.
#' @param yend
#' Final location of the line along the y-axis of the combined figure. The middle origin is at 0.5. Values from 0 to 1.
#' @param color
#' Color of the line. Default is set to black.
#' @param linewidth
#' Thickness of the line. Default is set to 0.5.
#' @param ...
#' Other parameters of the line that will be transferred to the function annotate()
#' from ggplot2.
#' @return
#' Prints a line to the combined plot.
#' @export
#' @importFrom cowplot ggdraw draw_plot theme_nothing
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
#' sm_add_line(combined_fig, x = 0.4, y = 0.4, xend = 0.6, yend = 0.6)
#'
sm_add_line <- function(combined_plot, x, y, xend, yend, color = 'black',
                        linewidth = 0.5, ...) {

  ggplot(NULL) +
    theme_nothing() +
    annotate('segment', x=x, y=y, xend=xend, yend=yend, color=color,
             linewidth=linewidth, ...) +
    scale_x_continuous(limits=c(min(x,xend),max(x,xend)),
                       expand = c(0,0)) +
    scale_y_continuous(limits=c(min(y,yend),max(y,yend)),
                       expand = c(0,0)) -> annot

  width <- abs(x-xend) + linewidth/100
  height <- abs(y-yend) + linewidth/100
  x <- min(x,xend) - linewidth/200
  y <- min(y,yend) - linewidth/200

  output <- ggdraw(combined_plot) + draw_plot(annot, x, y, width, height)
  return(output)
}
