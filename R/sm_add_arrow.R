#' Adding an arrow annotation in the combined plot
#'
#' @param x
#' Starting location of the arrow along the x-axis of the combined figure. The middle origin is at 0.5.
#' @param y
#' Starting location of the arrow along the y-axis of the combined figure. The middle origin is at 0.5.
#' @param xend
#' Final location of the arrow along the x-axis of the combined figure. The middle origin is at 0.5.
#' @param yend
#' Final location of the arrow along the y-axis of the combined figure. The middle origin is at 0.5.
#' @param color
#' Color of the line. Default is set to black.
#' @param linewidth
#' Thickness of the arrow Default is set to 0.5.
#' @param type
#' Type of the arrow. Default is set to "open". Other choices include "both' and "closed".
#' @param arrowlength
#' A unit specifying the length of the arrow head (from tip to base).
#'
#' @param ...
#' Other parameters of the arrow line that will be transferred to the function annotate()
#'
#' @return
#' Prints an arrow to the combined plot.
#' @export
#' @importFrom cowplot ggdraw draw_plot theme_nothing
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#' geom_point(shape = 21, fill = '#0f993d', color = 'white',
#'           size = 3) -> p1
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#'  geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) +
#'  sm_hvgrid() -> p2
#'
#' combined_fig <- sm_put_together(list(p1,p2), ncol=2,nrow=1)
#' combined_fig + sm_add_arrow(x = 0.4, y = 0.4, xend = 0.6, yend = 0.6)
sm_add_arrow <- function(x, y, xend, yend, color = 'black',
                         linewidth = 0.5, type = 'open', arrowlength = 1,
                         ...) {

  ggplot(NULL) +
    theme_nothing() +
    annotate('segment', x=x, y=y, xend=xend, yend=yend, color=color,
             linewidth = linewidth, ...,
             arrow = arrow(type = type, length = unit(arrowlength, 'npc'))) +
    scale_x_continuous(limits=c(min(x,xend),max(x,xend)),
                       expand = c(0,0)) +
    scale_y_continuous(limits=c(min(y,yend),max(y,yend)),
                       expand = c(0,0)) -> annot

  width <- abs(x-xend) + linewidth/100 + arrowlength/100
  height <- abs(y-yend) + linewidth/100 + arrowlength/100
  x <- min(x,xend) - linewidth/200 - arrowlength/200
  y <- min(y,yend) - linewidth/200 - arrowlength/200

  output <- draw_plot(annot, x, y, width, height)
  return(output)
}

