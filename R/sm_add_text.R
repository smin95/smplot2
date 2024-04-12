#' Add a text annotation onto the combined plot
#'
#' @param combined_plot
#' Combined figure, an output from sm_put_together().
#' @param label
#' Text label in strings.
#' @param x
#' Location of the text annotation along the x-axis of the combined figure. Default is the middle origin (0.5). Values from 0 to 1.
#' @param y
#' Location of the text annotation along the y-axis of the combined figure. Default is the middle origin (0.5). Values from 0 to 1.
#' @param angle
#' Angle of the text. Default is set to 0 (i.e., horizontal orientation).
#' @param color
#' Color of the text. Default is set to 'black'.
#' @param fontface
#' The default is to set the text as plain This can be changed, to
#' either "plain", "bold", "italic", "bold.italic" .
#' @param size
#' Size of the text annotation
#' @param ...
#' Other parameters of the text that will be transferred to the function annotate()
#' from ggplot2.
#' @return
#' Prints a text in the combined plot.
#' @export
#' @importFrom cowplot ggdraw draw_plot theme_nothing
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
#' sm_add_text(combined_fig, label='My label', x = .5, y= .5)
#'
#'
sm_add_text <- function(combined_plot, label, x, y, angle=0, color = 'black',
                        fontface='plain', size=10, ...) {


  ggplot(NULL) +
    theme_nothing() +
    annotate('text', label = label, x=.5, y=.5,
             angle=angle, color=color,
             size = size/.pt,
             fontface=fontface, ...) +
    scale_x_continuous(limits=c(0,1),
                       expand = c(0,0)) +
    scale_y_continuous(limits=c(0,1),
                       expand = c(0,0))-> annot

  x <- x - 0.5
  y <- y - 0.5

  output <- ggdraw(combined_plot) + draw_plot(annot, x, y)
  return(output)
}

