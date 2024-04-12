#' Common y-axis label (title) for combined subplots
#'
#' @param label
#' The input should be string.
#' @param size
#' Text size of the label.
#' @param x
#' Location of the label along the x-axis. Default is the middle origin (0.5).
#' @param y
#' Location of the label along the y-axis. Default is the middle origin (0.5).
#' @param fontface
#' The default is to set the text of the title as plain This can be changed, to
#' either "plain", "bold", "italic", "bold.italic" .
#' @param angle
#' Orientation of the y-axis title. Default is 90 degrees.
#'
#' @return
#' It returns a layer with the specified common y-axis label for combined plot.
#' @export
#' @importFrom cowplot ggdraw draw_label
#'
#' @examples
#' library(smplot2)
#' sm_common_ylabel('My y-axis')

sm_common_ylabel <- function(label = '', size=17, x = 0.5, y = 0.5,
                             fontface='plain', angle=90) {
  layer <- ggdraw()
  label <- draw_label(label, size = size, x = x, y = y,
                      fontface = fontface, angle=angle)

  return(list(layer+label)[[1]])
}
