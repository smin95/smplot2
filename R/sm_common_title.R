
#' Common title for combined subplots
#'
#' @param title
#' The input should be string.
#' @param size
#' Text size of the title.
#' @param x
#' Location of the title along the x-axis. Default is the middle origin (0.5).
#' @param y
#' Location of the legend along the y-axis. Default is the middle origin (0.5).
#' @param fontface
#' The default is to set the text of the title as bold. This can be changed, to
#' either "plain", "bold", "italic", "bold.italic" .
#'
#' @return
#' It prints title on a blank layer of ploting.
#' @export
#'
#' @importFrom cowplot ggdraw draw_label
#'
#' @examples
#' \donttest{
#' library(smplot2)
#' sm_common_title('My title')
#' }
sm_common_title <- function(title = '', size=17, x = 0.5, y = 0.5, fontface='bold') {

  layer <- ggdraw()
  title <- draw_label(title, size = size, x = x, y = y,
                      fontface = fontface)

  return(list(layer+title)[[1]])
}
