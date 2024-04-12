#' Writing a label for each panel of a combined figure
#'
#' @param all_plots
#' all_plots should be a list vector, which should contain all panels
#' that are to be combined into one figure.
#' @param x
#' Location of the label along the x-axis (0 to 1). 0.5 is the middle origin.
#' @param y
#' Location of the label along the y-axis (0 to 1). 0.5 is the middle origin.
#' @param panel_tag
#' A character vector that defines how each panel is enumerated. Options include:
#'  'a', 'A', '1', 'I' or 'i'.'a' is for lowercase letters. 'A' is for uppercase letters. '1'
#'  is for integers. 'I' is for upper case roman numerals. 'i' is for lower case roman numerals.
#'  Each panel will display a unique string based on the set enumeration.
#' @param panel_pretag
#' A character vector that is identical across panels BEFORE the panel_tag.
#' @param panel_posttag
#' A character vector that is identical across panels AFTER the panel_tag.
#' @param text_size
#' Text size of the panel label
#' @param text_color
#' Text color of the panel label
#' @param fontface
#' Fontface of the panel label. Options include "plain", "bold", "italic" and others
#' that are provided by ggplot2.
#' @param ...
#' Additional parameters for adjusting the appearance of the sticker. Same parameters
#' for annotate() from ggplot2.
#'
#' @return
#' It returns a list of plots with panel labels.
#' @export
#' @importFrom utils as.roman
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
#' sm_panel_label(list(p1,p2), x = 0.1, y = 0.9,
#'               panel_tag ='1', panel_pretag = 'S', text_size = 4, text_color = 'black')
#'
sm_panel_label <- function(all_plots, x, y, panel_tag='1', panel_pretag, panel_posttag,
                           text_size = 5.5, text_color = 'black', fontface = 'plain', ...) {

  if (panel_tag == 'A') {
    labelStr = LETTERS
  } else if (panel_tag == 'a') {
    labelStr = letters
  } else if (panel_tag == '1') {
    labelStr = as.character(1:length(all_plots))
  } else if (panel_tag == 'I') {
    labelStr = as.character(as.roman(1:length(all_plots)))
  } else if (panel_tag == 'i') {
    labelStr = tolower(as.character(as.roman(1:length(all_plots))))
  } else {
    labelStr = as.character(1:length(all_plots))
  }

  if (missing(panel_pretag)) {
    labelStr <- labelStr
  } else {
    labelStr <- paste0(panel_pretag,labelStr)
  }

  if (missing(panel_posttag)) {
    labelStr <- labelStr
  } else {
    labelStr <- paste0(labelStr, panel_posttag)
  }

  output <- lapply(1:length(all_plots), function(iPlot) {
    currXlim <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$x.range
    currYlim <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$y.range

    x_coord <- (max(currXlim)-min(currXlim))*x + min(currXlim)
    y_coord <- (max(currYlim)-min(currYlim))*y + min(currYlim)

    all_plots[[iPlot]] + annotate('text',label= labelStr[[iPlot]],
                                  x=x_coord, y=y_coord, size=text_size,
                                  color=text_color, fontface=fontface, ...)
  })
  return(output)
}
