#' Adding a sticker to each panel of a combined figure
#'
#' @param all_plots
#' all_plots should be list, which should contain all panels
#' that are to be combined into one figure.
#' @param x
#' Location of the label along the x-axis (0 to 1). 0.5 is the middle origin.
#' @param y
#' Location of the label along the y-axis (0 to 1). 0.5 is the middle origin.
#' @param shape
#' Shape of the sticker
#' @param colors
#' Colors of the stickers. It should be a vector of multiple colors. If only one is
#' provided, then the stickers on all panels will have the same color.
#' @param sticker_size
#' Size of the sticker.
#' @param ...
#' Additional parameters for adjusting the appearance of the sticker. Same parameters
#' for annotate() from ggplot2.
#'
#' @return
#' It returns a list of plots with panel stickers.
#' @export
#'
#' @examples
#' \dontrun{
#' all_plots_sticker <- sm_panel_sticker(all_plots, x = 0.1, y = 0.9, colors='gray80')
#' }

sm_panel_sticker <- function(all_plots, x, y, shape=17, colors, sticker_size=8,
                             ...) {
  if (missing(colors)) {
    colors <- sm_palette(10)
    if (length(all_plots) > 20) {
      stop('Not enough colors are provided. Number of panels exceeds the number of colors.')
    }
  }

  output <- lapply(1:length(all_plots), function(iPlot) {
    currXlim <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$x.range
    currYlim <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$y.range

    x_coord <- (max(currXlim)-min(currXlim))*x + min(currXlim)
    y_coord <- (max(currYlim)-min(currYlim))*y + min(currYlim)

    all_plots[[iPlot]] + annotate(geom='point', x=x_coord, y=y_coord,
                                  color=colors[iPlot], size=sticker_size, ...)
  })
  return(output)

}
