
#' Remove xticklabels and yticklabels in selected panels for proper subplotting
#'
#' @param all_plots
#' all_plots should be list, which should contain all panels
#' that are to be combined into one figure.
#' @param ncol
#' Number of columns in the combined plot
#' @param nrow
#' Number of rows in the combined plot
#' @param hmargin
#' The amount of height of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#' @param wmargin
#' The amount of width of blank space between subplots.  It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#'
#' @return
#' Returns a list of plots with new layouts.
#' @export
#'
#' @examples
#' \dontrun{
#' all_plots1 <- sm_plot_clean(all_plots, ncol=3,nrow=2)
#' }
sm_plot_clean <- function(all_plots, ncol, nrow, wmargin=wmargin, hmargin=hmargin) { # returns list
  output <- lapply(1:length(all_plots), function(iPlot) {
    aX = (iPlot-1) %% ncol + 1 # column
    aY = (iPlot-1) %/% ncol + 1 # row

    if (aX == 1 && aY < nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('topleft', wmargin=wmargin, hmargin=hmargin)
    } else if (aX == 1 && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomleft',wmargin=wmargin, hmargin=hmargin)
    } else if (aX > 1 && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomcenter', wmargin=wmargin, hmargin=hmargin)
    } else if (aX == ncol && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomright', wmargin=wmargin, hmargin=hmargin)
    } else {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('topcenter', wmargin=wmargin, hmargin=hmargin)
    }
  })
  return(output)
}
