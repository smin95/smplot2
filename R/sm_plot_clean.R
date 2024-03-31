
#' Remove xticklabels and yticklabels in selected panels for proper subplotting
#'
#' @param all_plots
#' all_plots should be list, which should contain all panels
#' that are to be combined into one figure.
#' @param ncol
#' Number of columns in the combined plot
#' @param nrow
#' Number of rows in the combined plot
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' all_plots1 <- sm_plot_clean(all_plots, ncol=3,nrow=2)
#' }
sm_plot_clean <- function(all_plots, ncol, nrow) { # returns list
  output <- lapply(1:length(all_plots), function(iPlot) {
    aX = (iPlot-1) %% ncol + 1 # column
    aY = (iPlot-1) %/% ncol + 1 # row

    if (aX == 1 && aY < nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('topleft')
    } else if (aX == 1 && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomleft')
    } else if (aX > 1 && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomcenter')
    } else if (aX == ncol && aY == nrow) {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('bottomright')
    } else {
      all_plots[[iPlot]] <- all_plots[[iPlot]] + sm_common_axis('topcenter')
    }
  })
  return(output)
}
