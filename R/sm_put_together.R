#' Combining figures together
#'
#' @param all_plots
#' all_plots should be list, which should contain all panels
#' that are to be combined into one figure.
#' @param title
#' Title layer that will determine the main title of the combined plot.
#' This is created using sm_common_title().
#' @param xlabel
#' xlabel layer that will determine the label of the combined plot's x-axis.
#' This is created using sm_common_xlabel().
#' @param ylabel
#' ylabel layer that will determine the label of the combined plot's y-axis.
#' This is created using sm_common_ylabel().
#' @param legend
#' ggplot() layer that has legend
#' @param ncol
#' Number of columns in the combined plot
#' @param nrow
#' Number of rows in the combined plot
#' @param panel_scale
#' Scale of the panel. Default is set to 0.9 to reduce empty space
#' within and around each panel. The user can set to a value from 0 to 1 to
#' see what happens to the spacing within each panel and between panels.
#' @param wRatio
#' This adjusts the ratio of the width of the first column to those of other columns.
#' By default, it is set to be 1.1x wider than that of other columns. The 0.1 difference
#' is to compensate for the loss of space due to y-ticks in the first column.
#' @param hRatio
#' This adjusts the ratio of the height of the last row to those of other rows
#' By default, it is set to be 1.1x taller than that of other columns. The 0.1 difference
#' is to compensate for the loss of space due to x-ticks in the last row.
#' @param hmargin
#' The amount of height of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#' @param wmargin
#' The amount of width of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#' @param remove_ticks
#' X-axis ticks and y-axis ticks will be removed in inner plots.
#' @return
#' Returns a combined figure.
#' @export
#' @importFrom cowplot plot_grid
#'
#' @examples
#' \dontrun{
#' title <- sm_common_title('My title')
#' xlabel <- sm_common_xlabel('My x-axis ')
#' ylabel <- sm_common_ylabel('My y-axis')
#' plots_tgd <- sm_put_together(indv_plots, title, xlabel,ylabel,
#' ncol=3,nrow=3)
#' }

sm_put_together <- function(all_plots, title, xlabel, ylabel, legend,
                            ncol, nrow, panel_scale = 0.9, wRatio = 1.1,
                            hRatio = 1.1, hmargin = 1, wmargin = 1, remove_ticks = TRUE) {


  if (missing(legend)) {
    all_plots <- all_plots
  } else {
    all_plots[[length(all_plots)+1]] <- legend
  }


  if (remove_ticks == FALSE) {
    all_plots1 <- lapply(1:length(all_plots), function(iPlot) {
      all_plots[[iPlot]] + sm_common_axis('bottomleft', hmargin=hmargin, wmargin=wmargin)
    })
    rel_widths <- rep(1,ncol)
    rel_heights <- rep(1,ncol)
  } else {
    all_plots1 <- sm_plot_clean(all_plots, ncol=ncol,nrow=nrow, hmargin=hmargin, wmargin=wmargin)
    rel_widths <- c(wRatio, rep(1,ncol-1))
    rel_heights <- c(rep(1,nrow-1), hRatio)
  }


  # all_plots should be list
  all_plots2 <- lapply(1:length(all_plots1), function(iPlot) {
    plot_grid(all_plots1[[iPlot]], scale=panel_scale)
  })


  tgd1 <- plot_grid(plotlist = all_plots2, ncol=ncol, nrow=nrow,
                    rel_widths = rel_widths, rel_heights = rel_heights)
  tgd2 <- plot_grid(title, tgd1, ncol=1, rel_heights=c(0.1,1))
  tgd3 <- plot_grid(tgd2, xlabel, ncol=1, rel_heights = c(1,0.1))
  tgd4 <- plot_grid(ylabel, tgd3, ncol=2, rel_widths = c(0.1,1))

  return(tgd4)
}
