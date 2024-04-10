#' Combining figures together
#'
#' @param all_plots
#' all_plots should be list, which should contain all panels
#' that are to be combined into one figure.
#' @param title
#' Title layer that will determine the main title of the combined plot.
#' This is created using sm_common_title(). Optional argument.
#' @param xlabel
#' xlabel layer that will determine the label of the combined plot's x-axis.
#' This is created using sm_common_xlabel(). Optional argument.
#' @param ylabel
#' ylabel layer that will determine the label of the combined plot's y-axis.
#' This is created using sm_common_ylabel(). Optional argument.
#' @param legend
#' ggplot() layer that has legend. Optional argument.
#' @param ncol
#' Number of columns in the combined plot
#' @param nrow
#' Number of rows in the combined plot
#' @param tickRatio
#' Relative size of the ticks to the default aesthetics of the thematic functions (ex. sm_hgrid()).
#' If there are more rows or columns, please increase the tickRatio (1.4 - 1.8).
#' @param panel_scale
#' Scale of the panel. Default is set to 0.9 to reduce empty space
#' within and around each panel. The user can set to a value from 0 to 1 to
#' see what happens to the spacing within each panel and between panels.
#' @param wRatio
#' This adjusts the ratio of the width of the first column to those of other columns.
#' By default, it is set to be slightly wider than that of other columns. If the value
#' is larger than 1, then it will be wider than that of other columns.
#' @param hRatio
#' This adjusts the ratio of the height of the last row to those of other rows
#' By default, it is set to be 1.11x taller than that of other columns. If the value
#' is larger than 1, then it will be taller than that of other columns.
#' @param hmargin
#' The amount of height of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 0. If its positive, the blank spacing will increase. If its negative, it will get reduced
#' between panels.
#' @param wmargin
#' The amount of width of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 0. If its positive, the blank spacing will increase. If its negative, it will get reduced
#' between panels.
#' @param remove_ticks
#' If set to 'some', x-axis ticks and y-axis ticks will be removed in inner plots.
#' If set to 'all', then all panels' ticks will be removed.
#' If set to 'none', then all panels' ticks will be kept.
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
                            ncol, nrow, tickRatio = 1.4, panel_scale = 0.9, wRatio,
                            hRatio = 1.11, hmargin = 0, wmargin = 0, remove_ticks = 'some') {

  all_plots <- flatten_ggplot(all_plots)

  if (missing(wRatio)) {
    if (ncol == 2) wRatio <- 1.15
    else if (ncol == 3) wRatio <- 1.07
    else if (ncol == 4) wRatio <- 1.07
    else if (ncol == 5) wRatio <- 1.07
    else if (ncol > 5) wRatio <- 1.06
  } else {
    wRatio <- wRatio
  }

  if (missing(title)) title <- NULL
  if (missing(xlabel)) xlabel <- NULL
  if (missing(ylabel)) ylabel <- NULL


  all_plots <- lapply(1:length(all_plots), function(iPlot) {
    all_plots[[iPlot]] + theme(axis.text.x = element_text(size = rel(tickRatio)),
                               axis.text.y = element_text(size = rel(tickRatio)))
  })


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

  if (remove_ticks == 'none') {
    all_plots1 <- lapply(1:length(all_plots), function(iPlot) {
      all_plots[[iPlot]] + sm_common_axis('bottomleft', hmargin=hmargin, wmargin=wmargin)
    })
    rel_widths <- rep(1,ncol)
    rel_heights <- rep(1,ncol)
  } else if (remove_ticks == 'some') {
    all_plots1 <- sm_plot_clean(all_plots, ncol=ncol,nrow=nrow, hmargin=hmargin, wmargin=wmargin)
    rel_widths <- c(wRatio, rep(1,ncol-1))
    rel_heights <- c(rep(1,nrow-1), hRatio)
  } else if (remove_ticks == 'all') {
    all_plots1 <- lapply(1:length(all_plots), function(iPlot) {
      all_plots[[iPlot]] + sm_common_axis('topright', hmargin=hmargin, wmargin=wmargin)
    })
    rel_widths <- rep(1,ncol)
    rel_heights <- rep(1,ncol)
  }

  if (missing(legend)) {
    all_plots1 <- all_plots1
  } else {
    all_plots1[[length(all_plots1)+1]] <- legend
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

flatten_ggplot <- function(lst) {
  plots <- list()
  for (item in lst) {
    if (inherits(item, "gg")) {
      plots <- c(plots, list(item))
    } else if (is.list(item)) {
      plots <- c(plots, flatten_ggplot(item))
    }
  }
  return(plots)
}
