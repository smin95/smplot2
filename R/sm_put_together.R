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
#' By default, it is set to be 1.1x wider than that of other columns. If the value
#' is larger than 1, then it will be wider than that of other columns. Users are encouraged
#' to adjust this value because different computers can show different looking outputs.
#' @param hRatio
#' This adjusts the ratio of the height of the last row to those of other rows
#' By default, it is set to be 1.1x taller than that of other columns. If the value
#' is larger than 1, then it will be taller than that of other columns. Users are encouraged
#' to adjust this value because different computers can show different looking outputs.
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
#' @param xlabel2
#' 2nd xlabel layer that will determine the label of the combined plot's secondary x-axis.
#' This is created using sm_common_xlabel(). Optional argument.
#' @param ylabel2
#' 2nd ylabel layer that will determine the label of the combined plot's y-axis.
#' This is created using sm_common_ylabel(). Optional argument.
#' @param wRatio2
#' This adjusts the ratio of the width of the last column to those of other columns.
#' By default, if ylabel2 is provided, it is set to be 1.1x wider than that of other columns. If the value
#' is larger than 1, then it will be wider than that of other columns. Users are encouraged
#' to adjust this value because different computers can show different looking outputs.
#' @param hRatio2
#' This adjusts the ratio of the height of the first row to those of other rows
#' By default, if xlabel2 is provided, it is set to be 1.1x taller than that of other columns. If the value
#' is larger than 1, then it will be taller than that of other columns. Users are encouraged
#' to adjust this value because different computers can show different looking outputs.
#' @return
#' Returns a combined figure.
#' @export
#' @importFrom cowplot plot_grid
#'
#' @examples
#' library(smplot2)
#' library(ggplot2)
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#' geom_point(shape = 21, fill = '#0f993d', color = 'white',
#'           size = 3) -> p1
#'
#' ggplot(data = mtcars, mapping = aes(x = drat, y = mpg)) +
#'   geom_point(shape = 21, fill = '#0f993d', color = 'white', size = 3) +
#'   sm_hvgrid() -> p2
#'
#' title <- sm_common_title('My title')
#' xlabel <- sm_common_xlabel('My x-axis')
#' ylabel <- sm_common_ylabel('My y-axis')
#'
#' sm_put_together(list(p1,p2), title=title, xlabel=xlabel,
#'                 ylabel=ylabel, ncol=2,nrow=1)
#'

sm_put_together <- function(all_plots, title, xlabel, ylabel, legend,
                            ncol, nrow, xlabel2, ylabel2, tickRatio = 1.4, panel_scale = 0.9, wRatio=1.1,
                            hRatio = 1.1, hmargin = 0, wmargin = 0, remove_ticks = 'some',
                            wRatio2= 1.1, hRatio2 = 1.1) {

  all_plots <- flatten_ggplot(all_plots)


  if (missing(title)) title <- NULL
  if (missing(xlabel)) xlabel <- NULL
  if (missing(ylabel)) ylabel <- NULL
  if (missing(xlabel2)) xlabel2 <- NULL
  if (missing(ylabel2)) ylabel2 <- NULL


  all_plots <- lapply(1:length(all_plots), function(iPlot) {
    all_plots[[iPlot]] + theme(axis.text.x = element_text(size = rel(tickRatio)),
                               axis.text.y = element_text(size = rel(tickRatio)))
  })


  if (remove_ticks == 'none') {
    all_plots1 <- lapply(1:length(all_plots), function(iPlot) {
      all_plots[[iPlot]] + sm_common_axis('single', hmargin=hmargin, wmargin=wmargin)
    })
    rel_widths <- rep(1,ncol)
    rel_heights <- rep(1,ncol)

  } else if (remove_ticks == 'some') {
    all_plots1 <- sm_plot_clean(all_plots, ncol=ncol,nrow=nrow, hmargin=hmargin, wmargin=wmargin)
    if (is.null(ylabel2)) {
      rel_widths <- c(wRatio, rep(1,ncol-1))
    } else {
      rel_widths <- c(wRatio, rep(1,ncol-2), wRatio2)
    }
    if (is.null(xlabel2)) {
      rel_heights <- c(rep(1,nrow-1), hRatio)
    } else {
      rel_heights <- c(hRatio, rep(1,nrow-2), hRatio2)
    }

  } else if (remove_ticks == 'all') {
    all_plots1 <- lapply(1:length(all_plots), function(iPlot) {
      all_plots[[iPlot]] + sm_common_axis('center', hmargin=hmargin, wmargin=wmargin)
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
                    rel_widths = rel_widths, rel_heights = rel_heights, axis='tblr', align='hv')

  if (!is.null(xlabel)) tgd1 <- plot_grid(tgd1, xlabel, ncol=1, rel_heights = c(1,0.1))
  if (!is.null(ylabel)) tgd1 <- plot_grid(ylabel, tgd1, ncol=2, rel_widths = c(0.1,1))
  if (!is.null(xlabel2)) tgd1 <- plot_grid(xlabel2, tgd1, ncol=1, rel_heights = c(0.1,1))
  if (!is.null(ylabel2)) tgd1 <- plot_grid(tgd1, ylabel2, ncol=2, rel_widths = c(1,0.1))
  if (!is.null(title)) tgd1 <- plot_grid(title, tgd1, ncol=1, rel_heights=c(0.1,1))

  return(tgd1)
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
