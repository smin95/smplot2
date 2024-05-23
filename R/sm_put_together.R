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
#' By default, it chooses the optimal tickRatio based on the given plot. But this
#' can be overwritten if the input is supplied (ex. try 1.4 to begin with). FOr example, 1.4x means that
#' it is 1.4x larger than the tick size of a single given plot.
#' @param panel_scale
#' Scale of the panel. Default is set to 0.9 to reduce empty space
#' within and around each panel. The user can set to a value from 0 to 1 to
#' see what happens to the spacing within each panel and between panels.
#' @param wRatio
#' This adjusts the ratio of the width of the first column to those of other columns.
#' By default, it chooses an optimal wRatio based on the given plot. However, this can be overwritten if the input is supplied.
#' If the value is larger than 1, then it will be wider than that of other columns. Users are encouraged
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
#' By default, it chooses an optimal wRatio2 based on the given plot. However, this can be overwritten if the input is supplied.
#' If the value is larger than 1, then it will be wider than that of other columns. Users are encouraged
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
#' @importFrom ggplot2 ggplot_build
#' @importFrom utils head
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
                            ncol, nrow, xlabel2, ylabel2, tickRatio, panel_scale = 0.9, wRatio,
                            hRatio = 1.1, hmargin = 0, wmargin = 0, remove_ticks = 'some',
                            wRatio2, hRatio2 = 1.1) {

  all_plots <- flatten_ggplot(all_plots)

  if (missing(title)) title <- NULL
  if (missing(xlabel)) xlabel <- NULL
  if (missing(ylabel)) ylabel <- NULL
  if (missing(xlabel2)) xlabel2 <- NULL
  if (missing(ylabel2)) ylabel2 <- NULL

  ncr <- max(ncol,nrow)
  if (missing(tickRatio)) {
    if (ncr > 1) {
      tickRatio = 1+ncr/12
    } else {
      tickRatio = 1
    }
  } else {
    tickRatio = tickRatio
  }

  y_axis1_label <-  ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y$get_labels()
  y_axis1_label <- y_axis1_label[!is.na(y_axis1_label)]

  y_axis2_label <-  ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y.sec$get_labels()
  y_axis2_label <- y_axis2_label[!is.na(y_axis2_label)]

  min_y_len <- min(length(y_axis1_label), length(y_axis2_label))

  if (all(head(y_axis1_label,min_y_len) == head(y_axis2_label,min_y_len))){
    double_yaxis = FALSE
  } else {
    double_yaxis = TRUE
  }

  x_axis1_label <- ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$x$get_labels()
  x_axis1_label <- x_axis1_label[!is.na(x_axis1_label)]

  x_axis2_label <- ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$x.sec$get_labels()
  x_axis2_label <- x_axis2_label[!is.na(x_axis2_label)]

  min_x_len <- min(length(x_axis1_label), length(x_axis2_label))

  if (all(head(x_axis1_label,min_x_len) == head(x_axis2_label,min_x_len))){
    double_xaxis = FALSE
  } else {
    double_xaxis = TRUE
  }

  nChar_y1 <- max(nchar(ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y$get_labels()))
  nChar_y1a <- max(nchar(gsub('[[:punct:]]','',ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y$get_labels()))) # pure number length
  nPunc_y1 <- nChar_y1 - nChar_y1a
  nChar_y2 <- max(nchar(ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y.sec$get_labels()))
  nChar_y2a <- max(nchar(gsub('[[:punct:]]','',ggplot_build(all_plots[[1]])$layout$panel_params[[1]]$y.sec$get_labels()))) # pure number length
  nPunc_y2 <- nChar_y2 - nChar_y2a

  if (missing(wRatio)) {
    wRatio = 1 + (0.04 + 0.005*ncol + (0.9-ifelse(panel_scale > 0.9, 0.9, panel_scale))/10)*(nChar_y1a + 0.75*nPunc_y1)
  }

  if (missing(wRatio2)) {
    wRatio2 = 1 + (0.04 + 0.005*ncol + (0.9-ifelse(panel_scale > 0.9, 0.9, panel_scale))/10)*(nChar_y2a + 0.75*nPunc_y2)
  }

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
    if (double_yaxis == FALSE) {
      rel_widths <- c(wRatio, rep(1,ncol-1))
    } else {
      if (ncol == 2) {
        rel_widths <- rep(1,ncol)
      } else {
        rel_widths <- c(wRatio, rep(1,ncol-2), wRatio2)
      }
    }
    if (double_xaxis == FALSE) {
      rel_heights <- c(rep(1,nrow-1), hRatio)
    } else {
      if (nrow == 2) {
        rel_heights <- rep(1,ncol)
      } else {
        rel_heights <- c(hRatio2, rep(1,nrow-2), hRatio)
      }

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

  ## x-axis
  if (!is.null(xlabel)) tgd1 <- plot_grid(tgd1, xlabel, ncol=1, rel_heights = c(1,0.1))
  if (!is.null(xlabel2)) tgd1 <- plot_grid(xlabel2, tgd1, ncol=1, rel_heights = c(0.1,1))
  ## title
  if (!is.null(title)) tgd1 <- plot_grid(title, tgd1, ncol=1, rel_heights=c(0.1,1))
  ## y-axis
  if (!is.null(ylabel)) tgd1 <- plot_grid(ylabel, tgd1, ncol=2, rel_widths = c(0.1,1))
  if (!is.null(ylabel2)) tgd1 <- plot_grid(tgd1, ylabel2, ncol=2, rel_widths = c(1,0.1))

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
