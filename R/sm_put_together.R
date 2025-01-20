#' Combine Multiple Plots into a Single Composite Figure
#'
#' `sm_put_together` combines multiple ggplot objects into a single composite figure.
#' The function optimizes the layout by considering the presence of tick labels and
#' axis labels in the input plots, ensuring a clean and well-aligned output.
#'
#' Users can supply custom titles and axis labels either as ggplot layers created
#' with `sm_common_xlabel()`, `sm_common_ylabel()`, and `sm_common_title()` or as
#' character strings directly. The function attempts to adjust the size and placement
#' of titles and labels dynamically based on the input plots.
#'
#' While the `all_plots` argument is required, all other arguments are optional,
#' and defaults are provided for most parameters.
#'
#' @param all_plots A list of ggplot objects to be combined. Each plot should
#'   ideally include tick labels on both x and y axes to allow the function to
#'   optimize the layout effectively.
#' @param title A ggplot layer or character string specifying the title of the
#'   combined figure. If supplied as a string, the function dynamically adjusts
#'   its size and placement. Optional.
#' @param xlabel A ggplot layer or character string specifying the label for the
#'   x-axis of the combined figure. Optional.
#' @param ylabel A ggplot layer or character string specifying the label for the
#'   y-axis of the combined figure. Optional.
#' @param legend A ggplot legend layer to be added to the combined figure. Optional.
#' @param ncol Number of columns in the combined figure grid. If not supplied,
#'   the function determines this based on the number of plots.
#' @param nrow Number of rows in the combined figure grid. If not supplied,
#'   the function determines this based on the number of plots.
#' @param tickRatio A scaling factor for tick label size. By default, this is
#'   adjusted dynamically based on the number of rows and columns in the grid.
#'   Larger values increase tick label size. Optional.
#' @param panel_scale A numeric value between 0 and 1 that scales the individual
#'   panels to reduce empty space within and around each panel. Default is `0.9`.
#' @param wRatio A scaling factor for the width of the first column relative to
#'   other columns. By default, this is computed based on the input plots. Users
#'   can override it with a numeric value. Optional.
#' @param hRatio A scaling factor for the height of the last row relative to other
#'   rows. By default, this is computed based on the input plots. Users can override
#'   it with a numeric value. Optional.
#' @param hmargin Vertical margin between subplots. Positive values increase the
#'   spacing, while negative values reduce it. Default is `0`.
#' @param wmargin Horizontal margin between subplots. Positive values increase the
#'   spacing, while negative values reduce it. Default is `0`.
#' @param remove_ticks Specifies whether to remove ticks from the inner panels:
#'   `"some"` (default) removes ticks only from inner plots, `"all"` removes ticks
#'   from all plots, and `"none"` keeps all ticks.
#' @param xlabel2 A secondary x-axis label layer or character string. Optional.
#' @param ylabel2 A secondary y-axis label layer or character string. Optional.
#' @param wRatio2 A scaling factor for the width of the last column relative to
#'   other columns. By default, this is computed based on the input plots. Optional.
#' @param hRatio2 A scaling factor for the height of the first row relative to
#'   other rows. By default, this is computed based on the input plots. Optional.
#' @param labelRatio A scaling factor for the text size of titles, axis labels,
#'   and secondary labels when provided as character strings. Default is `1`.
#'   Larger values increase text size proportionally.
#' @return A composite ggplot object combining the input plots into a grid layout.
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
#' sm_put_together(list(p1,p2), title='My title', xlabel='My x-axis',
#'                 ylabel='My y-axis', labelRatio = 1.1, ncol=2,nrow=1)
#'

sm_put_together <- function(all_plots, title = NULL, xlabel = NULL, ylabel = NULL, legend = NULL,
                            ncol = NULL, nrow = NULL, xlabel2 = NULL, ylabel2 = NULL, tickRatio = NULL, panel_scale = 0.9, wRatio = NULL,
                            hRatio = NULL, hmargin = 0, wmargin = 0, remove_ticks = 'some',
                            wRatio2 = NULL, hRatio2 = NULL, labelRatio = 1) {


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

  count_lines <- function(text) {
    if (!is.null(text)){
      res <- length(strsplit(text, "\n")[[1]])
    } else {
      res <- 0
    }
    return(res)

  }

  if (!is.list(all_plots)) stop("`all_plots` must be a list of ggplot objects.")
  all_plots <- flatten_ggplot(all_plots)

  if (is.null(ncol) || is.null(nrow)) {
    # Infer grid dimensions if not provided
    nrow <- floor(sqrt(length(all_plots)))
    ncol <- ceiling(length(all_plots) / nrow)
  }

  ncr <- max(ncol,nrow)
  if (is.null(tickRatio)) {
    if (ncr > 1) {
      tickRatio = 1+ncr/12
    } else {
      tickRatio = 1
    }
  } else {
    tickRatio = tickRatio
  }

  double_yaxis_output <- unlist(lapply(1:length(all_plots), function(iPlot) {

    y_axis1_label <-  ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$y$get_labels()
    y_axis1_label <- y_axis1_label[!is.na(y_axis1_label)]

    y_axis2_label <-  ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$y.sec$get_labels()
    y_axis2_label <- y_axis2_label[!is.na(y_axis2_label)]

    min_y_len <- min(length(y_axis1_label), length(y_axis2_label))

    !all(head(y_axis1_label,min_y_len) == head(y_axis2_label,min_y_len))
  }))

  double_xaxis_output <- unlist(lapply(1:length(all_plots), function(iPlot) {
    x_axis1_label <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$x$get_labels()
    x_axis1_label <- x_axis1_label[!is.na(x_axis1_label)]

    x_axis2_label <- ggplot_build(all_plots[[iPlot]])$layout$panel_params[[1]]$x.sec$get_labels()
    x_axis2_label <- x_axis2_label[!is.na(x_axis2_label)]

    min_x_len <- min(length(x_axis1_label), length(x_axis2_label))

    !all(head(x_axis1_label,min_x_len) == head(x_axis2_label,min_x_len))
  }))

  double_yaxis = any(double_yaxis_output)
  double_xaxis = any(double_xaxis_output)
  y_left_only <- all(!double_yaxis_output)
  x_bottom_only <- all(!double_xaxis_output)

  if (double_yaxis) double_yaxis_which = which(double_yaxis_output)[[1]]
  if (double_xaxis) double_xaxis_which = which(double_xaxis_output)[[1]]
  if (y_left_only) left_yaxis_which = which(!double_yaxis_output)[[1]]
  if (x_bottom_only) bottom_xaxis_which = which(!double_xaxis_output[[1]])

  if (double_yaxis) {
    y1_label <-  as.character(ggplot_build(all_plots[[double_yaxis_which]])$layout$panel_params[[1]]$y$get_labels())
    y1_label <- y1_label[!is.na(y1_label)]

    nChar_y1 <- max(nchar(y1_label))
    nChar_y1a <- max(nchar(gsub('[[:punct:]]','', y1_label))) # pure number length
    nPunc_y1 <- nChar_y1 - nChar_y1a

    y2_label <-  as.character(ggplot_build(all_plots[[double_yaxis_which]])$layout$panel_params[[1]]$y.sec$get_labels())
    y2_label <- y2_label[!is.na(y2_label)]

    nChar_y2 <- max(nchar(y2_label))
    nChar_y2a <- max(nchar(gsub('[[:punct:]]','', y2_label))) # pure number length
    nPunc_y2 <- nChar_y2 - nChar_y2a
  } else if (y_left_only) {

    y1_label <-  as.character(ggplot_build(all_plots[[left_yaxis_which]])$layout$panel_params[[1]]$y$get_labels())
    y1_label <- y1_label[!is.na(y1_label)]

    nChar_y1 <- max(nchar(y1_label))
    nChar_y1a <- max(nchar(gsub('[[:punct:]]','', y1_label))) # pure number length
    nPunc_y1 <- nChar_y1 - nChar_y1a

    nChar_y2 <- 0
    nChar_y2a <- 0
    nPunc_y2 <- 0
  }

  if (double_xaxis) {

    x1_label <- as.character(ggplot_build(all_plots[[double_xaxis_which]])$layout$panel_params[[1]]$x$get_labels())
    x1_label <- x1_label[!is.na(x1_label)]
    maxLines_x1 <- max(unlist(lapply(x1_label, count_lines)))

    x2_label <- as.character(ggplot_build(all_plots[[double_xaxis_which]])$layout$panel_params[[1]]$x.sec$get_labels())
    x2_label <- x2_label[!is.na(x2_label)]
    maxLines_x2 <- max(unlist(lapply(x2_label, count_lines)))
  } else if (x_bottom_only) {

    x1_label <- as.character(ggplot_build(all_plots[[bottom_xaxis_which]])$layout$panel_params[[1]]$x$get_labels())
    x1_label <- x1_label[!is.na(x1_label)]
    maxLines_x1 <- max(unlist(lapply(x1_label, count_lines)))

    maxLines_x2 <- 0
  }

  if (is.null(wRatio)) wRatio = 1 + (0.04 + 0.005*ncol + (0.9-ifelse(panel_scale > 0.9, 0.9, panel_scale))/10)*(nChar_y1a + 0.85*nPunc_y1)
  if (is.null(wRatio2)) wRatio2 = 1 + (0.04 + 0.005*ncol + (0.9-ifelse(panel_scale > 0.9, 0.9, panel_scale))/10)*(nChar_y2a + 0.85*nPunc_y2)

  if (is.null(hRatio))  hRatio = 1 + 0.1*maxLines_x1 + 0.04*ifelse(maxLines_x1 > 2, 1,0)
  if (is.null(hRatio2))  hRatio2 = 1 + 0.1*maxLines_x2 + 0.04*ifelse(maxLines_x2 > 2, 1,0)

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
      if (ncol < 2.1) {
        rel_widths <- rep(1,ncol)
      } else {
        rel_widths <- c(wRatio, rep(1,ncol-2), wRatio2)
      }
    }
    if (double_xaxis == FALSE) {
      rel_heights <- c(rep(1,nrow-1), hRatio)
    } else {
      if (nrow < 2.1) {
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

  if (is.null(legend)) {
    all_plots1 <- all_plots1
  } else {
    all_plots1[[length(all_plots1)+1]] <- legend
  }

  # all_plots should be list
  all_plots2 <- lapply(1:length(all_plots1), function(iPlot) {
    cowplot::plot_grid(all_plots1[[iPlot]], scale=panel_scale)
  })


  tgd1 <- cowplot::plot_grid(plotlist = all_plots2, ncol=ncol, nrow=nrow,
                             rel_widths = rel_widths, rel_heights = rel_heights, axis='tblr', align='hv')

  # add labels

  if (inherits(ylabel, 'character'))  {
    ylabelStr <- ylabel
    ylabel <- sm_common_ylabel('')
  } else ylabelStr <- NULL
  if (inherits(xlabel, 'character'))  {
    xlabelStr <- xlabel
    xlabel <- sm_common_xlabel('')
  } else xlabelStr <- NULL
  if (inherits(ylabel2, 'character')) {
    ylabel2Str <- ylabel2
    ylabel2 <- sm_common_ylabel('')
  } else ylabel2Str <- NULL
  if (inherits(xlabel2, 'character')) {
    xlabel2Str <- xlabel2
    xlabel2 <- sm_common_xlabel('')
  } else xlabel2Str <- NULL
  if (inherits(title, 'character'))  {
    titleStr <- title
    title <- sm_common_title('')
  } else titleStr <- NULL

  xDelta <- 1/ncol * 0.05
  yDelta <- 1/nrow * 0.05

  xloc <- 0.5 + ifelse(!is.null(ylabel),xDelta,0) - ifelse(!is.null(ylabel2),xDelta,0)

  yloc <- 0.5 + ifelse(!is.null(xlabel),yDelta,0) - ifelse(!is.null(xlabel2),yDelta,0)

  ## x-axis
  if (!is.null(xlabel)) {
    tgd1 <- cowplot::plot_grid(tgd1, xlabel, ncol=1, rel_heights = c(1,0.1))
    tgd1 <- tgd1 + sm_add_text(xlabelStr,
                               x = xloc, y = 0.05, size = ((10+ncr)*tickRatio)*labelRatio)
  }
  if (!is.null(xlabel2)) {
    tgd1 <- cowplot::plot_grid(xlabel2, tgd1, ncol=1, rel_heights = c(0.1,1))
    tgd1 <- tgd1 + sm_add_text(xlabel2Str,
                               x = xloc, y = 0.95, size = ((10+ncr)*tickRatio)*labelRatio)
  }
  ## title
  if (!is.null(title)) {
    tgd1 <- cowplot::plot_grid(title, tgd1, ncol=1, rel_heights=c(0.1,1))
    tgd1 <- tgd1 + sm_add_text(titleStr,
                               x = xloc, y = 0.95, size = ((10+ncr)*tickRatio)*labelRatio,
                               fontface='bold')
  }
  ## y-axis
  if (!is.null(ylabel)) {
    tgd1 <- cowplot::plot_grid(ylabel, tgd1, ncol=2, rel_widths = c(0.1,1))
    tgd1 <- tgd1 + sm_add_text(ylabelStr, x = 0.05,
                               y = yloc, angle = 90, size = ((10+ncr)*tickRatio)*labelRatio)
  }
  if (!is.null(ylabel2)) {
    tgd1 <- cowplot::plot_grid(tgd1, ylabel2, ncol=2, rel_widths = c(1,0.1))
    tgd1 <- tgd1 + sm_add_text(ylabel2Str, x = 0.95,
                               y = yloc, angle = 270, size = ((10+ncr)*tickRatio)*labelRatio)
  }

  return(tgd1)
}
