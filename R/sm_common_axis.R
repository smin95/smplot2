#' A function to plot panels with common x- and y- axes
#'
#' This function should be used when plotting individual data.
#' Please read Chapter 5 of the online guide for more
#' details: smin95.github.io/dataviz
#'
#'
#' @param location
#' Location of the panel.
#' `'topleft'`: removes x-axis title, x-axis ticklabel, y-axis title.
#' `'topright'`:  removes x-axis title, x-axis ticklabel, y-axis title, y-axis ticklabel.
#' `'bottomleft'`: removes x-axis title, y-axis title.
#' `'bottomright'`: removes x-axis title, y-axis title, y-axis ticklabel.
#' `'topcenter'`: removes x-axis title, x-axis ticklabel, y-axis title, y-axis ticklabel.
#' `'bottomcenter'`: removes x-axis title, y-axis title, y-axis ticklabel.
#' `'center'`: removes x-axis title, x-axis ticklabel, y-axis title, y-axis ticklabel.
#'
#' @param margin_size
#' It sets the size of the empty space between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#'
#' @export
#'
#'
sm_common_axis <- function(location, margin_size = 1) {
  if (location == 'topleft') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x= element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      theme(plot.margin = margin(r=margin_size))
  } else if (location == 'topright') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size, l=margin_size))
  } else if (location == 'bottomleft') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size))
  } else if (location == 'bottomright') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size, l=margin_size))
  } else if (location == 'topcenter') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size, l=margin_size))
  } else if (location == 'bottomcenter') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size, l=margin_size))
  } else if (location == 'center') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r=margin_size, l=margin_size))
    } else {
    stop('The location has to be set to either:\n"topleft",\n"topright",\n"bottomleft"\n,"bottomright"\n,"topmiddle"\n,"bottommiddle"')
  }
}
