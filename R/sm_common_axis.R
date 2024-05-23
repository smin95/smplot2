#' A function to plot panels with common x- and y- axes
#'
#' This function is used to create a composite figure.
#'
#' @param location
#' Location of the panel.
#' `'topleft'`: removes x-axis title, x-axis ticklabel, y-axis title.
#' `'topright'`:  removes x-axis title, x-axis ticklabel, y-axis title, y-axis ticklabel.
#' `'bottomleft'`: removes x-axis title, y-axis title.
#' `'bottomright'`: removes x-axis title, y-axis title, y-axis ticklabel.
#' `'topcenter'`: removes x-axis title, x-axis ticklabel, y-axis title, y-axis ticklabel.
#' `'bottomcenter'`: removes x-axis title, y-axis title, y-axis ticklabel.
#' `'single'`: keeps all ticks but removes title
#' `'centerleft'` : removes some ticks and titles
#' `'centerright'` : removes some ticks and titles
#' `'center'`: removes everything
#'
#' @param hmargin
#' The amount of height of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#'
#' @param wmargin
#' The amount of width of blank space between subplots. It sets the size of the empty space (i.e., margin) between panels. T
#' he default is set to 1, which should reduce the empty space (right and left side of each panel)
#' between the panels.
#'
#' @export
#' @return
#' Returns a ggplot2 output with ticks removed.
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#' set.seed(1) # generate random data
#' day1 = rnorm(16,2,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2'), each = length(day1))
#' df <- cbind(Subject, Data, Day)
#'
#' # with aesthetic defaults of smplot
#' ggplot(data = df, mapping = aes(x = Day, y = Value, color = Day)) +
#' sm_bar() +
#' scale_color_manual(values = sm_color('blue','orange')) +
#' sm_common_axis('bottomleft')
#'
#'

sm_common_axis <- function(location, hmargin = 1, wmargin = 1) {
  # 9+1 locations
  if (location == 'topleft') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.bottom = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(axis.text.y.right = element_blank()) +
      theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'topright') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.bottom = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(axis.text.y.left = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomleft') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.top = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(axis.text.y.right = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomright') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.top = element_blank()) +
      ggplot2::theme(axis.text.y.left = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'topcenter') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.bottom = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomcenter') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x.top = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'center') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'centerleft') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y.right = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'centerright') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.x = element_blank()) +
      ggplot2::theme(axis.text.y.left = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'single') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomleft2x') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(axis.text.y.right = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomcenter2x') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.y = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else if (location == 'bottomright2x') {
    ggplot2::theme(axis.title.x = element_blank()) +
      ggplot2::theme(axis.text.y.left = element_blank()) +
      ggplot2::theme(axis.title.y = element_blank()) +
      ggplot2::theme(plot.margin = margin(r = wmargin, t = hmargin, l = wmargin, b = hmargin))
  } else {
    stop('Wrong input. Please check again.')
  }
}
