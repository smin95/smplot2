#' Linear regression slope from correlation
#'
#' @description
#' This function plots the linear regression of a correlation.
#' This does so by obtain coefficients from the lm() function using
#' two datasets (x and y).
#' It is recommended that the unity slope is plotted only when both
#' axes are scaled to each other (ex. z-scores).
#'
#' @param x
#' Numeric vectors of data (ex. group 1). This corresponds to data in the
#' x-axis of the correlation plot.
#'
#' If the argument 'data' is also included, then x can be the column name
#' of 'data'.
#'
#' @param y
#' Numeric vectors of data (ex. group 2). This corresponds to data in the
#' y-axis of the correlation plot.
#'
#' If the argument 'data' is also included, then y can be the column name
#' of 'data'.
#'
#' @param line_color
#' Color of the linear regression from the correlation.
#' The input should be a character string.
#'
#' @param line_size
#' Line width of the linear regression from the correlation.
#' It should be a number.
#'
#' @param line_linetype
#'
#' Line type of the linear regression from the correlation.
#' The input should be a character string: 'solid', 'dashed', 'dotted',
#' 'blank', 'dotdash', 'longdash', and 'twodash'.
#' @return
#' @export
#'
#' @examples
#'
ses_corrline <- function(x, y, data = .data, line_color = 'black',
                         line_size = 0.6, line_linetype = 'dashed') {

  if (!missing(data)) {
    df1 <- data[[deparse(substitute(x))]]
    df2 <- data[[deparse(substitute(y))]]
  } else {
    df1 <- x
    df2 <- y
  }

  res_lm <- lm(df2 ~ df1)
  coefs <- coef(res_lm)

  ggplot2::geom_abline(aes(slope = coefs[[2]],
                           intercept = coefs[[1]]),
                       size = line_size,
                       linetype = line_linetype,
                       colour = line_color)
}
