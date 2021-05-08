#' Linear regression and unity slopes from correlation
#'
#' @description
#' This function plots the linear regression of a correlation.
#' This does so by obtain coefficients from the lm() function using
#' two datasets (df1 and df2).
#' Unity slope, which has a slope of 1, is also plotted if called
#' (unity = 'TRUE).
#' It is recommended that the unity slope is plotted only when both
#' axes are scaled to each other (ex. z-scores).
#'
#' @param df1
#' Numeric vectors of data (ex. group 1). This corresponds to data in the
#' x-axis of the correlation plot.
#'
#' @param df2
#' Numeric vectors of data (ex. group 2). This corresponds to data in the
#' y-axis of the correlation plot.
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
#'
#' @param unity
#'
#' A reference line with a slope of 1 (unity line).
#' If the unity line should be displayed, the input should be 'TRUE'.
#' If it is not wanted, then the input should be 'FALSE'.
#'
#' @param unity_color
#' Color of the unity line.
#' The input should be a character string.
#'
#' @param unity_size
#' Size of the unity line.
#' It should be a number.
#' @param unity_type
#'
#' Line type of the unity line.
#' The input should be a character string: 'solid', 'dashed', 'dotted',
#' 'blank', 'dotdash', 'longdash', and 'twodash'.
#'
#' @param unity_slope
#' 'Positive' if the unity line has a slope of 1.
#' 'Negative' if the unity line has a slope of -1.
#'
#' @return
#' @export
#'
#' @examples
#'
ses_corrline <- function(df1, df2, line_color = '#1262b3', line_size = 0.6, line_linetype = 'dashed',
                         unity = TRUE, unity_slope = 'positive', unity_color = 'black', unity_size = 0.4,
                         unity_type = 'dashed') {

  res_lm <- lm(df2 ~ df1)
  coefs <- coef(res_lm)

  if (unity_slope == 'positive') {
    unity_slope = 1
  } else if (unity_slope == 'negative') {
    unity_slope = -1
  } else {
    error('unity_slope argument has an error')
  }

  if (unity == TRUE) {
    list(ggplot2::geom_abline(intercept = 0, slope = unity_slope,
                              linetype = unity_type,
                              size = unity_size,
                              color = unity_color),
         ggplot2::geom_abline(aes(slope = coefs[[2]],
                                  intercept = coefs[[1]]),
                              size = line_size,
                              linetype = line_linetype,
                              colour = line_color))

  } else if (unity == FALSE) {

    ggplot2::geom_abline(aes(slope = coefs[[2]],
                             intercept = coefs[[1]]),
                         size = line_size,
                         linetype = line_linetype,
                         colour = line_color)
  }
}
