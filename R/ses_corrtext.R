#' Annotation of results from a paired correlation test on ggplot2
#'
#' @description
#' This function pastes the texts of the results from a correlation test
#' on ggplot2. For more information, please visit \url{https://www.ses21.com}.
#'
#' @param df1
#' Numeric vectors of data (ex. group 1).
#' @param df2
#' Numeric vectors of data (ex. group 2).
#' df1 and df2 must have the same length and paired.
#' @param x
#' Location of the text in the x-axis of the plot.
#' @param y
#' Location of the text in the y-axis of the plot.
#' @param method
#' Method of computing the correlation coefficient: 'pearson', 'kendall',
#' or 'spearman'.
#' 'Pearson' is often used for normally distributed data.
#' 'Spearman' is often used for computing the strength of a non-linear correlation.
#' @param p
#' Logical input is required ('TRUE' for displaying the p-value, '
#' FALSE' for not displaying the p-value).
#' p-value is used for determining statistical significance in the
#' hypothesis test. It depends on the sample size.
#' @param r
#' Logical input is required ('TRUE' for displaying the r-value, '
#' FALSE' for not displaying the r-value).
#' r-value is the correlation coefficient. It does not depend on
#' @param text_color
#' Color of the text in character string (ex. 'black').
#' @param font
#' Font size of the text
#' @param size
#' Font of the text in character string.
#' @return
#' @export
#'
#' @examples
#'
ses_corrtext <- function(df1, df2, x, y, method = 'pearson', p = TRUE,
                         r = TRUE, text_color = 'black',
                         font = '', size = 5) {

  res <- cor.test(df1, df2, method = method)
  p_val <- res$p.value
  r_val <- res$estimate[[1]]

  if (p_val < 0.001) {
    p_str <- 'p < 0.001'
  } else {
    p_str <- paste('p =', signif(p_val, 2))
  }

  if (p == TRUE) {
    if (r == TRUE) {
      ggplot2::annotate("text",label = paste('r =', signif(res$estimate,2),
                                    '\n',p_str),
               x=x, y=y, color = text_color, family = font, size = size)
    } else if(r == FALSE) {
      ggplot2::annotate("text",label = p_str,
               x=x, y=y, color = text_color, family = font, size = size)
    }
  } else if (p == FALSE) {
    if (r == TRUE) {
      ggplot2::annotate("text",label = paste('r =', signif(res$estimate,2)),
               x=x, y=y, color = text_color, family = font, size = size)
    } else if (r == FALSE) {

    }
  }
}
