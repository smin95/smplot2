#' Annotation of results from a paired correlation test on ggplot2
#'
#' @description
#' This function pastes the texts of the results from a correlation test
#' on ggplot2. For more information, please visit \url{https://www.ses21.com}.
#'
#' @param x
#' Data set of the x-axis in the correlation plot.
#' If the argument 'data' is filled, then x can be the column name.
#' Otherwise, it has to take a form of numerical vector.
#'
#' @param y
#' Data set of the y-axis in the correlation plot.
#' If the argument 'data' is filled, then y can be the column name.
#' Otherwise, it has to take a form of numerical vector.
#' x and y must have the same length.
#'
#' @param text_x
#' Location of the text in the x-axis of the plot.
#' @param text_y
#' Location of the text in the y-axis of the plot.
#'
#' @param data
#' Dataframe that contains x and y. This argument is optional.
#'
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
#' Font of the text in character string.
#'
#' @param size
#'Font size of the text
#'
#' @return
#' @export
#'
#' @examples
#'
ses_corrtext <- function(x, y, text_x, text_y, data = data, method = 'pearson', p = TRUE,
                         r = TRUE, text_color = 'black',
                         font = '', size = 4) {

  if (!missing(data)) {
    df1 <- data[[deparse(substitute(x))]]
    df2 <- data[[deparse(substitute(y))]]
  } else {
    df1 <- x
    df2 <- y
  }

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
                        x=text_x, y=text_y, color = text_color, family = font, size = size)
    } else if(r == FALSE) {
      ggplot2::annotate("text",label = p_str,
                        x=text_x, y=text_y, color = text_color, family = font, size = size)
    }
  } else if (p == FALSE) {
    if (r == TRUE) {
      ggplot2::annotate("text",label = paste('r =', signif(res$estimate,2)),
                        x=text_x, y=text_y, color = text_color, family = font, size = size)
    } else if (r == FALSE) {
      error('Two FALSEs were used as input')
    }
  }
}


