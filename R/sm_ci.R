#' Confidence interval
#'
#' @description
#' This function computes the confidence interval.
#'
#' @param data
#' Numerical vector of data
#'
#' @param alpha
#' Default is set to 0.05, so that 95\% confidence interval is computed.
#'
#' @param low
#' If its TRUE, it will compute the low tail of the confidence interval.
#' If its FALSE, it will compute the high tail of the confidence interval.
#'
#' @importFrom stats qt
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#'
#' a <- rnorm(10)
#'
#' sm_ci(a)
#' sm_ci(a, low=F)
#' }
#'
sm_ci <- function(data, alpha=0.05, low=TRUE) {
  if (low == TRUE) {
    res <- mean(data) - qt(p=alpha/2, df=length(data)-1, lower.tail=FALSE) * sm_stdErr(data)
  } else if (low == FALSE) { # upper tail of 95% CI
    res <- mean(data) + qt(p=alpha/2, df=length(data)-1, lower.tail=FALSE) * sm_stdErr(data)
  } else {
    stop('low must be TRUE or FALSE (logical value)')
  }

  return(res)
}
