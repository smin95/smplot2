#' Standard error
#'
#' @param data
#' Numerical vector of data.
#' @importFrom stats sd
#' @export
#' @return
#' A double vector is returned with a standard error of the input (given sample).
#' @examples
#' library(smplot2)
#' sm_stdErr(rnorm(10,0,1))
#'
#'
#'
sm_stdErr <- function(data) {
  sd(data)/sqrt(length(data))
}
