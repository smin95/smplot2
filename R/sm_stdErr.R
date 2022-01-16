#' Standard error
#'
#' @param data
#' Numerical vector of data.
#' @importFrom stats sd
#' @export
#'
#'
sm_stdErr <- function(data) {
  sd(data)/sqrt(length(data))
}
