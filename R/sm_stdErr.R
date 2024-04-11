#' Standard error
#'
#' @param data
#' Numerical vector of data.
#' @importFrom stats sd
#' @export
#' @examples
#' \dontrun{
#' sm_stdErr(rnorm(10,0,1))
#' }
#'
#'
sm_stdErr <- function(data) {
  sd(data)/sqrt(length(data))
}
