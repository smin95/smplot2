#' Calculation of the Area under a Curve (Trapezoidal numerical integration)
#'
#' This is equivalent to Matlab's trapz function.
#'
#' @param x
#' This is the scalar spacing of the coordinate.
#' When the argument for 'x' is not provided, it will calculates the
#' approximate integral value for ‘Y’ with unit spacing based on the length of ‘y’.
#'
#' @param y
#' Numerical data. The length of x and y must be equal.
#'
#' @importFrom zoo rollmean
#'
#' @examples
#' \dontrun{
#' X = c(1,2,3,4,5)
#' Y1 = c(2,3,4,2,3)
#' Y2 = c(3,3,3,3,3)
#'
#' sm_auc(Y2)
#' sm_auc(X,Y1)
#' }
sm_auc <- function(x,y) {
  if (is.null(x)) {
    n <- length(y)
    res <- 0.5*(y[1]+y[n]+2*sum(y[-c(1,n)]))

  } else {
    id <- order(x)
    res <- sum(diff(x[id])* rollmean(y[id],2))
  }
  return(res)
}
