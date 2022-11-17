#' Calculating the slope across multiple conditions and subjects
#'
#' This function is deprecated. Use `sm_slope_all` instead.
#' @param ...
#' This is the parameter for `sm_slope_all`.
#'
#' @return
#' @export
#'
#' @examples
sm_slope_list <- function(...) {
  message('sm_slope_list is deprecated. Use sm_slope_all instead.')
  sm_auc_all(...)
}
