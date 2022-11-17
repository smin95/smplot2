#' Calculating Area under Curve across multiple conditions and subjects
#'
#' This function is deprecated. Use `sm_auc_all` instead.
#'
#' @param ...
#' Parameters of `sm_auc_all`
#' @return
#' @export
#'
#' @examples
sm_auc_list <- function(...) {
  message('sm_auc_list is deprecated. Use sm_auc_all instead.')
  sm_auc_all(...)
}
