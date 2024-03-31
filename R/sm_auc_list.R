#' Calculating Area under Curve across multiple conditions and subjects
#'
#' This function is deprecated. Use `sm_auc_all` instead.
#'
#' @param ...
#' Parameters of `sm_auc_all`
#' @return Returns a data frame containing area under curve from each subject
#' and experimental condition and/or group.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1) # generate random data
#' day1 = rnorm(16,0,1)
#' day2 = rnorm(16,5,1)
#' Subject <- rep(paste0('S',seq(1:16)), 2)
#' Value <- data.frame(Value = matrix(c(day1,day2),ncol=1))
#' Day <- rep(c(1,2), each = length(day1))
#' Condition <- rep('Control', length(day1)*2)
#' df <- cbind(Subject, Value, Condition, Day)
#'
#' sm_auc_list(data = df, subjects = 'Subject',values = 'Value',
#' conditions = 'Condition',x = 'Day')
#'
#' }
sm_auc_list <- function(...) {
  message('sm_auc_list is deprecated. Use sm_auc_all instead.')
  sm_auc_all(...)
}
