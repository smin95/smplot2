#' Calculating Area under Curve across multiple conditions and subjects
#'
#' This function returns a dataframe of AUC computed from raw data
#' ('values' argument).
#'
#' @param subjects
#' A column of 'subjects' from a data frame or a unique list of subjects
#' @param conditions
#' A column of 'Conditions' from a data frame or a unique list of subjects
#' @param x
#' A column of 'x' from a data frame or a unique list of x coordinate.
#' @param values
#' A column of 'values' from a data frame that will be used to
#' the compute area under curve.
#'
#' @return
#'
#'
#' @examples
sm_auc_list <- function(subjects, conditions, x, values) {
  x <- unique(x)
  subjects_list <- unique(subjects)
  subj_num <- length(subjects_list)
  cond_list <- unique(conditions)
  cond_num <- length(cond_list)
  x_length <- length(unique(x))

  auc_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
  names(auc_list) <- c('Subject', 'Condition', 'AUC')

  for (iCond in seq_along(1:cond_num)) {
    for (iSubj in seq_along(1:subj_num)) {
      ind <- which(condition == unique(cond_list)[iCond] &
                     subject == unique(subject)[iSubj])
      auc_list$Subject[(cond_num*(iSubj-1))+(iCond)] <- subj_list[iSubj]
      auc_list$Condition[(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
      auc_list$AUC[(cond_num*(iSubj-1))+(iCond)] <- sm_auc(x,values)
    }
  }

  return(auc_list)
}
