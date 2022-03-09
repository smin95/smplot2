#' Calculating Area under Curve across multiple conditions and subjects
#'
#' This function returns a dataframe of AUC computed from raw data
#' ('values' argument).
#'
#' @param subjects
#' The name of the column of the data frame that contains subjects.
#' It must strings, ex. `'Subject'`, not `Subject`.
#' @param conditions
#' The name of the column of the data frame that contains each condition.
#' It must strings, ex. `'Day'`, not `Day`.
#' @param x
#' The name of the column of the data frame that contains the x-axis points
#'  (i.e., x coordinates) from which the AUC can be calculated.
#' It must  strings, ex. `'Time'`, not `Time`.
#' @param values
#' The name of the column of the data frame that contains the
#' actual data, which are the y-axis points from which the
#' AUC can be calculated. It must strings, ex. `'Cbratio'`, not `Cbratio`.
#' @param data
#' Name of the variable that stores the data frame. ex. `df`.
#'
#' @return
#'
#'
#' @examples
sm_auc_list <- function(subjects, conditions, x, values,data) {

  x <- unique(data[[x]])
  subjects_list <- unique(as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  cond_list <- unique(data[[conditions]])
  cond_num <- length(cond_list)
  x_length <- length(unique(x))

  auc_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
  names(auc_list) <- c(subjects, conditions, paste0('AUC_', values))

  for (iCond in seq_along(1:cond_num)) {
    for (iSubj in seq_along(1:subj_num)) {
      ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                     data[[subjects]] == unique(subjects_list)[iSubj])

      auc_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
      auc_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
      auc_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_auc(x,data[[values]][ind])
    }
  }
  auc_list[[conditions]] <- as.factor(auc_list[[conditions]])
  return(auc_list)
}
