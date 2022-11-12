#' Calculating Area under Curve across multiple conditions and subjects
#'
#'
#' @description
#' This function returns a data frame containing AUCs from a data frame
#' that contains the original raw data. One of the two arguments 'groups'
#' and 'conditions' must be filled. The function will throw an error
#' if both arguments are empty.

#' @param data
#' Name of the variable that stores the data frame that contains
#' the columns with the specified column names.
#'
#' @param subjects
#' The name of the column of the data frame that contains subjects.
#' It must be strings.
#'
#' @param groups
#' The name of the column of the data frame that contains each group.
#' It must be strings.

#' @param conditions
#' The name of the column of the data frame that contains each condition.
#' It must be strings.

#' @param x
#' The name of the column of the data frame that contains the
#' x-axis points/x coordinates from which the AUC can be calculated.
#' It must be strings. The column must not have
#' characters.

#' @param values
#' The name of the column of the data frame that contains the
#' actual data, which are the y-axis points from which the
#' AUC can be calculated. It must be strings.
#'
#' @importFrom stats na.omit
#'
#' @export
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
#' sm_auc_all(data = df, subjects = 'Subject',values = 'Value',
#' conditions = 'Condition',x = 'Day')
#'
#' }
#'
sm_auc_all <- function(data, subjects, groups, conditions, x, values) {

  x_val <- unique(data[[x]])
  subjects_list <- unique(base::as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  x_length <- length(unique(x_val))

  if (missing(groups) && missing(conditions)) {
    stop('conditions or groups argument has to be filled')
  } else if (missing(groups)) {
    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    auc_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
    names(auc_list) <- c(subjects, conditions, paste0('AUC_', values))

    for (iCond in seq_along(1:cond_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        auc_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
        auc_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
        auc_list[,3][(cond_num*(iSubj-1))+(iCond)] <- sm_auc(x_val,data[[values]][ind])
      }
    }
    auc_list[[conditions]] <- base::as.factor(auc_list[[conditions]])
    levels(auc_list[[conditions]]) <- levels(data[[conditions]])

  } else if (missing(conditions)) {
    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])


    auc_list <- data.frame(matrix(ncol = 3, nrow = subj_num*group_num))
    names(auc_list) <- c(subjects, groups, paste0('AUC_', values))

    for (iGroup in seq_along(1:group_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        auc_list[,1][(group_num*(iSubj-1))+(iGroup)] <- subjects_list[iSubj]
        auc_list[,2][(group_num*(iSubj-1))+(iGroup)] <- group_list[iGroup]
        auc_list[,3][(group_num*(iSubj-1))+(iGroup)] <- sm_auc(x_val,data[[values]][ind])
      }
    }
    auc_list[[groups]] <- base::as.factor(auc_list[[groups]])
    levels(auc_list[[groups]]) <- levels(data[[groups]])

  } else {

    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])

    auc_list <- data.frame(matrix(ncol = 4, nrow = subj_num*cond_num*group_num))

    names(auc_list) <- c(subjects, conditions, groups, paste0('AUC_', values))

    for (iGroup in seq_along(1:group_num)) {
      for (iCond in seq_along(1:cond_num)) {
        for (iSubj in seq_along(1:subj_num)) {
          ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                         data[[conditions]] == unique(cond_list)[iCond] &
                         data[[subjects]] == unique(subjects_list)[iSubj])
          if (length(ind)!=0) {
            auc_list[,1][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- subjects_list[iSubj]
            auc_list[,2][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- cond_list[iCond]
            auc_list[,3][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- group_list[iGroup]
            auc_list[,4][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- sm_auc(x_val,data[[values]][ind])
          }

        }
      }
    }

    auc_list[[groups]] <- base::as.factor(auc_list[[groups]])
    levels(auc_list[[groups]]) <- levels(data[[groups]])
    auc_list[[conditions]] <- base::as.factor(auc_list[[conditions]])
    levels(auc_list[[conditions]]) <- levels(data[[conditions]])

  }
  auc_list <- na.omit(auc_list)
  print(paste('AUC =', values, '*', x))
  return(auc_list)
}

sm_auc_list <- function(...) {
  message('sm_auc_list is deprecated. Use sm_auc_all instead.')
  sm_auc_all(...)
}
