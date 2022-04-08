#' Calculating the slope across multiple conditions and subjects
#'
#' @description
#' This function returns a data frame containing slope (from linear regression)
#' from a data frame that contains the original raw data.
#'
#' The user can use lm() from base R to compute the slope as well.

#' @param data
#' Name of the variable that stores the data frame that contains
#' the columns with the specified column names.
#'
#' @param subjects
#' The name of the column of the data frame that contains subjects.
#' It must be strings.

#' @param conditions
#' The name of the column of the data frame that contains each condition.
#' It must be strings.

#' @param x
#' The name of the column of the data frame that contains the
#' x-axis points/x coordinates from which the slopes can be calculated.
#' It must be strings. The column must not have
#' characters.

#' @param values
#' The name of the column of the data frame that contains the
#' actual data, which are the y-axis points from which the
#' slope can be calculated. It must be strings.
#'
#' @export
#' @importFrom stats coef lm
#' @examples
#'
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
#' sm_slope_list(data = df, subjects = 'Subject',values = 'Value',
#' conditions = 'Condition',x = 'Day')
#'
#' }
#'
sm_slope_list <- function(data, subjects, conditions, x, values) {
  data[[conditions]] <- as.factor(data[[conditions]])

  subjects_list <- unique(base::as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  cond_list <- unique(data[[conditions]])
  cond_num <- length(cond_list)
  x_val <- unique(data[[x]])
  x_length <- length(unique(x_val))

  slope_list <- data.frame(matrix(ncol = 3, nrow = subj_num*cond_num))
  names(slope_list) <- c(subjects, conditions, paste0('Slope'))

  for (iCond in seq_along(1:cond_num)) {
    for (iSubj in seq_along(1:subj_num)) {
      ind <- which(data[[conditions]] == unique(cond_list)[iCond] &
                     data[[subjects]] == unique(subjects_list)[iSubj])
      lin_model <- lm(data[[values]][ind] ~ x_val)

      slope_list[,1][(cond_num*(iSubj-1))+(iCond)] <- subjects_list[iSubj]
      slope_list[,2][(cond_num*(iSubj-1))+(iCond)] <- cond_list[iCond]
      slope_list[,3][(cond_num*(iSubj-1))+(iCond)] <- coef(lin_model)[[2]]
    }
  }
  slope_list[[conditions]] <- base::as.factor(slope_list[[conditions]])
  levels(slope_list[[conditions]]) <- levels(data[[conditions]])

  print(paste0('Slope = ', values, ' ~ ', x))

  return(slope_list)

}
