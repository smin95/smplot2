sm_slope_list <- function(data, subjects, groups, conditions, x, values) {

  x_val <- unique(as.numeric(base::as.character(data[[x]])))
  subjects_list <- unique(base::as.character(data[[subjects]]))
  subj_num <- length(subjects_list)
  x_length <- length(unique(x_val))

  if (missing(groups) && missing(conditions)) {
    stop('conditions or groups argument has to be filled')
  } else if (missing(groups)) {
    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

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

  } else if (missing(conditions)) {
    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])


    slope_list <- data.frame(matrix(ncol = 3, nrow = subj_num*group_num))
    names(slope_list) <- c(subjects, groups, paste0('Slope'))

    for (iGroup in seq_along(1:group_num)) {
      for (iSubj in seq_along(1:subj_num)) {
        ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                       data[[subjects]] == unique(subjects_list)[iSubj])

        lin_model <- lm(data[[values]][ind] ~ x_val)

        slope_list[,1][(group_num*(iSubj-1))+(iGroup)] <- subjects_list[iSubj]
        slope_list[,2][(group_num*(iSubj-1))+(iGroup)] <- cond_list[iGroup]
        slope_list[,3][(group_num*(iSubj-1))+(iGroup)] <- coef(lin_model)[[2]]
      }
    }
    slope_list[[groups]] <- base::as.factor(slope_list[[groups]])
    levels(slope_list[[groups]]) <- levels(data[[groups]])

  } else {

    cond_list <- unique(data[[conditions]])
    cond_num <- length(cond_list)
    data[[conditions]] <- as.factor(data[[conditions]])

    group_list <- unique(data[[groups]])
    group_num <- length(group_list)
    data[[groups]] <- as.factor(data[[groups]])

    slope_list <- data.frame(matrix(ncol = 4, nrow = subj_num*cond_num*group_num))

    names(slope_list) <- c(subjects, conditions, groups, paste0('Slope'))

    for (iGroup in seq_along(1:group_num)) {
      for (iCond in seq_along(1:cond_num)) {
        for (iSubj in seq_along(1:subj_num)) {
          ind <- which(data[[groups]] == unique(group_list)[iGroup] &
                         data[[conditions]] == unique(cond_list)[iCond] &
                         data[[subjects]] == unique(subjects_list)[iSubj])
          if (length(ind)!=0) {
            lin_model <- lm(data[[values]][ind] ~ x_val)

            slope_list[,1][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- subjects_list[iSubj]
            slope_list[,2][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- cond_list[iCond]
            slope_list[,3][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- group_list[iGroup]
            slope_list[,4][(group_num*cond_num*(iSubj-1))+(iGroup+iCond)] <- coef(lin_model)[[2]]
          }

        }
      }
    }

    slope_list[[groups]] <- base::as.factor(slope_list[[groups]])
    levels(slope_list[[groups]]) <- levels(data[[groups]])
    slope_list[[conditions]] <- base::as.factor(slope_list[[conditions]])
    levels(slope_list[[conditions]]) <- levels(data[[conditions]])

  }
  slope_list <- na.omit(slope_list)
  print(paste('Slope = ', values, ' ~ ', x))
  return(slope_list)
}


