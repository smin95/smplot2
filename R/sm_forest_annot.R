sm_forest_annot <- function(data, x, y, errorbar_type = 'ci',
                            text.params = list(size=4,color='black'),
                            sep_level = 2, ...) {

  params <- list(...)
  text.params <- modifyList(params, text.params)

  df <- data %>% dplyr::mutate(y_axis = as.numeric(factor({{y}})))

  if (errorbar_type == 'ci') {
    df_avg <- df %>% dplyr::group_by({{y}}, y_axis) %>%
      dplyr::summarise(avg = mean({{x}}),
                       low = sm_ci({{x}}, low = TRUE),
                       high = sm_ci({{x}}, low = FALSE))
  } else if (errorbar_type == 'sd') {
    df_avg <- df %>% dplyr::group_by({{y}}, y_axis) %>%
      dplyr::summarise(avg = mean({{x}}),
                       low = mean({{x}}) - sd({{x}}),
                       high = mean({{x}}) + sd({{x}}))
  } else if (errorbar_type == 'se') {
    df_avg <- df %>% dplyr::group_by({{y}}, y_axis) %>%
      dplyr::summarise(avg = mean({{x}}),
                       low = mean({{x}}) - sm_stdErr({{x}}),
                       high = mean({{x}}) + sm_stdErr({{x}}))
  }

  df_ci <- df_avg %>% mutate(int = paste0('(',round(low,2),', ',round(high,2),')'))

  position_nudge_vector <- sep_level/10

  textPlot <- do.call('geom_text',
                      modifyList(list(data = df_ci,
                                      aes(y = y_axis, x = avg, label = int),
                                      position = position_nudge(y = position_nudge_vector)),
                                 text.params))

  return(textPlot)


}

