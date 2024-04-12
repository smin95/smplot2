#' Annotation of the error range on the forest plot
#'
#' @param data
#' Data frame variable that is used for plotting.
#' @param x
#' Column of the data frame that represents the x-axis.
#' @param y
#' Column of the data frame that represents the y-axis.
#' @param errorbar_type
#' This argument determines the errorbar type.
#' If it is set to 'se', standard error bar will be shown.
#' If it is set to 'sd' (default), the error bar will display standard deviation.
#' If it is set to 'ci', the error bar will display 95\% confidence interval.
#'
#' @param text.params
#' List of parameters for the text annotation, such as color, size etc
#'
#' @param sep_level
#' A numerical value that controls the level of the separation between
#' the text annotation and the average point.
#' If it's 0, all of these are clustered together. If it's higher (and more positive),
#' the text annotations will increasingly go above the mean point. Default is set to 2. The values
#' can be negative so that the texts can be below the mean point. There is no limit of
#' the range for this argument. Ideally, this should equal to the sep_level in sm_forest().
#'
#' @param ...
#' Parameters for the text annotation, such as size and color etc.
#'
#' @return Annotations showing the range of uncertainty will printed
#' on the forest plot.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(smplot2)
#'
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' day3 = rnorm(20,6,1.5)
#' day4 = rnorm(20,7,2)
#' Subject <- rep(paste0('S',seq(1:20)), 4)
#' Data <- data.frame(Value = matrix(c(day1,day2,day3,day4),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2', 'Day 3', 'Day 4'), each = length(day1))
#' df2 <- cbind(Subject, Data, Day)
#'
#' ggplot(data = df2, aes(x = Value, y = Day, color = Day)) +
#'  sm_forest(point_jitter_width = 0.12, sep_level = 3) +
#'  scale_color_manual(values = sm_palette(4)) +
#'  sm_forest_annot(data = df2, x = Value, y = Day, sep_level = 3)

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

  df_ci <- df_avg %>% dplyr::mutate(int = paste0('(',round(low,2),', ',round(high,2),')'))

  position_nudge_vector <- sep_level/10

  textPlot <- do.call('geom_text',
                      modifyList(list(data = df_ci,
                                      aes(y = y_axis, x = avg, label = int),
                                      position = position_nudge(y = position_nudge_vector)),
                                 text.params))

  return(textPlot)


}

globalVariables(c('ggplot', 'geom_point', 'geom_line', 'geom_linerangeh',
                  'position_nudge', '%>%', 'position','ci','avg','high','low',
                  'y_axis', 'mutate', 'int',
                  'geom_flat_violin', 'x_axis', 'jit'))
