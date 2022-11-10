#' A forest plot - a combination of average point, error bar and individual points
#'
#' @description
#' This function visualizes a forest plot, which is most commonly used when describing
#' effect size across various studies in meta-analyses.
#'
#' @param data
#' Data frame. Each column has to be a variable.
#' Each row has to be an observation. This data will get used as
#' argument in ggplot().
#'
#' @param x
#' Column variable of the data that defines the level of the x-axis.
#' This is used to define the aes() in ggplot().
#'
#' @param y
#' Column variable of the data that defines the level of the y-axis.
#' This is used to define the aes() in ggplot().
#'
#' @param err_side
#' String argument to specify the side of the text of the error range (ex. 95% CI interval texts).
#' The options are: 'top' and 'bottom'. This is an similar argument to
#' 'which_side' in the `sm_raincloud()` function.
#'
#' @param errorbar_type
#'
#' This argument determines the errorbar type.
#' If it is set to 'se' , standard error bar will be shown.
#' If it is set to 'sd', the error bar will display standard deviation.
#' If it is set to 'ci' (default), the error bar will display 95\% confidence interval.
#'
#' @param sep_level
#' A numerical value (0-3) that controls the level of the separation among
#' the individual points and the average point. The value can be 0-4.
#' If it's 0, all of these are clustered together. If it's 3, they are all
#' separated. 1 and 2 are somewhere in the middle. Default is set to 2.
#'
#' @param vertical
#'
#' The orientation of the plots. The default is set to FALSE.
#' If you want the horizontal orientation of the plot, please set this argument
#' as FALSE.
#'
#' @param jitter_width
#' A numerical value that determines the degree of the jitter for each point. If its 0,
#' all the points will have no jitter (aligned along the y-axis).
#'
#' @param point_size
#' Size of the individual points.
#'
#' @param avg_point_shape
#' Shape of the average point.
#'
#' @param err_color
#' Color of the error bar.
#'
#' @param avg_point_size
#' Size of the avrage point
#'
#' @param err_text_size
#' Size of the texts that indicate the width of the error.
#'
#' @param borders
#' If the border needs to be displayed, the input should be TRUE.
#' If the border is not needed, the input should be FALSE.
#'
#' @param legends
#' If the legend needs to be displayed, the input should be TRUE.
#' If the legend is not needed, the input should be FALSE.
#'
#'
#' @param ...
#'
#' Parameters for the points' aesthetics, such as 'fill', 'shape', 'color',
#' and 'alpha'. For more information, please type ?geom_point
#'
#' @import ggplot2 cowplot
#' @importFrom ggstance geom_linerangeh
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by mutate
#' @importFrom utils globalVariables
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #' set.seed(2) # generate random data
#' day1 = rnorm(20,0,1)
#' day2 = rnorm(20,5,1)
#' day3 = rnorm(20,6,1.5)
#' day4 = rnorm(20,7,2)
#' Subject <- rep(paste0('S',seq(1:20)), 4)
#' Data <- data.frame(Value = matrix(c(day1,day2,day3,day4),ncol=1))
#' Day <- rep(c('Day 1', 'Day 2', 'Day 3', 'Day 4'), each = length(day1))
#' df2 <- cbind(Subject, Data, Day)
#'
#' sm_forest(data = df2, x = Value, y = Day, alpha = 0.2,
#' error_type = 'sd',
#' err_side = 'top',
#' jitter_width = 0.1, sep_level = 0,
#' avg_point_shape= 23, shape = 21)
#' }
#'
sm_forest <- function(data, x, y,
          err_side = 'top',
          errorbar_type = 'ci',
          sep_level = 2,
          vertical = FALSE,
          jitter_width = 0,
          point_size = 2.5,
          avg_point_shape = 18,
          err_color = 'black',
          avg_point_size = 5,
          err_text_size = 4,
          borders = TRUE,
          legends = FALSE,
          ...) {

  if (jitter_width == 0) {
    jitter_width = 1e-15
  }

  df <- data %>% dplyr::mutate(y_axis = as.numeric(factor({{y}}))) %>%
    dplyr::mutate(jit = jitter(y_axis, amount = jitter_width))

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




  df_ci <- df_avg %>% mutate(ci = paste0('(',round(low,2),', ',round(high,2),')'))


  nLevels = length(unique(df$y_axis))

  if (err_side == 'top') {
    if (sep_level == 3) {
      position_nudge_vector <- c(-0.3, 0,0.3)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(-0.2, 0,0.2)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(-0.1,0,0.1)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,0.35)
    }
  } else if (err_side == 'bottom') {
    if (sep_level == 3) {
      position_nudge_vector <- c(0.3, 0,-0.3)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(0.2, 0,-0.2)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(0.1,0,-0.1)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,-0.35)
    }
  }

  fig <- ggplot(data = df, aes(fill = {{y}}, color = {{y}})) +

    geom_text(data = df_ci,
              aes(y = y_axis, x = avg, label = ci),
              position = position_nudge(y = position_nudge_vector[3]),
              color = 'black', size = err_text_size) +

    geom_point(data = df,
               aes(x = {{x}}, y = jit), size = point_size,
               position = position_nudge(y = position_nudge_vector[1]),...) +

    geom_point(data = df_avg,
               aes(x = avg, y = y_axis), size = avg_point_size,
               shape = avg_point_shape,
               position = position_nudge(y = position_nudge_vector[2])) +

    geom_linerangeh(data = df_avg, aes(y = y_axis, xmin = low, xmax = high),
                    color = err_color) +



    ylab('Group label') + sm_hgrid(borders = borders, legends = legends) +
    xlab('Value')


  if (vertical == TRUE) {
    fig <- fig + coord_flip()
  }
  return(fig)

}

globalVariables(c('ggplot', 'geom_point', 'geom_line', 'geom_linerangeh',
                  'position_nudge', '%>%', 'position','ci','avg','high','low',
                  'y_axis',
                  'geom_flat_violin', 'x_axis', 'jit'))
