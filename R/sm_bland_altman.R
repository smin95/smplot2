#' A Bland Altman plot
#'
#' @description
#' This function generates a Bland-Altman plot. This function
#' requires two paired data sets as input (same length), and uses sm_statBlandAlt()
#' to compute statistical values necessary for a Bland Altman plot. For more
#' information on these values, please type ?sm_statBlandAlt.
#'
#' The plot automatically uses sm_classic() theme.
#' The upper dashed line indicates the upper limit (mean_diff + 1.96*sd), the middle
#' dashed line indicates the mean difference between the
#' two samples, and the lower dashed line indicates the lower limit
#' (mean_diff - 1.96*sd).
#'
#' To add a legend, you will need to add sm_classic(legends = TRUE).
#' To customise the figure, you can add more geom objects.
#'
#' @param first
#' Data from the first repetition/session
#' @param second
#' Data from the second repetition/session
#' @param point_size
#' The size of the individual points. The default is set to 3.3.
#' @param  diff_ci
#' If set TRUE, then it will draw a shaded region that represents the 95%
#' confidence interval of the difference between the two sessions from one-sample t-test.
#' If the region (i.e. confidence interval) overlaps with zero, then there
#' is no significant bias/difference between the two sessions/datasets.
#' If it does not overlap with 0, then the measurement variability is significantly large.
#' @param ...
#' Parameters of geom_point(), such as 'color', 'fill', 'shape', etc.
#' @export
#' @import ggplot2 cowplot
#' @return
#' Prints a figure, which is the Bland-Altman plot (ggplot2 object).
#' @examples
#' library(smplot2)
#' library(tibble)
#'
#' first <- rnorm(20)
#' second <- rnorm(20)
#' df <- as_tibble(cbind(first,second))
#' sm_bland_altman(df$first, df$second)
#' # when all 3 dashed lines are not shown, extend the range of the y-axis.
#'
sm_bland_altman <- function(first, second,
                             point_size = 3.3,
                             diff_ci = TRUE,
                             ...) {

  statBlandAlt <- sm_statBlandAlt(first, second)

  if (diff_ci == FALSE) {
    fig <- ggplot(data = statBlandAlt$data, aes(x = statBlandAlt$mean, y = statBlandAlt$diff)) +

      # add geom() objects

      geom_abline(intercept = statBlandAlt$upper_limit, slope = 0,linetype = "dashed", size = .4) +
      geom_abline(intercept = statBlandAlt$mean_diff, slope = 0,linetype = "dashed", size = .4) +
      geom_abline(intercept = statBlandAlt$lower_limit, slope = 0,linetype = "dashed", size = .4) +
      geom_point(size = point_size, ...) +
      ylab('Difference between data') +
      xlab('Mean across data') +
      sm_classic(legends = FALSE)
  } else if (diff_ci == TRUE) {
    fig <- ggplot(data = statBlandAlt$data, aes(x = statBlandAlt$mean, y = statBlandAlt$diff)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin= statBlandAlt$diff_ci[[1]],
                  ymax = statBlandAlt$diff_ci[[2]], fill = "grey", alpha = .25) +
      geom_abline(intercept = statBlandAlt$upper_limit, slope = 0,linetype = "dashed", size = .4) +
      geom_abline(intercept = statBlandAlt$mean_diff, slope = 0,linetype = "dashed", size = .4) +
      geom_abline(intercept = statBlandAlt$lower_limit, slope = 0,linetype = "dashed", size = .4) +
      geom_point(size = point_size, ...) +
      ylab('Difference between data') +
      xlab('Mean across data') +
      sm_classic(legends = FALSE)

  }
  return(fig)
}
