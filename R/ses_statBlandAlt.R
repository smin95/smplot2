#' Statistics for a Bland-Altman plot
#'
#' @description
#' Bland-Altman plot is drawn to
#' show measurement variability/reliabiilty of a task. This
#' function requires two paired datasets (same length).
#' It returns a list of difference (by element), mean,
#' standard deviation of the difference,
#' mean difference, upper and lower limits. These values
#' are necessary to draw a Bland Altman plot.
#' The list returned from this function can be directly
#' used as an argument for ses_bland_altman(), which
#' draws a Bland-Altman plot using ggplot2.
#'
#' @param first
#' Data from the first repetition/session
#' @param second
#' Data from the second repetition/session
#' @export
#'
#' @examples
ses_statBlandAlt <- function(first, second) {
  diff = second - first
  avg = rowMeans(cbind(first,second),na.rm=T)
  mean_diff = mean(diff)
  sd = sd(diff)
  upper_limit  = mean_diff + 1.96*sd
  lower_limit = mean_diff - 1.96*sd

  res <- list(diff,avg,sd, mean_diff,upper_limit,lower_limit)
  names(res) <- c('diff', 'mean', 'sd', 'mean_diff',
                  'upper_limit','lower_limit')
  return(res)
}
