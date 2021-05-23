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
#'
#' The list returned from this function can be directly
#' used as an argument for ses_bland_altman(), which
#' draws a Bland-Altman plot using ggplot2.
#'
#' Another output 'data' is a tibble with two columns:
#' 1) Mean across each pair for each element
#' (ex. a mean of the 1st element
#' from the first set and 1st element from the second set), 2)
#' Difference between each pair for every element.
#' The output 'data' should be used as a argument for data in ggplot()
#' when plotting.
#'
#' @param first
#' Data from the first repetition/session
#' @param second
#' Data from the second repetition/session
#' @export
#' @importFrom stats t.test
#' @importFrom tibble as_tibble
#' @references
#' Giavarina D. (2015). Understanding Bland Altman analysis. Biochemia medica, 25(2), 141–151. https://doi.org/10.11613/BM.2015.015
#' @examples
#' \dontrun{
#' set.seed(1)
#' first <- rnorm(20)
#' second <- rnorm(20)
#' df <- as_tibble(cbind(first,second)) # requires library(tidyverse)
#' ses_statBlandAlt(df$first, df$second)
#' }


ses_statBlandAlt <- function(first, second) {
  diff = second - first
  mean = rowMeans(cbind(first,second),na.rm=T)
  mean_diff = mean(diff)
  sd = sd(diff)
  upper_limit  = mean_diff + 1.96*sd
  lower_limit = mean_diff - 1.96*sd
  diff_ci <- t.test(diff)$conf.int
  data <- as_tibble(cbind(mean,diff))
  data$diff[[1]] <- upper_limit
  data$diff[[2]] <- lower_limit
  res <- list(diff,mean,sd, mean_diff,upper_limit,lower_limit,
              data, diff_ci)
  names(res) <- c('diff', 'mean', 'sd', 'mean_diff',
                  'upper_limit','lower_limit','data',
                  'diff_ci')
  return(res)
}
