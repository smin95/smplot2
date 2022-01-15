#' Cohen's d - effect size
#'
#' Cohen's d is a measure of the effect size.
#' It is often reported with p-values (ex. from a t-test or posthoc pairwise comparisons).
#'
#' @param group1
#' Numeric vector containing data from one sample (i.e., group 1)
#' that is to be compared with another group.
#' @param group2
#' Numeric vector containing data from another sample (i.e., group 2)
#' that is to be compared with the former group.
#' @param absolute
#' If set TRUE, the function will print the absolute value of the effect size.
#' If set FALSE, the function will print effect size of group2 - group1. For
#' example, it will be positive if group2 has a larger mean than group 1.
#' @export
#' @importFrom stats var
#'
#' @examples
#' \dontrun{
#' group1 <- rnorm(10,0,1)
#' group2 <- rnorm(10,1,1)
#' sm_effsize(group1, group2)
#' }
sm_effsize <- function(group1, group2, absolute = TRUE) {
  n1 <- length(group1) # sample size for group 1
  n2 <- length(group2) # sample size for group 2
  diff <- mean(group2) - mean(group1)
  sd_pool <- sqrt(((n2-1)*var(group2) + (n1-1)*var(group1))/(n1+n2-2))
  effect_size <- diff / sd_pool
  if (absolute == TRUE) {
    return(abs(effect_size))
  } else if (absolute == FALSE) {
    return(effect_size)
  } else {
    stop('absolute has to be a logical value, either TRUE or FALSE.')
  }
}
