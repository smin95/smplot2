#' Power analysis using two-sample or paired t-test
#' @param group1
#' Numeric vector containing data from one sample (i.e., group 1)
#' that is to be compared with another group.
#' @param group2
#' Numeric vector containing data from another sample (i.e., group 2)
#' that is to be compared with the former group.
#' @param paired
#' A logical indicating whether your two samples (group1 and group2) are paired.
#' @param sig.level
#' Significance level (Type I error probability). Default is set to 0.05.
#' @param power
#' Power of test (1 minus Type II error probability). Default is set to 0.8.
#' @importFrom pwr pwr.t2n.test
#' @export
#'
#' @examples
sm_power <- function(group1, group2, paired,
                     sig.level = 0.05, power = 0.8) {

  eff_size <- sm_cohensD(group1, group2)

  if (paired == TRUE) {
    res <- pwr::pwr.t.test(d = eff_size, sig.level = sig.level,
                           power = power, type = 'paired')
  } else if (paired == FALSE)  {
    res <- pwr::pwr.t.test(d = eff_size, sig.level = sig.level,
                           power = power, type = 'two.sample')
  } else {
    stop('paired has to be a logical value: TRUE or FALSE.')
  }
  return(res)
}
