#' SES themed Bland Altman plot
#'
#' @description
#' This function generates a Bland-Altman plot using the output
#' from ses_statBlandAlt. The plot automatically uses ses_classic() theme.
#' The upper dashed line indicates the upper limit (mean_diff + 1.96*sd), the middle
#' dashed line indicates the mean difference between the
#' two samples, and the lower dashed line indicates the lower limit
#' (mean_diff - 1.96*sd).
#'
#' To add a legend, you will need to add ses_classic(legends = TRUE).
#'
#' @param statBlandAlt
#' Results from ses_statBlandAlt(), which is a function
#' that calculates important indices from drawing a Bland-Altman plot.
#' @param point_size
#' The size of the individual points. The default is set to 3.3.
#' @param ...
#' Parameters of geom_point(), such as 'color', 'fill', 'shape', etc.
#' @export
#' @import ggplot2 cowplot
#' @examples
#' \dontrun{
#' set.seed(1)
#' first <- rnorm(20)
#' second <- rnorm(20)
#' df <- as_tibble(cbind(first,second)) # requires library(tidyverse)
#' res <- ses_statBlandAlt(df$first, df$second)
#' df %>% ggplot(aes(x = res$mean, y = res$diff)) +
#' ses_bland_altman(res)
#' }
#'
ses_bland_altman <- function(statBlandAlt, point_size = 3.3,...) {

  list(geom_abline(intercept = statBlandAlt$upper_limit, slope = 0,linetype = "dashed", size = .4),
         geom_abline(intercept = statBlandAlt$mean_diff, slope = 0,linetype = "dashed", size = .4),
         geom_abline(intercept = statBlandAlt$lower_limit, slope = 0,linetype = "dashed", size = .4),
       geom_point(size = point_size, ...),
       sesplot::ses_classic(legends = F))
}
