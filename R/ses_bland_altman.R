#' SES themed Bland Altman plot
#'
#' @description
#' It automatically uses ses_classic() theme.
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
#'
ses_bland_altman <- function(statBlandAlt, point_size = 3.3,...) {

  list(geom_abline(intercept = statBlandAlt$upper_limit, slope = 0,linetype = "dashed", size = .4),
         geom_abline(intercept = statBlandAlt$mean_diff, slope = 0,linetype = "dashed", size = .4),
         geom_abline(intercept = statBlandAlt$lower_limit, slope = 0,linetype = "dashed", size = .4),
       geom_point(size = point_size, ...),
       sesplot::ses_classic(legends = F))
}
