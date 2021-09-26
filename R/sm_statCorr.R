#' Title
#' Linear regression slope and statistical values
#' from a paired correlation test
#'
#' @description
#' This combines two different functions:
#' 1) `geom_smooth()` from ggplot, and 2) `stat_cor()` from ggpubr.
#' `geom_smooth()` is used to fit the best-fit model, whereas
#' `stat_cor()` is used to print correlation results at an optimized location.
#'
#' @param ...
#' Arguments for the properties of regression line.
#' For more information, type ?geom_smooth
#' @param line_type
#' Options include:  'blank', 'solid', 'dashed', 'dotted',
#' 'dotdash', 'longdash', 'twodash'.
#' @param corr_method
#' Method of the correlation test.
#' Options include: 'pearson', 'kendall', or 'spearman'.
#' @param ln_method
#' This is the method of producing the best-fit line.
#' 'lm' for linear regression, 'glm' for generalized linear fit, 'loess' for local smooths.
#' @param se
#' If 'TRUE', the confidence interval of the best-fit line will be shown.
#' If 'FALSE', the confidence interval will not be shown.
#' Check `geom_smooth` for more information.
#' @param line_size
#' The thickness of the line. It can be set by giving a number.
#' @param separate_by
#' This marks how the p- and r- values should be separated.
#' The default option is: ','
#' For more information, check out stat_cor() from the ggpubr package.
#' @param label_x
#' Location of the statistical value prints along the figure's x-axis.
#' It asks for a number within the x-axis limit.
#' @param label_y
#' Location of the statistical value prints along the figure's y-axis.
#' It requires a number within the y-axis limit.
#' @param text_size
#' Size (numerical value) of the texts from correlation.
#'
#' @import ggplot2 cowplot
#' @importFrom ggpubr stat_cor
#'
#' @export
sm_statCorr <- function(...,
                       line_type = 'dashed',
                       corr_method = 'pearson',
                       ln_method = 'lm',
                       se = F,
                       line_size = 1,
                       separate_by = ',',
                       label_x = NULL,
                       label_y = NULL,
                       text_size = 4) {
  list(ggplot2::geom_smooth(...,
               linetype = line_type,
               size = line_size, method = ln_method,
               alpha = 0.2, se = se, weight = 0.8),
       ggpubr::stat_cor(p.accuracy = 0.01, method = corr_method,
                        label.sep = separate_by,
                        label.x = label_x,
                        label.y = label_y,
                size = text_size))
}
