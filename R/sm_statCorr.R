#' Linear regression slope and statistical values
#' from a paired correlation test
#'
#' @description
#' This combines two different functions:
#' 1) `geom_smooth()` from ggplot, and 2) `stat_cor()` from ggpubr.
#' `geom_smooth()` is used to fit the best-fit model, whereas
#' `stat_cor()` is used to print correlation results at an optimized location.
#'
#' Updates from smplot2 include more flexibility, less input arguments and its
#' pairing with `sm_hvgrid()` / `sm_corr_theme()`.
#'
#' @param ...
#' Arguments for the properties of regression line, such as `linetype`, `color`, etc.
#' For more information, type ?geom_smooth
#'
#' @param fit.params
#' Paramters for the fitted line, such as color, linetype and alpha.
#'
#' @param corr_method
#' Method of the correlation test.
#' Options include: 'pearson', 'kendall', or 'spearman'.
#'
#' @param alternative
#' Specifies the alternative hypothesis (H1). 'two.sided' is the standard way.
#' 'greater' is a positive association, whereas 'less' is a negative association.
#'
#' @param separate_by
#' This marks how the p- and r- values should be separated.
#' The default option is: ','
#' For more information, check out stat_cor() from the ggpubr package.
#'
#' @param label_x
#' Location of the statistical value prints along the figure's x-axis.
#' It asks for a number within the x-axis limit.
#'
#' @param label_y
#' Location of the statistical value prints along the figure's y-axis.
#' It requires a number within the y-axis limit.
#'
#' @param text_size
#' Size (numerical value) of the texts from correlation.
#'
#' @param show_text
#' If the statistical result needs to be displayed, the input should be TRUE (default).
#' If the statistical result is not needed, the input should be FALSE.
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
#' @return Plots a best-fitted linear regression on a correlation plot
#' with results from correlation statistical tests.
#' @import ggplot2 cowplot
#' @importFrom ggpubr stat_cor
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(smplot2)
#'
#' set.seed(11) # generate random data
#' method1 = c(rnorm(19,0,1),2.5)
#' method2 = c(rnorm(19,0,1),2.5)
#' Subject <- rep(paste0('S',seq(1:20)), 2)
#' Data <- data.frame(Value = matrix(c(method1,method2),ncol=1))
#' Method <- rep(c('Method 1', 'Method 2'), each = length(method1))
#' df_general <- cbind(Subject, Data, Method) # used for sm_bar(), sm_boxplot(), sm_violin(), etc

#' df_corr <- data.frame(first = method1, second = method2) # used for correlation
#'
#' ggplot(data = df_corr, mapping = aes(x = first,  y = second)) +
#' geom_point(size = 2) +
#' sm_statCorr()

#' }
sm_statCorr <- function(...,
                        fit.params = list(),
                        corr_method = 'pearson',
                        alternative = 'two.sided',
                        separate_by = ',',
                        label_x = NULL,
                        label_y = NULL,
                        text_size = 4,
                        show_text = TRUE,
                        borders = TRUE,
                        legends = FALSE) {

  params <- list(...)
  fit.params <- modifyList(params, fit.params)

  fitPlot <- do.call('geom_smooth',
                     modifyList(list(method = 'lm', se = FALSE,
                                     alpha = 0.2, weight = 0.8), fit.params))


  textPlot <- ggpubr::stat_cor(p.accuracy = 0.001, method = corr_method,
                               alternative = alternative,
                               label.sep = separate_by,
                               label.x = label_x,
                               label.y = label_y,
                               size = text_size)

  if (show_text == FALSE) {
    textPlot <- NULL
  }

  list(fitPlot,
       textPlot,
       sm_hvgrid(borders = borders, legends = legends))
}
