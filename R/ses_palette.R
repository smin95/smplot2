#' SES custom palette of colors
#'
#' @description
#' This is a custom color palette that SES recommends for data visualization.
#' For more information, please visit \url{https://www.ses21.com}. It returns
#' up to 16 different colors with a high contrast.
#'
#' @param colorNum
#' Number of colors (1-16).
#'
#'
#' @examples
#' ses_palette(3) # returns 3 colors
#'
#' ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class)) +
#' geom_point(size = 2) +
#' scale_color_manual(values = ses_palette(7)) +
#' ses_corr_theme()
#'
ses_palette <- function(colorNum = 10) {

  if (colorNum == 1) {
    output <- c("#1262b3")
  } else if (colorNum ==2) {
    output <- c("#cc1489","#1262b3")
  } else if (colorNum == 3) {
    output <- c("#cc1489","#0f993d","#1262b3")
  } else if (colorNum == 4) {
    output <- c("#cc1489", "#1262b3", "#5b4080", "#e57717")
  } else if (colorNum == 5) {
    output <- c("#cc1489", "#1262b3", "#5b4080", "#e57717", "#0f993d")
  } else {
    output <- c('#7f404a', '#5b4080', '#408073', '#8c994d', '#cc9666',
                '#cc1489', '#1262b3', '#cc3d3d',
                '#da73e6', '#66b1cc', '#0f993d', '#7f5d0d', '#7b3dcc',
                '#45e0e6', '#63e617', '#e57717')
  }

  return(output)
}
