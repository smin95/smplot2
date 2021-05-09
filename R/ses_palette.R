#' SES custom palette of colors
#'
#' @description
#' This is a custom color palette that SES recommends for data visualization.
#' For more information, please visit \url{https://www.ses21.com}. It returns
#' up to 16 different colors with a high contrast.
#'
#' @param colorNum
#' A numerical input that indicates the number of colors (1-16).
#' @param color
#' A character string of a specific color: 'blue', 'red', 'purple',
#' 'green', 'orange'. These colors are from the SES palette.
#'
#' This only works when colorNum = 1.
#'
#' @return
#' @export
#'
#' @examples
#' ses_color(2) # returns two colors
#'
#' # plot a bar graph with a different color for each group
#'
#' df <- data.frame(group=c("One", "Two", "Three"),
#' score=c(4.8, 9, 8.2))
#'
#' p <- df %>%
#' ggplot(aes(x=group, y=score, fill = group)) +
#' geom_bar(stat='identity')
#'
#' p +
#' ses_bar_border() +
#' ses_color(nrow(df)) # number of colors = number of groups

ses_palette <- function(colorNum = 10, color) {

  if (colorNum == 1) {
    if (color == 'blue') return("#1262b3")
    if (color == 'red') return("#cc1489")
    if (color == 'green') return("#0f993d")
    if (color == 'purple') return("#5b4080")
    if (color == 'orange') return("#e57717")

  }

  if (!missing(color)) {
    stop('Sorry, color argument only works when colorNum = 1')
  } else {
    if (colorNum == 2) {
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
  }

  return(output)
}
