#' SES custom colors
#'
#' @description
#' This is a custom color palette that SES recommends for data visualization.
#' For more information, please visit \url{https://www.ses21.com}. It returns
#' up to 16 different colors with a high contrast.
#'
#' @param colorNum
#' A numerical input that indicates the number of colors.
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

ses_color <- function(colorNum = 10) {

  if (colorNum == 2) {
    color <- c("#cc1489","#1262b3")
  } else if (colorNum == 3) {
    color <- c("#cc1489","#1262b3","#408073")
  } else if (colorNum == 4) {
    color <- c("#cc1489", "#1262b3", "#5b4080", "#e57717")
  } else if (colorNum == 5) {
    color <- c("#cc1489", "#1262b3", "#5b4080", "#e57717", "#0f993d")
  } else {
    color <- c('#7f404a', '#5b4080', '#408073', '#8c994d', '#cc9666',
               '#cc1489', '#1262b3', '#cc3d3d',
               '#da73e6', '#66b1cc', '#0f993d', '#7f5d0d', '#7b3dcc',
               '#45e0e6', '#63e617', '#e57717')
  }

  return(color)
}
