#' SES custom palette of colors
#'
#' @description
#' This is a custom color palette that SES recommends for data visualization.
#' For more information, please visit \url{https://www.ses21.com}. It returns
#' up to 16 different colors with a high contrast.
#'
#' @param color
#' If the input is a character string, it has to be the name of a specific color: 'blue', 'red', 'purple',
#' 'green', 'orange'. These colors are from the SES palette.
#'
#' If the input is a number, then it should mean the number of colors.
#' Numbers less than 17 are accepted.
#' This function returns up to 16 different functions.
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

ses_color <- function(...) {
  colors <- list(...)

  string_to_hex <- function(color) {
    if (color == 'blue') return("#1262b3")
    if (color == 'red') return("#cc1489")
    if (color == 'green') return("#0f993d")
    if (color == 'purple') return("#5b4080")
    if (color == 'orange') return("#e57717")
    if (color == 'lightblue') return('#66b1cc')
    if (color == 'pink') return('#da73e6')
    if (color == 'limegreen') return('#63e617')
    if (color == 'lightpurple') return('#7b3dcc')
    if (color == 'brown') return('#7f5d0d')
    if (color == 'lightred') return('#cc3d3d')
    if (color == 'lightorange') return('#cc9666')
    if (color == 'asparagus') return('#8c994d')
    if (color == 'viridian') return('#408073')
    if (color == 'darkred') return('#7f404a')
  }
  return(unlist(lapply(colors,string_to_hex)))
}


