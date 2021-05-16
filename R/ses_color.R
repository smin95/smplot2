#' SES custom palette of colors
#'
#' @description
#' This is a custom color palette that SES recommends for data visualization.
#' For more information, please visit \url{https://www.ses21.com}. It returns
#' up to 16 different colors with a high visibility.
#'
#' @param color
#' The input has to be a character string.
#' There are 16 colors available from the SES palette:
#' `'blue'`,`'cyan'`, `'green'`, `'purple'`, `'orange'`,
#' `'lightblue'`, `'pink'`, `'limegreen'`, `'lightpurple'`,
#' `'brown'`, `'red'`, `'lightorange'`,
#' `'asparagus'`, `'viridian'`, `'darkred'`
#'
#' @return
#' @export
#'
#' @examples
#' ses_color('cyan')
#'
#' ses_color('cyan', 'green', 'blue')

ses_color <- function(...) {
  colors <- list(...)

  string_to_hex <- function(color) {
    if (color == 'blue') return("#1262b3")
    if (color == 'cyan') return("#cc1489")
    if (color == 'green') return("#0f993d")
    if (color == 'purple') return("#5b4080")
    if (color == 'orange') return("#e57717")
    if (color == 'lightblue') return('#66b1cc')
    if (color == 'pink') return('#da73e6')
    if (color == 'limegreen') return('#63e617')
    if (color == 'lightpurple') return('#7b3dcc')
    if (color == 'brown') return('#7f5d0d')
    if (color == 'red') return('#cc3d3d')
    if (color == 'lightorange') return('#cc9666')
    if (color == 'asparagus') return('#8c994d')
    if (color == 'viridian') return('#408073')
    if (color == 'darkred') return('#7f404a')
  }
  return(unlist(lapply(colors,string_to_hex)))
}


