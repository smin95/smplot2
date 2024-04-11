#' SM custom palette of colors
#'
#' @description
#' This is a custom color palette that SM recommends for data visualization.
#' It returns up to 20 different colors with a high visibility.
#'
#' @return A character/string of hex codes
#'
#' @param ...
#' The input has to be a character string.of a color name.
#' There are 20 colors available from the SM palette:
#' `'blue'`,`'crimson'`, `'green'`, `'purple'`, `'orange'`,
#' `'skyblue'`, `'pink'`, `'limegreen'`, `'lightpurple'`,
#' `'brown'`, `'red'`, `'lightbrown'`,
#' `'asparagus'`, `'viridian'`, `'darkred'`, `'lightblue'`,
#' `'light blue'`, `'wine'`, `'yellow'`, `'lightgreen'`
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(smplot2)
#' sm_color('crimson')
#'
#' sm_color('crimson', 'green', 'blue')
#' }

sm_color <- function(...) {
  colors <- list(...)

  string_to_hex <- function(color) {
    if (color == 'blue') return("#1262b3")
    if (color == 'crimson') return("#cc1489")
    if (color == 'green') return("#0f993d")
    if (color == 'purple') return("#5b4080")
    if (color == 'orange') return("#e57717")
    if (color == 'skyblue') return('#66b1cc')
    if (color == 'pink') return('#da73e6')
    if (color == 'limegreen') return('#63e617')
    if (color == 'lightpurple') return('#7b3dcc')
    if (color == 'brown') return('#7f5d0d')
    if (color == 'red') return('#cc3d3d')
    if (color == 'lightbrown') return('#cc9666')
    if (color == 'asparagus') return('#8c994d')
    if (color == 'viridian') return('#408073')
    if (color == 'darkred') return('#7f404a')
    if (color == 'lightblue') return('#45e0e6')
    if (color == 'wine') return('#c9b9c6')
    if (color == 'yellow') return('#ffe764')
    if (color == 'yelloworange') return('#ffb359')
    if (color == 'lightgreen') return('#9ee1a8')
  }
  return(unlist(lapply(colors,string_to_hex)))
}


