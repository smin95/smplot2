#' SM custom palette of colors
#'
#' @description
#' This is a custom color palette that SM recommends for data visualization.
#' It returns up to 20 different colors with a high contrast.
#'
#' @param colorNum
#' Number of colors (1-20).
#'
#' @examples
#' \dontrun{
#' #' sm_palette(3) # returns 3 colors
#' }
#'
#' @export
sm_palette <- function(colorNum = 10) {

  if (colorNum == 1) {
    output <- c("#1262b3")
  } else if (floor(colorNum) == 2) {
    output <- c("#cc1489","#1262b3")
  } else if (floor(colorNum) == 3) {
    output <- c("#cc1489","#0f993d","#1262b3")
  } else if (floor(colorNum) == 4) {
    output <- c("#cc1489", "#1262b3", "#5b4080", "#e57717")
  } else if (floor(colorNum) == 5) {
    output <- c("#cc1489", "#1262b3", "#5b4080", "#e57717", "#0f993d")
  } else if (floor(colorNum) <= 20 ) {
    output <- c('#7f404a', '#5b4080', '#408073', '#8c994d', '#cc9666',
                '#cc1489', '#1262b3', '#cc3d3d',
                '#da73e6', '#66b1cc', '#0f993d', '#7f5d0d', '#7b3dcc',
                '#45e0e6', '#63e617', '#e57717', '#c9b9c6', '#ffe764',
                '#ffb359', '#9ee1a8')
  } else {
    stop('Number of colorNum must be 1 to 20.')
  }

  return(output)
}
