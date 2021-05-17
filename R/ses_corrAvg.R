ses_summary <- function(data = .data, x, y) {

  z <- plyr::ddply(data,plyr::.(), plyr::summarise,
                   xSE = sesplot::se(x),
                   ySE = sesplot::se(y),
                   xMean = mean(x),
                   yMean = mean(y))
  return(z)
}
