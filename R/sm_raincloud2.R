sm_raincloud <- function(...,
                         boxplot.params = list(),
                         violin.params = list(alpha = 0.3, color = 'transparent'),
                         point.params = list(alpha = 1, size = 3, shape = 21,
                                             color = 'transparent'),
                         which_side = 'r',
                         sep_level = 2,
                         point_jitter_width = 0.12,
                         vertical = TRUE,
                         borders = TRUE,
                         legends = FALSE) {


  if (which_side == 'right') {
    which_side <- 'r'
  } else if (which_side == 'left') {
    which_side <- 'l'
  }


  params <- list(...)
  point.params <- modifyList(params, point.params)
  boxplot.params <- modifyList(params, boxplot.params)
  violin.params <- modifyList(params, violin.params)


  if (which_side == 'r') {
    if (sep_level == 4) {
      position_nudge_vector <- c(-0.2, 0,0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(-0.15, 0,0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(-0.15,0,0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(-0.08,0,0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,0)
    }
  } else if (which_side == 'l') {
    if (sep_level == 4) {
      position_nudge_vector <- c(0.2, 0,-0.2)
    } else if (sep_level == 3) {
      position_nudge_vector <- c(0.15, 0,-0.15)
    } else if (sep_level == 2) {
      position_nudge_vector <- c(0.15,0,0)
    } else if (sep_level == 1) {
      position_nudge_vector <- c(0.08,0,0)
    } else if (sep_level == 0) {
      position_nudge_vector <- c(0,0,0)
    }
  }

  pointPlot <- do.call('geom_point',
                       modifyList(list(position = position_jitternudge(jitter.width=point_jitter_width,
                                                                       jitter.height=0,
                                                                       seed=10,
                                                                  nudge.x = position_nudge_vector[1])),
                                  point.params))



  boxPlot <- do.call('geom_half_boxplot',
                     modifyList(list(position = position_nudge(x = position_nudge_vector[2]),
                                     side = which_side,
                                     errorbar.draw = FALSE, width = 0.2,
                                     color = 'black'),
                                boxplot.params))

  violinPlot <- do.call('geom_half_violin',
                        modifyList(list(position = position_nudge(x = position_nudge_vector[3]),
                                        side = which_side),
                                   violin.params))




  list(violinPlot,boxPlot,pointPlot,
       sm_hgrid(borders=borders, legends=legends))


}

