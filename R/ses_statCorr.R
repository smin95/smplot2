ses_statCorr <- function(line_color = 'black',
                       line_type = 'dashed',
                       method = 'pearson',
                       line_size = 1,
                       separate_by = ',',
                       label_x = NULL,
                       label_y = NULL,
                       text_size = 4) {
  list(stat_lm(color = line_color,
               linetype = line_type,
               size = line_size),
       stat_cor(p.accuracy = 0.01, method = method,
                        label.sep = separate_by,
                        label.x = label_x,
                        label.y = label_y,
                size = text_size))
}
