p3 <- ggplot(data = mtcars, aes(x = drat, y = mpg)) +
  geom_point(shape = 21, fill = '#0f993d', color = 'white',
             size = 3)
p3 + ses_corr_theme() + ses_statCorr(line_color = '#0f993d')

save_plot('stat.png',p4,base_asp = 1)
