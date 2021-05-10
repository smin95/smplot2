p3 <- ggplot(data = mtcars, aes(x = drat, y = mpg)) +
  geom_point(shape = 21, fill = '#0f993d', color = 'white',
             size = 3)
p4 <- p3 + ses_corr_theme() + ses_statCorr(line_color = '#0f993d')

save_plot('stats.png',p4,base_asp = 1)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = iris, aes(x = Sepal.Length, y =Petal.Length,
                        color = Species)) +
  geom_point(shape = 21, size = 3) +
  ses_statCorr(aes(color = Species))
