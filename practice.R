ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
  ses_corr(legends = F, borders = F) + scale_color_manual(values=ses_color()) +
  ses_corrtext(mpg$displ, mpg$hwy, x = 6, y = 35) +
  ses_corrline(mpg$displ,mpg$hwy)

save_plot("p1.jpg", p1,  base_asp = 1)


df <- data.frame(dose=c("D0.5", "D1", "D2"),
                 len=c(4.2, 10, 29.5))
ggplot(data=df, aes(x=dose, y=len, fill = dose)) +
  geom_bar(stat="identity") + ses_bar(legends=F, borders = F) +
  scale_fill_manual(values = ses_color(3))

p2 <- p + ses_bar_border(legends = F) + scale_fill_manual(values = ses_color(3))

save_plot('p2.jpg',p2, base_asp = 1)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)
ggplot(df, aes(x=weight)) + geom_histogram() + ses_hist()
