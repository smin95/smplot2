library(tidyverse)
library(sesplot)
library(cowplot)
# sample data using random numbers: 2 groups, 20 data points each.
set.seed(1)
day1 = rnorm(20,0,1)
day2 = rnorm(20,5,1)

Subject <- rep(paste0('S',seq(1:20)), 2)
Data <- data.frame(Value = matrix(c(day2,day2),ncol=1))
Time <- rep(c('Day 1', 'Day 2'), each = length(day1))

DataFrame <- cbind(Subject, Data, Time)

p5 <- ggplot(data = data1, aes(x = Time, y = Value, group = Subject,
                         Fill = Time))  +
  geom_line(color = "gray53", size = .4) +
  geom_point(size = 3, shape= 21, fill = '#0f993d', color = 'white') +
  scale_x_discrete(position = 'top',expand = c(0.15, .1), drop=FALSE,)

save_plot('p5.jpg', p5, base_asp = 1)
p6 <- p5 + ses_slope()
save_plot('p6.jpg', p6, base_asp = 1)

p1 <-  ggplot(data = mtcars, aes(x = drat, y = mpg)) +
  geom_point(shape = 21, fill = '#0f993d', color = 'white',
             size = 3) + ses_corrStat(drat,mpg, data = mtcars,
                                      text_x = 3.2, text_y = 30,
                                      line_color = ses_palette(1,'green'))


p4 <- p1 + ses_corr()

save_plot('p3.jpg', p3, base_asp = 1)
save_plot('p4a.jpg', p4, base_asp = 1)
save_plot('p4.1.jpg', p4.1, base_asp = 1)

df <- data.frame(group=c("One", "Two", "Three"),
                 score=c(5.2, 9.3, 12))

p1 <- ggplot(data=df, aes(x=group, y=score, fill = group)) +
  geom_bar(stat="identity", width = .8)


p2 <- p1 + ses_bar_theme(legends = F) +
  scale_fill_manual(values = ses_palette(3))

ggplot(data=df, aes(x=group, y=score, fill = group)) +
  geom_bar(stat="identity", width = .8)

set.seed(1)
day1 = rnorm(20,0,1)
day2 = rnorm(20,5,1)

Subject <- rep(paste0('S',seq(1:20)), 2)
Data <- data.frame(Value = matrix(c(day1,day2),ncol=1))
Time <- rep(c('Day 1', 'Day 2'), each = length(day1))

DataFrame <- cbind(Subject, Data, Time)
df1 <- DataFrame %>% group_by(Time) %>%
  summarise(mean = mean(Value))
df1

df1 %>% ggplot(aes(Time, mean)) +
  geom_bar(stat="identity", width = .8) +
  geom_linerange(aes(ymin = mean-0, ymax= mean+0))

df1 %>% ggplot(aes(Time, mean, fill = Time)) +
  ses_bar(data = DataFrame,
          aes_x = Time, aes_y = Value,
          width = .7) + ses_bar_theme(legends = F)
save_plot('bar2.png',bar,base_asp=1)

data_summary(DataFrame, varname = 'Value',groupnames=c('Time'))
data_summary(DataFrame, varname = 'Value',
             groupnames= 'Time')
