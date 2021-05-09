library(tidyverse)
library(sesplot)
# sample data using random numbers: 2 groups, 20 data points each.
set.seed(1)
group1 = rnorm(20,0,1)
group2 = rnorm(20,5,1)

Subject <- rep(paste0('S',seq(1:20)), 2)
Data <- data.frame(Value = matrix(c(group1,group2),ncol=1))
Time <- rep(c('Before', 'After'), each = length(group1))

DataFrame <- cbind(Subject, Data, Time)

p5 <- ggplot(data = data1, aes(x = Time, y = Value, group = Subject,
                         Fill = Time))  +
  geom_line(color = "gray53", size = .4) +
  geom_point(size = 3, shape= 21, fill = '#0f993d', color = 'white') +
  scale_x_discrete(position = 'top')

p5 + ses_slope()
