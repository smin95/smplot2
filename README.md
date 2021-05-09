# sesplot

sesplot provides simple themes and a color palette for data visualization. It has been developed to invite people without a coding background to pick up R. Here are the SES functions:

* `ses_bar()`: a SES theme appropriate for plotting bar graphs. 
* `ses_corr()`: a SES theme appropriate for plotting correlations.
* `ses_hist()`: a SES theme appropriate for histograms.
* `ses_slope()`: a SES theme useful for plotting a slope chart.
* `ses_color()`: a SES color palette with up to 16 different colors.

These two specific functions are for correlation plots:

* `ses_corrline()`: plots the linear slope of a correlation based on the function `lm()`.
* `ses_corrtext()`: prints out p- and r-values from a paired correlation test.

### Installation

``` r
install.packages("devtools")
devtools::install_github('smin95/sesplot')
```

### Set-up after installation

```r
library(sesplot) # SES
library(tidyverse) # sample data
```

### Example 1: Bar graph

```{r example}
# sample data: three groups with three different scores

df <- data.frame(group=c("One", "Two", "Three"),
                 score=c(5.2, 9.3, 12))

p1 <- ggplot(data=df, aes(x=group, y=score, fill = group)) +
  geom_bar(stat="identity") 
  
p2 <-  p1 + ses_bar_border(legends = F) + scale_fill_manual(values = ses_color(3))
  
```
<img src="bar.png" width="85%">

Notice that the text sizes, colors and the background have all changed using the SES functions. The texts are larger, the colors more different, and the background less distracting.

### Example 2: Correlation plot

```{r cars}

p3 <- ggplot(data = mtcars, aes(x = drat, y = mpg)) +
  geom_point() 
  
p4 <- p3 + ses_corr() + ses_corrtext(mtcars$drat, mtcars$mpg, x= 3,y=30) + ses_corrline(mtcars$drat, mtcars$mpg)
```

<img src="scatter.png" width="85%">

Notice that the text size and the background have changed using the SES functions, along with the addition of the linear regression and the statistical values. The texts are larger,  the background less distracting, and the scatterplot more informative about the data.
