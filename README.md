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

### Examples

```{r example}
library(sesplot)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
