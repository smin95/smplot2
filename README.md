# sesplot

The goal of sesplot is to provide custom SES themes/colors for plotting data. Here are the SES functions:

* 'ses_bar()': a SES custom theme that is useful for plotting bar graphs. It has no border.
* 'ses_bar_border()': a SES custom theme that is useful for plotting bar graphs. It has a border and a horizontal grid.
* 'ses_corr': a SES custom theme that is useful for plotting correlations.
* 'ses_hist': a SES custom theme that is useful for plotting histograms. 
* 'ses_color': a SES color palette with 16 different colors.

### Install
``` r
install.packages("devtools")
devtools::install_github('smin95/sesplot')
```

### Examples

This is a basic example which shows you how to solve a common problem:

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
