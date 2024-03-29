% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sm_corr_avgErr.R
\name{sm_corr_avgErr}
\alias{sm_corr_avgErr}
\title{Superimposition of the average point with horizontal and vertical error bars
in the correlation plot}
\usage{
sm_corr_avgErr(
  data,
  x,
  y,
  point.params = list(size = 2.5),
  errh.params = list(height = 0),
  errv.params = list(width = 0),
  errorbar_type = "se",
  ...
)
}
\arguments{
\item{data}{Data frame variable that is used for plotting.}

\item{x}{Column of the data frame that represents the x-axis.}

\item{y}{Column of the data frame that represents the y-axis.}

\item{point.params}{List of parameters for the mean point, such as color, alpha, fill etc}

\item{errh.params}{List of parameters for the horizontal error bar, such as color, alpha, fill etc}

\item{errv.params}{List of parameters for the vertical points, such as color, alpha, fill etc}

\item{errorbar_type}{This argument determines the error bar type.
If it is set to 'se' , standard error bar will be shown.
If it is set to 'sd', the error bar will display standard deviation.
If it is set to 'ci' (default), the error bar will display 95\% confidence interval.}

\item{...}{A generic aesthetic parameter across points and errorbars. This is optional.}
}
\description{
Superimposition of the average point with horizontal and vertical error bars
in the correlation plot
}
\examples{
\dontrun{
library(smplot2)
 set.seed(11) # generate random data
method1 = c(rnorm(19,0,1),2.5)
method2 = c(rnorm(19,0,1),2.5)
Subject <- rep(paste0('S',seq(1:20)), 2)
Data <- data.frame(Value = matrix(c(method1,method2),ncol=1))
Method <- rep(c('Method 1', 'Method 2'), each = length(method1))
df_general <- cbind(Subject, Data, Method) # used for sm_bar(), sm_boxplot(), sm_violin(), etc

df_corr <- data.frame(first = method1, second = method2) # used for correlation

ggplot(data = df_corr, mapping = aes(x = first,  y = second)) +
 geom_point(size = 2) +
 sm_corr_avgErr(df_corr, first,second, errorbar_type = 'se',
                color = sm_color('red'))
}
}
