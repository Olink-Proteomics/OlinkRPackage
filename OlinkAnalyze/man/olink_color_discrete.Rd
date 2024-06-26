% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Olink_color.R
\name{olink_color_discrete}
\alias{olink_color_discrete}
\title{Olink color scale for discrete ggplots}
\usage{
olink_color_discrete(..., alpha = 1, coloroption = NULL)
}
\arguments{
\item{...}{Optional. Additional arguments to pass to ggplot2::discrete_scale()}

\item{alpha}{transparency}

\item{coloroption}{string, one or more of the following:
c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')}
}
\value{
No return value, called for side effects
}
\description{
Olink color scale for discrete ggplots
}
\examples{
library(ggplot2)

ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) +
geom_point(size = 4) +
olink_color_discrete() +
theme_bw()

ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) +
geom_point(size = 4) +
olink_color_discrete(coloroption = c('lightblue', 'red', 'green')) +
theme_bw()
}
