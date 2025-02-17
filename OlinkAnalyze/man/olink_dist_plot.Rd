% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_plot.R
\name{olink_dist_plot}
\alias{olink_dist_plot}
\title{Function to plot the NPX distribution by panel}
\usage{
olink_dist_plot(df, color_g = "QC_Warning", ...)
}
\arguments{
\item{df}{NPX data frame in long format. Must have columns SampleID, NPX and Panel}

\item{color_g}{Character value indicating which column to use as fill color (default: QC_Warning)}

\item{...}{Color option passed to specify color order.}
}
\value{
An object of class "ggplot" which displays NPX distribution for each sample per panel
}
\description{
Generates boxplots of NPX vs. SampleID colored by QC_Warning (default) or any other grouping variable
and faceted by Panel using ggplot and ggplot2::geom_boxplot.
}
\examples{
\donttest{olink_dist_plot(npx_data1, color_g = "QC_Warning")}
}
\keyword{NPX}
