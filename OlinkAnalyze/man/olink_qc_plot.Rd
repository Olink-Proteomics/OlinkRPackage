% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/olink_qc_plot.R
\name{olink_qc_plot}
\alias{olink_qc_plot}
\title{Function to plot an overview of a sample cohort per Panel}
\usage{
olink_qc_plot(
  df,
  color_g = "QC_Warning",
  plot_index = F,
  label_outliers = T,
  ...
)
}
\arguments{
\item{df}{NPX data frame in long format. Must have columns SampleID, Index, NPX and Panel}

\item{color_g}{Character value indicating which column to use as fill color (default QC_Warning)}

\item{plot_index}{Boolean. If FALSE (default), a point will be plotted for a sample. If TRUE,
a sample's unique index number is displayed.}

\item{label_outliers}{Boolean. If TRUE, an outlier sample will be labelled with its SampleID.}

\item{...}{coloroption passed to specify color order}
}
\value{
An object of class "ggplot"
}
\description{
Generates a facet plot per Panel using ggplot and ggplot2::geom_point and stats::IQR plotting IQR vs. median for all samples.
Horizontal dashed lines indicate +/-3 standard deviations from the mean IQR.
Vertical dashed lines indicate +/-3 standard deviations from the mean sample median.
}
\examples{
\donttest{olink_qc_plot(npx_data1, color_g = "QC_Warning")}
}
\keyword{NPX}
