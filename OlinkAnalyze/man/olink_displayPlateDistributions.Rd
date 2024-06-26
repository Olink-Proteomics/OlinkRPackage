% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Olink_plate_randomizer.R
\name{olink_displayPlateDistributions}
\alias{olink_displayPlateDistributions}
\title{Plot distributions of a given variable for all plates}
\usage{
olink_displayPlateDistributions(data, fill.color)
}
\arguments{
\item{data}{tibble/data frame in long format returned from the olink_plate_randomizer function.}

\item{fill.color}{Column name to be used as coloring variable for wells.}
}
\value{
An object of class "ggplot" showing the percent distribution of fill.color in each plate (x-axis)
}
\description{
Displays a bar chart for each plate representing the distribution of the given grouping variable on each plate using ggplot2::ggplot and ggplot2::geom_bar.
}
\examples{
\donttest{randomized.manifest <- olink_plate_randomizer(manifest)}
\donttest{olink_displayPlateDistributions(data=randomized.manifest,fill.color="Site")}
}
\seealso{
\itemize{
\item{\code{\link[OlinkAnalyze:olink_plate_randomizer]{olink_plate_randomizer()}} for generating a plating scheme}
\item{\code{\link[OlinkAnalyze:olink_displayPlateLayout]{olink_displayPlateLayout()}} for visualizing the generated plate layouts}
}
}
\keyword{ggplot}
\keyword{plates}
\keyword{randomized}
