% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/olink_normalization_product.R
\name{olink_normalization_qs}
\alias{olink_normalization_qs}
\title{Quantile smoothing normalization of all proteins between two NPX projects.}
\usage{
olink_normalization_qs(
  lst_df,
  ref_cols,
  not_ref_cols,
  bridge_samples,
  ref_product
)
}
\arguments{
\item{lst_df}{A named list of the 2 input datasets. First element should be
the reference dataset from Olink Explore HT and the second element should
originate from Olink Explore 3072. (required)}

\item{ref_cols}{A named list with the column names to use. Exported from
olink_norm_input_check. (required)}

\item{not_ref_cols}{A named list with the column names from the non-reference
dataset. Exported from olink_norm_input_check. (required)}

\item{bridge_samples}{Character vector of samples to be used for the
quantile mapping. (required)}

\item{ref_product}{Name of reference product. Must be one of "HT" or
"Reveal".}
}
\value{
A "tibble" of Olink data in long format containing both input
datasets with the quantile normalized quantifications.
}
\description{
This function uses bridge samples to map quantiles of the non-reference
dataset to the ones of the reference dataset. Mapped quantiles are used to
transform the quantifications of the the non-reference dataset to the
reference.
}
\details{
In the case when a study is separated into multiple projects,
an additional normalization step is needed to allow the data to be
comparable across projects. Across different Olink products, some of the
assays exist in corresponding but distinct NPX spaces. For those assays,
the median of paired differences is insufficient for bridging as it only
considers one anchor point (the median/50\% quantile). Instead, quantile
smoothing (QS) using multiple anchor points (5\%, 10\%, 25\%, 50\%, 75\%, 90\%
and 95\% quantiles) is favored to map the Explore 3072 data to the Explore
HT distribution. The \code{olink_normalization_qs()} performs quantile smoothing
bridging normalization between datasets from two Olink products (for example
Olink Explore 3072 and Olink Explore HT) by performing the following
steps: \cr

\itemize{

\item An empirical cumulative distribution function is used to map
datapoints for the bridging samples from one product to the equivalent
space in the other product.
\item A spline regression model is constructed using unmapped
and mapped data from one product, using anchor points from the
quantiles defined above.
\item The spline regression model is used to predict the normalized
NPX values for all datapoints

}

More information on quantile smoothing and between product normalization
can be found in the Bridging Olink Explore 3072 to Olink Explore HT tutorial.
}
\examples{
\donttest{
# Bridge samples
bridge_samples <- intersect(
  x = unique(OlinkAnalyze:::data_ht_small$SampleID),
  y = unique(OlinkAnalyze:::data_3k_small$SampleID)
) |>
  (\(x) x[!grepl("CONTROL", x)])()

# Run the internal function olink_norm_input_check
check_norm <- OlinkAnalyze:::olink_norm_input_check(
  df1 = OlinkAnalyze:::data_ht_small,
  df2 = OlinkAnalyze:::data_3k_small,
  overlapping_samples_df1 = bridge_samples,
  overlapping_samples_df2 = NULL,
  df1_project_nr = "P1",
  df2_project_nr = "P2",
  reference_project = "P1",
  reference_medians = NULL
)

# Named list of input datasets
lst_df <- list(
  check_norm$ref_df,
  check_norm$not_ref_df
)
names(lst_df) <- c(check_norm$ref_name, check_norm$not_ref_name)

ref_cols <- check_norm$ref_cols
not_ref_cols <- check_norm$not_ref_cols

qs_result <- OlinkAnalyze:::olink_normalization_qs(
 lst_df = lst_df,
 ref_cols = ref_cols,
 not_ref_cols = not_ref_cols,
 bridge_samples = bridge_samples,
 ref_product = "HT"
)
}

}
\author{
Amrita Kar
Marianne Sandin
Masoumeh Sheikhi
Klev Diamanti
}
\keyword{Normalization}
\keyword{Quantile}
\keyword{Smoothing}
