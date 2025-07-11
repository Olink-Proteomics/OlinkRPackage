% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/olink_normalization_utils.R
\name{olink_norm_input_cross_product}
\alias{olink_norm_input_cross_product}
\title{Check if bridge or cross-platform normalization}
\usage{
olink_norm_input_cross_product(
  lst_df,
  lst_cols,
  reference_project,
  product_ids,
  ref_ids
)
}
\arguments{
\item{lst_df}{Named list of datasets to be normalized.}

\item{lst_cols}{Named list of vectors with the required column names for each
dataset in \var{lst_df}.}

\item{reference_project}{Project name of reference_project. Should be one of
\var{df1_project_nr} or \var{df2_project_nr}. Indicates the project to which
the other project is adjusted to.}

\item{product_ids}{Named character vector with the Olink product name that
each input dataset matches to.}

\item{ref_ids}{Named character vector with \var{df1_project_nr} and
\var{df2_project_nr} marked as "ref" and "not_ref".}
}
\value{
Character string indicating the type of normalization to be
performed. One of
bridge, subset, ref_median or norm_cross_product. # nolint line_length_linter
And the updated list of datasets in case of cross-platform normalization.
}
\description{
A function to check whether we are to perform simple bridge normalization, or
cross-platform (Olink Explore 3072 - Olink Explore HT/Olink Reveal)
normalization.

The function uses the internal dataset \var{eHT_e3072_mapping} to determine
the product source of each dataset. If both datasets originate from the same
Olink product, then it will return
bridge. If the datasets to be normalized
originate from Olink Explore HT and Olink Explore 3072
or Olink Reveal and Olink Explore 3072, it will return
norm_cross_product. In any other case an
error is thrown.
}
\author{
Klev Diamanti
}
