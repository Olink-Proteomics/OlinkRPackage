% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/olink_normalization.R
\name{norm_internal_adjust_not_ref}
\alias{norm_internal_adjust_not_ref}
\title{Add adjustment factors to a dataset}
\usage{
norm_internal_adjust_not_ref(df, name, cols, adj_fct_df, adj_fct_cols)
}
\arguments{
\item{df}{The dataset to be normalized (required).}

\item{name}{Project name of the dataset (required).}

\item{cols}{Named list of column names in the dataset (required).}

\item{adj_fct_df}{Dataset containing the adjustment factors to be applied to
the dataset \code{not_ref_df} (required).}

\item{adj_fct_cols}{Named list of column names in the dataset containing
adjustment factors (required).}
}
\value{
Tibble or ArrowObject with the normalized dataset with additional
columns "Project" and "Adj_factor".
}
\description{
Add adjustment factors to a dataset
}
\author{
Klev Diamanti
}
