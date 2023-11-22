#' NPX Data in Long format.
#'
#' This is a synthetic dataset aiming to use-cases of functions from this
#' package.
#'
#' A tibble with 29,440 rows and 17 columns.
#'
#' `npx_data1` is an Olink NPX data file (tibble) in long format with 158 unique
#' Sample ID's (including 2 repeats each of control samples:
#' CONTROL_SAMPLE_AS 1 CONTROL_SAMPLE_AS 2). The data also contains 1104 assays
#' (uniquely identified using OlinkID) over 2 Panels.
#'
#' @format In addition to standard read_NPX() columns, this dataset also
#' contains columns:
#' \describe{
#'   \item{Subject}{Subject Identifier}
#'   \item{Treatment}{ Treated or Untreated}
#'   \item{Site}{Site indicator, 5 unique values}
#'   \item{Time}{Baseline, Week.6 and Week.12}
#'   \item{Project}{Project ID number}
#'   }
#'
"npx_data1"

#' NPX Data in Long format, a follow-up.
#'
#' This is a synthetic dataset aiming to use-cases of functions from this
#' package.
#'
#' The format is very similar to `npx_data1`. Both datasets can be used to
#' demonstrate the use of normalization functionality.
#'
#' A tibble with 32,384 rows and 17 columns.
#'
#' `npx_data2` is an Olink NPX data file (tibble) in long format  with 174
#' unique Sample ID's (including 2 repeats each of control samples:
#' CONTROL_SAMPLE_AS 1 CONTROL_SAMPLE_AS 2). The data also contains 1,104 assays
#' (uniquely identified using OlinkID) over 2 Panels. This dataset also contain
#' 16 bridge samples with SampleID's that are also present in data (npx_data1).
#' These sample ID's are: A13, A29, A30, A36, A45, A46, A52, A63, A71, A73, B3,
#' B4, B37, B45, B63, B75
#'
#'
#'
#' @format In addition to standard read_NPX() columns, this dataset also
#' contains columns:
#' \describe{
#'   \item{Subject}{Subject Identifier}
#'   \item{Treatment}{Treated or Untreated}
#'   \item{Site}{Site indicator, 5 unique values}
#'   \item{Time}{Baseline, Week.6 and Week.12}
#'   \item{Project}{Project ID number}
#'   }
#'
"npx_data2"

#' Example Sample Manifest
#'
#' Synthetic sample manifest to demonstrate use of functions in this package.
#'
#'
#' A tibble with 138 rows and 4 columns. This manifest contains 26 example
#' subjects, with 6 visits and 2 sites.
#'
#'
#' @format This dataset  contains columns:
#' \describe{
#'   \item{SubjectID}{Subject Identifier, A-Z}
#'   \item{Visit}{Visit Number, 1-6}
#'   \item{SampleID}{138 unique sample IDs}
#'   \item{Site}{Site1 or Site2}
#'   }
#'
"manifest"
