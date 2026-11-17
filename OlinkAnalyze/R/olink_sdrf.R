#' Create an SDRF table from Olink NPX and sample metadata
#'
#' @description
#' Builds an SDRF-Proteomics **affinity-proteomics** annotation table by
#' combining long-format Olink NPX results with a sample-level metadata table
#' (sometimes called a manifest). Column expectations come from the package
#' internal dictionaries:
#' \itemize{
#'   \item NPX columns: \var{column_name_dict}
#'     (`data-raw/column_name_dict.R`)
#'   \item SDRF columns: \var{sdrf_column_dict}
#'     (`data-raw/sdrf_format.R`, from `data-raw/sdrf/*.yaml`)
#' }
#'
#' The join key between NPX and sample metadata is the NPX sample identifier
#' (typically \code{SampleID}).
#'
#' @details
#' SDRF inheritance resolved for this package is
#' \code{base → sample-metadata → affinity-proteomics}. Required SDRF headers
#' are those with \code{col_requirement == "required"} in
#' \var{sdrf_column_dict}.
#'
#' This function is under development; the body currently prepares column-name
#' lookups and then stops.
#'
#' @param npx Tibble/data frame in long format such as produced by
#' [read_npx()]. Must contain a sample identifier column matching one of the
#' names registered under \code{sample_id} in \var{column_name_dict}.
#' @param sample_metadata Tibble/data frame of sample-level metadata (one row
#' per sample). Must be joinable to \code{npx} on the sample identifier.
#' @param file Optional path to write the SDRF table (TSV). If \code{NULL},
#' only the tibble is returned.
#'
#' @return A tibble whose column names are official SDRF headers from
#' \var{sdrf_column_dict} (affinity-proteomics, including inherited columns).
#'
#' @seealso
#' \code{column_name_dict}, \code{sdrf_column_dict},
#' [read_npx()], [olink_plate_randomizer()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   npx <- OlinkAnalyze::npx_data1
#'   # sample_metadata: user-provided table with SampleID + study variables
#'   # sdrf <- olink_sdrf(npx = npx, sample_metadata = sample_metadata)
#' }
#'
olink_sdrf <- function(npx,
                       sample_metadata,
                       file = NULL) {

  # ---- Column dictionaries (internal sysdata) --------------------------------
  # NPX: data-raw/column_name_dict.R
  # SDRF: data-raw/sdrf_format.R ← data-raw/sdrf/{base,sample-metadata,
  #       affinity-proteomics}.yaml

  ## NPX aliases used when mapping / joining into SDRF
  npx_cols <- list(
    sample_id = get_alt_colnames(col_key = "sample_id"),
    sample_type = get_alt_colnames(col_key = "sample_type"),
    plate_id = get_alt_colnames(col_key = "plate_id"),
    panel = get_alt_colnames(col_key = "panel"),
    quant = get_alt_colnames(col_key = "quant"),
    normalization = get_alt_colnames(col_key = "normalization")
  )

  ## Official SDRF headers (first entry in each col_names vector)
  sdrf_official_names <- vapply(
    X = sdrf_column_dict[["col_names"]],
    FUN = function(nm) as.character(nm[[1L]]),
    FUN.VALUE = character(1L),
    USE.NAMES = TRUE
  )
  names(sdrf_official_names) <- sdrf_column_dict[["col_key"]]

  sdrf_cols <- list(
    all = sdrf_official_names,
    required = sdrf_official_names[
      sdrf_column_dict[["col_requirement"]] == "required"
    ],
    recommended = sdrf_official_names[
      sdrf_column_dict[["col_requirement"]] == "recommended"
    ],
    optional = sdrf_official_names[
      sdrf_column_dict[["col_requirement"]] == "optional"
    ]
  )

  cli::cli_abort(
    c(
      "{.fn olink_sdrf} is not implemented yet.",
      "i" = "NPX SampleID aliases: {.val {npx_cols$sample_id}}.",
      "i" = paste(
        "SDRF required columns ({length(sdrf_cols$required)}):",
        "{.val {unname(sdrf_cols$required)}}."
      ),
      "i" = paste(
        "Inputs received: {.arg npx} ({.cls {class(npx)[1]}}),",
        "{.arg sample_metadata} ({.cls {class(sample_metadata)[1]}}),",
        "{.arg file} = {.val {file}}."
      )
    )
  )
}
