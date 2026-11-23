#' Convert MSstats protein-level data to NPX-style long format
#'
#' @description
#' Reformats an MSstats (or DIA-NN via MSstats) **protein-level** table —
#' typically `dataProcess(...)$ProteinLevelData` — into a long NPX-style
#' tibble that can be used with OlinkAnalyze helpers such as
#' [check_npx()], [clean_npx()], [olink_dist_plot()], and [olink_ttest()].
#'
#' Expected input columns (names are matched after snake_case cleaning, so
#' both \code{LogIntensities} and \code{log_intensities} work):
#' \itemize{
#'   \item \strong{required:} \code{Protein}, \code{LogIntensities}
#'   \item \strong{sample ID:} \code{originalRUN} (preferred) or \code{RUN}
#'   \item \strong{optional:} \code{GROUP} / \code{Condition},
#'     \code{SUBJECT}, \code{more50missing}
#' }
#'
#' Each distinct protein receives a synthetic \code{OlinkID} of the form
#' \code{OID#####}. \code{UniProt} and \code{Assay} are set from
#' \code{Protein}; \code{NPX} is set from \code{LogIntensities}.
#'
#' @param data A data frame / tibble of protein-level intensities, or a
#' character path to a CSV / TSV file containing that table.
#' @param panel Character value for the \code{Panel} column.
#' Default \code{"MSstats/DIANN protein"}.
#' @param panel_version Character value for \code{Panel_Version}.
#' Default \code{"none"}.
#' @param check Logical. If \code{TRUE}, run [check_npx()] on the result
#' (warnings only; does not abort). Default \code{FALSE}.
#' @param clean Logical. If \code{TRUE}, run [clean_npx()] after conversion
#' (keeps QC warnings and control-like rows; drops non-finite NPX).
#' Default \code{FALSE}.
#'
#' @return A tibble in long NPX-style format with columns
#' \code{SampleID}, \code{OlinkID}, \code{UniProt}, \code{Assay}, \code{NPX},
#' \code{MissingFreq}, \code{Panel}, \code{Panel_Version}, \code{PlateID},
#' \code{QC_Warning}, \code{SampleType}, \code{Subject}, and
#' \code{Condition}.
#'
#' @seealso [check_npx()], [clean_npx()], [olink_ttest()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # From dataProcess()$ProteinLevelData
#'   npx <- MSstatstoNPX(processed_data$ProteinLevelData)
#'
#'   # From a CSV written by MSstats
#'   npx <- MSstatstoNPX("processed_data_protein_level.csv", check = TRUE)
#' }
#'
MSstatstoNPX <- function(data,
                         panel = "MSstats/DIANN protein",
                         panel_version = "none",
                         check = FALSE,
                         clean = FALSE) {

  check_is_scalar_character(x = panel, error = TRUE)
  check_is_scalar_character(x = panel_version, error = TRUE)
  check_is_scalar_boolean(x = check, error = TRUE)
  check_is_scalar_boolean(x = clean, error = TRUE)

  protein_tbl <- _msstats_read_protein_level(data = data)
  protein_tbl <- _msstats_harmonize_protein_level(df = protein_tbl)

  protein_ids <- protein_tbl |>
    dplyr::distinct(.data[["protein"]]) |>
    dplyr::arrange(.data[["protein"]]) |>
    dplyr::mutate(
      OlinkID = sprintf("OID%05d", dplyr::row_number())
    )

  missing_freq <- protein_tbl |>
    dplyr::group_by(.data[["protein"]]) |>
    dplyr::summarise(
      MissingFreq = mean(is.na(.data[["log_intensities"]])),
      .groups = "drop"
    )

  has_more50 <- "more50missing" %in% colnames(protein_tbl)

  npx_tbl <- protein_tbl |>
    dplyr::left_join(protein_ids, by = "protein") |>
    dplyr::left_join(missing_freq, by = "protein") |>
    dplyr::mutate(
      SampleID = as.character(.data[["original_run"]]),
      UniProt = as.character(.data[["protein"]]),
      Assay = as.character(.data[["protein"]]),
      NPX = .data[["log_intensities"]],
      Condition = .data[["group"]],
      Panel = panel,
      Panel_Version = panel_version,
      PlateID = .data[["group"]],
      SampleType = "SAMPLE",
      Subject = .data[["subject"]],
      warn_missing = is.na(.data[["NPX"]]) |
        if (has_more50) {
          dplyr::coalesce(as.logical(.data[["more50missing"]]), FALSE)
        } else {
          FALSE
        },
      QC_Warning = dplyr::if_else(.data[["warn_missing"]], "Warning", "Pass")
    ) |>
    dplyr::select(
      "SampleID",
      "OlinkID",
      "UniProt",
      "Assay",
      "NPX",
      "MissingFreq",
      "Panel",
      "Panel_Version",
      "PlateID",
      "QC_Warning",
      "SampleType",
      "Subject",
      "Condition"
    ) |>
    dplyr::arrange(.data[["SampleID"]], .data[["OlinkID"]])

  npx_tbl <- tibble::as_tibble(npx_tbl)

  if (isTRUE(check)) {
    check_npx(df = npx_tbl)
  }

  if (isTRUE(clean)) {
    npx_tbl <- npx_tbl |>
      dplyr::filter(is.finite(.data[["NPX"]])) |>
      clean_npx(
        remove_invalid_oid = FALSE,
        remove_qc_warning = FALSE,
        remove_control_assay = FALSE,
        remove_control_sample = FALSE,
        verbose = FALSE
      )
  }

  npx_tbl
}

#' Read MSstats protein-level table from data frame or file path
#'
#' @param data Data frame / tibble or character file path
#' @return Tibble
#' @keywords internal
#' @noRd
#'
_msstats_read_protein_level <- function(data) {
  if (is.character(data)) {
    check_is_scalar_character(x = data, error = TRUE)
    if (!file.exists(data)) {
      cli::cli_abort(
        c(
          "x" = "Protein-level file not found: {.file {data}}.",
          "i" = "Pass a data frame or a valid CSV/TSV path."
        )
      )
    }
    ext <- tolower(tools::file_ext(data))
    if (ext %in% c("tsv", "txt")) {
      protein_tbl <- utils::read.delim(
        file = data,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      protein_tbl <- utils::read.csv(
        file = data,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    return(tibble::as_tibble(protein_tbl))
  }

  if (!inherits(data, c("data.frame", "tbl_df", "tbl"))) {
    cli::cli_abort(
      c(
        "x" = "{.arg data} must be a data.frame/tibble or a file path.",
        "i" = "Got class {.cls {class(data)}}."
      )
    )
  }

  tibble::as_tibble(data)
}

#' Harmonize MSstats protein-level column names and required fields
#'
#' @param df Protein-level tibble
#' @return Tibble with snake_case names and required columns present
#' @keywords internal
#' @noRd
#'
_msstats_harmonize_protein_level <- function(df) {
  names(df) <- _msstats_clean_colnames(names(df))

  required_cols <- c("protein", "log_intensities")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Missing required column{?s}: {.val {missing}}.",
        "i" = "Need Protein and LogIntensities (any case / snake_case).",
        "i" = "Available columns: {.val {names(df)}}."
      )
    )
  }

  if (!"original_run" %in% names(df)) {
    if ("run" %in% names(df)) {
      df <- df |>
        dplyr::mutate(original_run = as.character(.data[["run"]]))
    } else {
      cli::cli_abort(
        c(
          "x" = "No sample identifier column found.",
          "i" = "Expected {.val originalRUN} or {.val RUN}."
        )
      )
    }
  }

  if ("group" %in% names(df)) {
    df <- df |>
      dplyr::mutate(group = as.character(.data[["group"]]))
  } else {
    df <- df |>
      dplyr::mutate(group = NA_character_)
  }

  if ("subject" %in% names(df)) {
    df <- df |>
      dplyr::mutate(subject = as.character(.data[["subject"]]))
  } else {
    df <- df |>
      dplyr::mutate(subject = NA_character_)
  }

  df |>
    dplyr::mutate(
      protein = as.character(.data[["protein"]]),
      log_intensities = as.numeric(.data[["log_intensities"]]),
      original_run = as.character(.data[["original_run"]])
    )
}

#' Snake-case column names (janitor::clean_names-like, no extra dependency)
#'
#' @param nms Character vector of names
#' @return Character vector
#' @keywords internal
#' @noRd
#'
_msstats_clean_colnames <- function(nms) {
  nms <- as.character(nms)
  # Insert underscores at camelCase / digit boundaries (janitor-like)
  nms <- gsub(
    pattern = "([a-z0-9])([A-Z])",
    replacement = "\\1_\\2",
    x = nms
  )
  nms <- gsub(
    pattern = "([A-Z]+)([A-Z][a-z])",
    replacement = "\\1_\\2",
    x = nms
  )
  nms <- tolower(nms)
  nms <- gsub(pattern = "[^a-z0-9]+", replacement = "_", x = nms)
  nms <- gsub(pattern = "^_|_$", replacement = "", x = nms)
  nms <- gsub(pattern = "_+", replacement = "_", x = nms)
  make.unique(nms, sep = "_")
}
