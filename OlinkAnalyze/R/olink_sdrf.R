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
#' \code{base → sample-metadata → affinity-proteomics}
#' (`data-raw/sdrf/*.yaml`). Required SDRF headers are those with
#' \code{col_requirement == "required"} in \var{sdrf_column_dict}.
#'
#' This is a first working draft: it writes the **required** affinity-proteomics
#' columns (plus a few recommended fields when available) with Olink-oriented
#' defaults. Ontology-perfect annotations are not guaranteed; use
#' [olink_sdrf_validate()] to check the result.
#'
#' Columns typically written (official SDRF headers; Olink-oriented examples):
#' \tabular{lll}{
#'   Header \tab Requirement \tab Olink example values \cr
#'   \code{source name} / \code{assay name} \tab required \tab
#'     SampleID from NPX \cr
#'   \code{characteristics[sample type]} \tab required \tab
#'     study sample; plate control \cr
#'   \code{characteristics[organism]} \tab required \tab homo sapiens \cr
#'   \code{characteristics[organism part]} \tab required \tab blood \cr
#'   \code{characteristics[biological replicate]} \tab required \tab 1; 2 \cr
#'   \code{comment[platform]} \tab required \tab
#'     Olink Target 96; Olink Explore HT; Olink Explore 3072 \cr
#'   \code{comment[panel name]} \tab recommended \tab
#'     Inflammation; Cardiometabolic; Explore 1536 \cr
#'   \code{comment[quantification unit]} \tab optional \tab NPX \cr
#'   \code{comment[data file]} \tab required \tab path/name of NPX export
#' }
#' Control-like SampleIDs matching \code{CONTROL|CTRL} (any case) are labeled
#' \code{plate control}; others default to \code{study sample}.
#'
#' @param npx Tibble/data frame in long format such as produced by
#' [read_npx()]. Must contain a sample identifier column matching one of the
#' names registered under \code{sample_id} in \var{column_name_dict}.
#' @param sample_metadata Optional tibble/data frame of sample-level metadata
#' (one row per sample), joinable to \code{npx} on the sample identifier.
#' If \code{NULL}, one row per distinct sample in \code{npx} is used.
#' @param file Optional path to write the SDRF table (TSV). If \code{NULL},
#' only the tibble is returned.
#' @param data_file Value for \code{comment[data file]} (recycled). Default
#' \code{"npx_data.tsv"}.
#' @param platform Value for \code{comment[platform]}. Olink examples:
#' \code{"Olink Target 96"}, \code{"Olink Explore HT"},
#' \code{"Olink Explore 3072"}. Default \code{"Olink Target 96"}.
#' @param organism Value for \code{characteristics[organism]}. Default
#' \code{"homo sapiens"} (typical for Olink clinical studies).
#' @param organism_part Value for \code{characteristics[organism part]}.
#' Default \code{"blood"} (common Olink sample type; override for e.g. CSF).
#' @param validate Logical. If \code{TRUE} (default) and \code{file} is set,
#' run [olink_sdrf_validate()] on the written file via `sdrf-pipelines`.
#' Ignored when \code{file} is \code{NULL}.
#' @param validate_error Logical passed to [olink_sdrf_validate()] as
#' \code{error}. Default \code{FALSE} so a failed validation still returns
#' the SDRF tibble (check \code{attr(x, "sdrf_validation")}).
#' @param skip_ontology Logical passed to [olink_sdrf_validate()].
#' Default \code{TRUE}.
#'
#' @return A tibble whose column names are official SDRF headers (required
#' columns always present; selected recommended columns when mappable).
#' When \code{validate = TRUE} and a file was written, the validation list from
#' [olink_sdrf_validate()] is attached as attribute \code{sdrf_validation}.
#'
#' @seealso
#' \code{column_name_dict}, \code{sdrf_column_dict},
#' [generate_sdrf()], [olink_sdrf_validate()], [read_npx()],
#' [olink_plate_randomizer()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   meta <- dplyr::distinct(
#'     OlinkAnalyze::npx_data1,
#'     SampleID, Subject, Treatment, Site, Time, Project
#'   )
#'   sdrf <- olink_sdrf(
#'     npx = OlinkAnalyze::npx_data1,
#'     sample_metadata = meta,
#'     platform = "Olink Target 96",
#'     organism = "homo sapiens",
#'     organism_part = "blood",
#'     file = "smoke_olink.sdrf.tsv"
#'   )
#'   # SDRF headers follow affinity-proteomics.yaml, e.g.:
#'   # characteristics[sample type], comment[platform], comment[panel name]
#'   attr(sdrf, "sdrf_validation")$ok
#' }
#'
olink_sdrf <- function(npx,
                       sample_metadata = NULL,
                       file = NULL,
                       data_file = "npx_data.tsv",
                       platform = "Olink Target 96",
                       organism = "homo sapiens",
                       organism_part = "blood",
                       validate = TRUE,
                       validate_error = FALSE,
                       skip_ontology = TRUE) {

  # ---- Column dictionaries (internal sysdata) --------------------------------
  npx_sample_id_names <- get_alt_colnames(col_key = "sample_id")
  npx_plate_id_names <- get_alt_colnames(col_key = "plate_id")
  npx_panel_names <- get_alt_colnames(col_key = "panel")

  sdrf_official_names <- vapply(
    X = sdrf_column_dict[["col_names"]],
    FUN = function(nm) as.character(nm[[1L]]),
    FUN.VALUE = character(1L),
    USE.NAMES = TRUE
  )
  names(sdrf_official_names) <- sdrf_column_dict[["col_key"]]

  # ---- resolve SampleID column in npx ----------------------------------------
  check_is_tibble_or_df <- function(x, arg) {
    if (!inherits(x, c("data.frame", "tbl_df", "tbl"))) {
      cli::cli_abort("{.arg {arg}} must be a data.frame or tibble.")
    }
    return(invisible(NULL))
  }
  check_is_tibble_or_df(npx, "npx")
  if (!is.null(sample_metadata)) {
    check_is_tibble_or_df(sample_metadata, "sample_metadata")
  }
  check_is_scalar_character(x = data_file, error = TRUE)
  check_is_scalar_character(x = platform, error = TRUE)
  check_is_scalar_character(x = organism, error = TRUE)
  check_is_scalar_character(x = organism_part, error = TRUE)
  check_is_scalar_boolean(x = validate, error = TRUE)
  check_is_scalar_boolean(x = validate_error, error = TRUE)
  check_is_scalar_boolean(x = skip_ontology, error = TRUE)
  if (!is.null(file)) {
    check_is_scalar_character(x = file, error = TRUE)
  }

  npx_sid <- intersect(npx_sample_id_names, colnames(npx))
  if (length(npx_sid) == 0L) {
    cli::cli_abort(
      c(
        "x" = "No sample ID column found in {.arg npx}.",
        "i" = "Expected one of {.val {npx_sample_id_names}}."
      )
    )
  }
  npx_sid <- npx_sid[[1L]]

  samples <- npx |>
    dplyr::distinct(.data[[npx_sid]]) |>
    dplyr::rename(SampleID = !!rlang::sym(npx_sid))

  # optional plate / panel from NPX (one value per sample; first if multiple)
  plate_col <- intersect(npx_plate_id_names, colnames(npx))
  panel_col <- intersect(npx_panel_names, colnames(npx))
  extras <- npx |>
    dplyr::group_by(.data[[npx_sid]]) |>
    dplyr::summarise(
      .plate = if (length(plate_col) > 0L) {
        as.character(dplyr::first(.data[[plate_col[[1L]]]]))
      } else {
        NA_character_
      },
      .panel = if (length(panel_col) > 0L) {
        as.character(dplyr::first(.data[[panel_col[[1L]]]]))
      } else {
        NA_character_
      },
      .groups = "drop"
    ) |>
    dplyr::rename(SampleID = !!rlang::sym(npx_sid))

  samples <- samples |>
    dplyr::left_join(extras, by = "SampleID")

  # ---- join sample metadata --------------------------------------------------
  if (!is.null(sample_metadata)) {
    meta_sid <- intersect(npx_sample_id_names, colnames(sample_metadata))
    if (length(meta_sid) == 0L) {
      cli::cli_abort(
        c(
          "x" = "No sample ID column found in {.arg sample_metadata}.",
          "i" = "Expected one of {.val {npx_sample_id_names}}."
        )
      )
    }
    meta_sid <- meta_sid[[1L]]
    meta <- sample_metadata |>
      dplyr::mutate(SampleID = as.character(.data[[meta_sid]])) |>
      dplyr::distinct(.data[["SampleID"]], .keep_all = TRUE)

    n_overlap <- length(intersect(samples$SampleID, meta$SampleID))
    if (n_overlap == 0L) {
      cli::cli_warn(
        c(
          "!" = paste(
            "No overlapping SampleID between {.arg npx} and",
            "{.arg sample_metadata}; metadata columns will be ignored."
          )
        )
      )
    } else {
      samples <- samples |>
        dplyr::left_join(meta, by = "SampleID", suffix = c("", "_meta"))
    }
  }

  # ---- map to required (+ light recommended) SDRF columns --------------------
  tech <- sdrf_affinity_template[["technology_type_olink"]]
  if (is.null(tech) || !nzchar(tech)) {
    tech <- "protein expression profiling by antibody array"
  }
  quant_unit <- sdrf_affinity_template[["quantification_unit_olink"]]
  if (is.null(quant_unit) || !nzchar(quant_unit)) {
    quant_unit <- "NPX"
  }
  tmpl_ver <- sdrf_affinity_template[["version"]]
  if (is.null(tmpl_ver) || !nzchar(tmpl_ver)) {
    tmpl_ver <- "1.0.0"
  }

  is_control <- grepl(
    pattern = "CONTROL|control|CTRL|ctrl",
    x = samples$SampleID
  )

  extra_chars <- sdrf_chars_from_meta(
    metadata = samples,
    skip_cols = c("SampleID", ".panel", ".plate")
  )

  # Column order matters for sdrf-pipelines: all characteristics[*] before
  # "assay name", then technology / comment columns.
  sdrf <- dplyr::bind_cols(
    tibble::tibble(
      `source name` = samples$SampleID,
      `characteristics[sample type]` = dplyr::if_else(
        is_control,
        "plate control",
        "study sample"
      ),
      `characteristics[organism]` = organism,
      `characteristics[organism part]` = organism_part,
      `characteristics[biological replicate]` = 1L
    ),
    extra_chars,
    tibble::tibble(
      `assay name` = samples$SampleID,
      `technology type` = tech,
      `comment[technical replicate]` = 1L,
      `comment[data file]` = data_file,
      `comment[platform]` = platform,
      `comment[panel name]` = samples$.panel,
      `comment[plate]` = samples$.plate,
      `comment[quantification unit]` = quant_unit,
      `comment[sdrf version]` = paste0("v", tmpl_ver),
      `comment[sdrf template]` = paste(
        "affinity-proteomics",
        paste0("v", tmpl_ver)
      )
    )
  )

  # drop all-NA recommended extras
  for (col in c("comment[panel name]", "comment[plate]")) {
    if (all(is.na(sdrf[[col]]))) {
      sdrf[[col]] <- NULL
    }
  }

  # silence unused binding (kept for future richer mapping)
  force(sdrf_official_names)

  return(
    sdrf_write_and_validate(
      sdrf = sdrf,
      file = file,
      validate = validate,
      validate_error = validate_error,
      skip_ontology = skip_ontology
    )
  )
}

#' Validate an SDRF file with sdrf-pipelines
#'
#' @description
#' Soft-dependency wrapper around the
#' [sdrf-pipelines](https://github.com/bigbio/sdrf-pipelines)
#' CLI (`parse_sdrf validate-sdrf`). Use this to check that an SDRF TSV
#' produced by [olink_sdrf()] / [generate_sdrf()] (or elsewhere) matches
#' the official affinity-proteomics template
#' (`data-raw/sdrf/affinity-proteomics.yaml`).
#'
#' Requires Python with `sdrf-pipelines` installed and `parse_sdrf` on
#' `PATH`, for example:
#' \preformatted{pip install "sdrf-pipelines[ontology]"}
#'
#' For Olink files, validation typically covers headers such as
#' \code{characteristics[sample type]} (\code{study sample},
#' \code{plate control}), \code{comment[platform]}
#' (\code{Olink Target 96}, \code{Olink Explore HT}), and
#' \code{comment[panel name]} (e.g. Inflammation).
#'
#' @param sdrf_file Path to an SDRF TSV file (typically `*.sdrf.tsv`).
#' @param template SDRF template name passed to `--template`. Default
#'   \code{"affinity-proteomics"} (used for Olink PEA exports), matching
#'   `data-raw/sdrf/affinity-proteomics.yaml`.
#' @param skip_ontology Logical. If \code{TRUE} (default), pass
#'   `--skip-ontology` for structural validation without OLS ontology
#'   lookups. Set \code{FALSE} when ontology extras are installed and
#'   network/cache access is available.
#' @param use_ols_cache_only Logical. If \code{TRUE}, pass
#'   `--use_ols_cache_only` (only meaningful when \code{skip_ontology}
#'   is \code{FALSE}).
#' @param out Optional path for validation output (`--out`). If
#'   \code{NULL}, results stay in the returned object.
#' @param parse_sdrf Optional path/name of the `parse_sdrf` executable.
#'   If \code{NULL}, resolved via [Sys.which()].
#' @param error Logical. If \code{TRUE} (default), abort when validation
#'   fails or `parse_sdrf` is missing. If \code{FALSE} and `parse_sdrf`
#'   is missing, emit a warning that validation was skipped and return
#'   the result list with \code{ok = FALSE} and \code{skipped = TRUE}.
#'
#' @return A named list with:
#' \itemize{
#'   \item{\code{ok}:} Logical; \code{TRUE} if exit status is 0
#'   \item{\code{status}:} Integer exit status from `parse_sdrf`
#'     (\code{NA_integer_} when skipped)
#'   \item{\code{skipped}:} Logical; \code{TRUE} when validation was
#'     skipped because `parse_sdrf` / sdrf-pipelines was not found
#'   \item{\code{command}:} Character vector of the invoked command
#'   \item{\code{stdout}:} Captured standard output lines
#'   \item{\code{stderr}:} Captured standard error lines (may be empty)
#'   \item{\code{sdrf_file}:} Absolute path validated
#'   \item{\code{template}:} Template name used
#' }
#'
#' @seealso
#' [olink_sdrf()], [generate_sdrf()],
#' [sdrf-pipelines on GitHub](https://github.com/bigbio/sdrf-pipelines),
#' [SDRF tools](https://sdrf.quantms.org/tools.html)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   res <- olink_sdrf_validate(
#'     sdrf_file = "my_study.sdrf.tsv",
#'     template = "affinity-proteomics",
#'     skip_ontology = TRUE
#'   )
#'   res$ok
#' }
#'
olink_sdrf_validate <- function(sdrf_file,
                                template = "affinity-proteomics",
                                skip_ontology = TRUE,
                                use_ols_cache_only = FALSE,
                                out = NULL,
                                parse_sdrf = NULL,
                                error = TRUE) {

  # ---- input checks ----------------------------------------------------------
  check_is_scalar_character(x = sdrf_file, error = TRUE)
  check_file_exists(file = sdrf_file, error = TRUE)
  check_is_scalar_character(x = template, error = TRUE)
  check_is_scalar_boolean(x = skip_ontology, error = TRUE)
  check_is_scalar_boolean(x = use_ols_cache_only, error = TRUE)
  check_is_scalar_boolean(x = error, error = TRUE)
  if (!is.null(out)) {
    check_is_scalar_character(x = out, error = TRUE)
  }

  sdrf_file <- normalizePath(sdrf_file, winslash = "/", mustWork = TRUE)

  # ---- locate parse_sdrf -----------------------------------------------------
  exe <- find_parse_sdrf(parse_sdrf = parse_sdrf, error = error)
  if (is.null(exe)) {
    msg <- paste(
      "parse_sdrf not found on PATH.",
      "Install with: pip install \"sdrf-pipelines[ontology]\""
    )
    cli::cli_warn(
      c(
        "!" = paste(
          "SDRF validation skipped: {.cmd parse_sdrf}",
          "(from sdrf-pipelines) is not installed or not on",
          "{.envvar PATH}."
        ),
        "i" = "Install with: {.code pip install \"sdrf-pipelines[ontology]\"}",
        "i" = "Or pass {.arg parse_sdrf} with the full path to the executable."
      )
    )
    return(
      list(
        ok = FALSE,
        status = NA_integer_,
        skipped = TRUE,
        command = character(),
        stdout = character(),
        stderr = msg,
        sdrf_file = sdrf_file,
        template = template
      )
    )
  }

  # ---- build CLI args --------------------------------------------------------
  args <- c(
    "validate-sdrf",
    "--sdrf_file", sdrf_file,
    "--template", template
  )
  if (isTRUE(skip_ontology)) {
    args <- c(args, "--skip-ontology")
  }
  if (isTRUE(use_ols_cache_only)) {
    args <- c(args, "--use_ols_cache_only")
  }
  if (!is.null(out)) {
    args <- c(args, "--out", out)
  }

  command <- c(exe, args)

  # ---- run -------------------------------------------------------------------
  # Capture both streams; on many platforms stderr is merged when stderr = TRUE
  # is passed together with stdout = TRUE in system2.
  output <- system2(
    command = exe,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  } else {
    status <- as.integer(status)
  }

  result <- list(
    ok = identical(status, 0L),
    status = status,
    skipped = FALSE,
    command = command,
    stdout = as.character(output),
    stderr = character(),
    sdrf_file = sdrf_file,
    template = template
  )

  if (!result$ok && isTRUE(error)) {
    cli::cli_abort(
      c(
        "x" = "{.cmd parse_sdrf validate-sdrf} failed (status {status}).",
        "i" = "File: {.file {sdrf_file}}",
        "i" = "Template: {.val {template}}",
        "i" = "Command: {.code {paste(command, collapse = \" \")}}",
        "i" = "Output: {paste(result$stdout, collapse = \"\\n\")}"
      )
    )
  }

  if (result$ok) {
    cli::cli_alert_success(
      paste(
        "SDRF validation passed for {.file {sdrf_file}}",
        "(template {.val {template}})."
      )
    )
  }

  return(result)
}

#' Locate the parse_sdrf executable from sdrf-pipelines
#'
#' @param parse_sdrf Optional explicit path or command name.
#' @param error If \code{TRUE}, abort when not found; else return \code{NULL}.
#' @return Character path/command, or \code{NULL} when missing and
#'   \code{error = FALSE}.
#' @keywords internal
#' @noRd
#'
find_parse_sdrf <- function(parse_sdrf = NULL, error = TRUE) {
  check_is_scalar_boolean(x = error, error = TRUE)

  if (!is.null(parse_sdrf)) {
    check_is_scalar_character(x = parse_sdrf, error = TRUE)
    if (file.exists(parse_sdrf) || nzchar(Sys.which(parse_sdrf))) {
      if (file.exists(parse_sdrf)) {
        return(normalizePath(parse_sdrf, winslash = "/", mustWork = TRUE))
      }
      return(Sys.which(parse_sdrf))
    }
    if (isTRUE(error)) {
      cli::cli_abort(
        c(
          "x" = "Cannot find {.arg parse_sdrf} at {.path {parse_sdrf}}.",
          "i" = "Install with: {.code pip install \"sdrf-pipelines[ontology]\"}"
        )
      )
    }
    return(NULL)
  }

  exe <- Sys.which("parse_sdrf")
  if (!nzchar(exe)) {
    if (isTRUE(error)) {
      cli::cli_abort(
        c(
          "x" = "{.cmd parse_sdrf} not found on {.envvar PATH}.",
          "i" = "Install sdrf-pipelines, then retry:",
          " " = "{.code pip install \"sdrf-pipelines[ontology]\"}",
          "i" = paste(
            "Or pass {.arg parse_sdrf} with the full path",
            "to the executable."
          )
        )
      )
    }
    return(NULL)
  }

  return(exe)
}

#' Generate an Olink affinity-proteomics SDRF from a metadata table
#'
#' @description
#' Builds an SDRF-Proteomics **affinity-proteomics** table from a sample
#' metadata data frame / tibble (for example the package \code{manifest}
#' dataset), without requiring NPX assay data. Output headers follow
#' `data-raw/sdrf/affinity-proteomics.yaml` (merged with base /
#' sample-metadata).
#'
#' Defaults assume **human blood** samples run on an Olink platform. Choose
#' the platform with \code{platform} (see [get_all_olink_platforms()]) and
#' optionally set \code{panel_name} (constant or a column in \code{metadata}).
#'
#' @details
#' Same core affinity columns as [olink_sdrf()]. Olink-oriented examples:
#' \itemize{
#'   \item \code{characteristics[sample type]}:
#'     study sample; plate control
#'   \item \code{comment[platform]}:
#'     Olink Target 96; Olink Explore HT; Olink Explore 3072
#'   \item \code{comment[panel name]}:
#'     Inflammation; Cardiometabolic; Explore 1536
#'   \item \code{characteristics[organism]} / \code{[organism part]}:
#'     homo sapiens / blood (defaults)
#'   \item \code{comment[quantification unit]}: NPX
#' }
#' Pass \code{sample_id} when the metadata ID column is not named
#' \code{SampleID}. Any other metadata columns (e.g. \code{Visit}, \code{Site},
#' \code{SubjectID}) are added as \code{characteristics[<column>]}.
#'
#' @param metadata Data frame / tibble with one row per sample. Must contain a
#' sample identifier column (see \code{sample_id}). Remaining columns become
#' \code{characteristics[...]} fields.
#' @param sample_id Name of the sample ID column in \code{metadata} used for
#' \code{source name} / \code{assay name}. Default \code{"SampleID"}.
#' @param file Optional path to write the SDRF TSV. If \code{NULL}, only the
#' tibble is returned.
#' @param organism Value for \code{characteristics[organism]}. Default
#' \code{"homo sapiens"}.
#' @param organism_part Value for \code{characteristics[organism part]}.
#' Default \code{"blood"}.
#' @param platform Olink platform for \code{comment[platform]}. Accepts names
#' from [get_all_olink_platforms()] (e.g. \code{"Target 96"},
#' \code{"Explore HT"}) or full names (e.g. \code{"Olink Target 96"},
#' \code{"Olink Explore HT"}). Default \code{"Target 96"}.
#' @param panel_name Optional value for \code{comment[panel name]}. Either a
#' single string (recycled) or the name of a column in \code{metadata}.
#' Olink examples: \code{"Inflammation"}, \code{"Cardiometabolic"},
#' \code{"Explore 1536"}.
#' @param data_file Value for \code{comment[data file]} (recycled).
#' Default \code{"npx_data.tsv"}.
#' @param validate,validate_error,skip_ontology See [olink_sdrf()].
#'
#' @return A tibble of SDRF columns. If \code{validate = TRUE} and \code{file}
#' is set, validation output is attached as \code{attr(x, "sdrf_validation")}.
#'
#' @seealso [olink_sdrf()], [olink_sdrf_validate()], [get_all_olink_platforms()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   sdrf <- generate_sdrf(
#'     metadata = manifest,
#'     sample_id = "SampleID",
#'     platform = "Target 96",
#'     panel_name = "Inflammation",
#'     organism = "homo sapiens",
#'     organism_part = "blood",
#'     file = "manifest_olink.sdrf.tsv"
#'   )
#' }
#'
generate_sdrf <- function(metadata,
                          sample_id = "SampleID",
                          file = NULL,
                          organism = "homo sapiens",
                          organism_part = "blood",
                          platform = "Target 96",
                          panel_name = NULL,
                          data_file = "npx_data.tsv",
                          validate = TRUE,
                          validate_error = FALSE,
                          skip_ontology = TRUE) {

  if (!inherits(metadata, c("data.frame", "tbl_df", "tbl"))) {
    cli::cli_abort("{.arg metadata} must be a data.frame or tibble.")
  }
  check_is_scalar_character(x = sample_id, error = TRUE)
  check_is_scalar_character(x = organism, error = TRUE)
  check_is_scalar_character(x = organism_part, error = TRUE)
  check_is_scalar_character(x = platform, error = TRUE)
  check_is_scalar_character(x = data_file, error = TRUE)
  check_is_scalar_boolean(x = validate, error = TRUE)
  check_is_scalar_boolean(x = validate_error, error = TRUE)
  check_is_scalar_boolean(x = skip_ontology, error = TRUE)
  if (!is.null(file)) {
    check_is_scalar_character(x = file, error = TRUE)
  }
  if (!sample_id %in% colnames(metadata)) {
    cli::cli_abort(
      c(
        "x" = "Column {.val {sample_id}} not found in {.arg metadata}.",
        "i" = "Available columns: {.val {colnames(metadata)}}."
      )
    )
  }

  samples <- metadata |>
    dplyr::mutate(
      SampleID = as.character(.data[[sample_id]])
    ) |>
    dplyr::filter(!is.na(.data[["SampleID"]]), .data[["SampleID"]] != "") |>
    dplyr::distinct(.data[["SampleID"]], .keep_all = TRUE)

  if (nrow(samples) == 0L) {
    cli::cli_abort("No non-missing sample IDs found in {.arg metadata}.")
  }

  platform_sdrf <- resolve_olink_sdrf_platform(platform = platform)
  panel_values <- resolve_olink_sdrf_panel_name(
    panel_name = panel_name,
    metadata = samples,
    n = nrow(samples)
  )

  tech <- sdrf_affinity_template[["technology_type_olink"]]
  if (is.null(tech) || !nzchar(tech)) {
    tech <- "protein expression profiling by antibody array"
  }
  quant_unit <- sdrf_affinity_template[["quantification_unit_olink"]]
  if (is.null(quant_unit) || !nzchar(quant_unit)) {
    quant_unit <- "NPX"
  }
  tmpl_ver <- sdrf_affinity_template[["version"]]
  if (is.null(tmpl_ver) || !nzchar(tmpl_ver)) {
    tmpl_ver <- "1.0.0"
  }

  is_control <- grepl(
    pattern = "CONTROL|control|CTRL|ctrl",
    x = samples$SampleID
  )

  # biological replicate: prefer SubjectID / Subject if present, else 1
  bio_rep <- rep(1L, nrow(samples))
  subj_col <- intersect(
    c("SubjectID", "Subject", "subject_id"),
    colnames(samples)
  )
  if (length(subj_col) > 0L) {
    bio_rep <- as.integer(factor(samples[[subj_col[[1L]]]]))
  }

  skip_meta_cols <- c("SampleID", sample_id)
  if (!is.null(panel_name) && panel_name %in% colnames(samples)) {
    skip_meta_cols <- c(skip_meta_cols, panel_name)
  }
  extra_chars <- sdrf_chars_from_meta(
    metadata = samples,
    skip_cols = skip_meta_cols
  )

  # Column order: required characteristics, then metadata characteristics,
  # then assay name / technology / comments (sdrf-pipelines rule).
  sdrf <- dplyr::bind_cols(
    tibble::tibble(
      `source name` = samples$SampleID,
      `characteristics[sample type]` = dplyr::if_else(
        is_control,
        "plate control",
        "study sample"
      ),
      `characteristics[organism]` = organism,
      `characteristics[organism part]` = organism_part,
      `characteristics[biological replicate]` = bio_rep
    ),
    extra_chars,
    tibble::tibble(
      `assay name` = samples$SampleID,
      `technology type` = tech,
      `comment[technical replicate]` = 1L,
      `comment[data file]` = data_file,
      `comment[platform]` = platform_sdrf,
      `comment[panel name]` = panel_values,
      `comment[quantification unit]` = quant_unit,
      `comment[sdrf version]` = paste0("v", tmpl_ver),
      `comment[sdrf template]` = paste(
        "affinity-proteomics",
        paste0("v", tmpl_ver)
      )
    )
  )

  if (all(is.na(sdrf[["comment[panel name]"]]))) {
    sdrf[["comment[panel name]"]] <- NULL
  }

  return(
    sdrf_write_and_validate(
      sdrf = sdrf,
      file = file,
      validate = validate,
      validate_error = validate_error,
      skip_ontology = skip_ontology
    )
  )
}

#' Map a package / SDRF Olink platform name to comment platform field
#'
#' @param platform Character scalar
#' @return Full SDRF-style platform string, e.g. "Olink Target 96"
#' @keywords internal
#' @noRd
#'
resolve_olink_sdrf_platform <- function(platform) {
  plats <- accepted_olink_platforms
  allowed_short <- plats[["name"]]
  allowed_full <- plats[["full_name"]]

  if (platform %in% allowed_full) {
    return(platform)
  }
  if (platform %in% allowed_short) {
    return(allowed_full[[match(platform, allowed_short)]])
  }
  # allow "Olink ExploreHT" style typos? no — be strict
  cli::cli_abort(
    c(
      "x" = "Unknown Olink {.arg platform}: {.val {platform}}.",
      "i" = "Use one of {.val {allowed_short}}",
      "i" = "or full names {.val {allowed_full}}."
    )
  )
  return(invisible(NULL))
}

#' Resolve comment panel name from a constant or metadata column
#'
#' @param panel_name NULL, scalar string, or column name
#' @param metadata Sample table
#' @param n Number of rows expected
#' @return Character vector of length n (may be all NA)
#' @keywords internal
#' @noRd
#'
resolve_olink_sdrf_panel_name <- function(panel_name, metadata, n) {
  if (is.null(panel_name)) {
    return(rep(NA_character_, n))
  }
  check_is_scalar_character(x = panel_name, error = TRUE)
  if (panel_name %in% colnames(metadata)) {
    return(as.character(metadata[[panel_name]]))
  }
  return(rep(panel_name, n))
}

#' Map leftover metadata columns to characteristics[col] SDRF headers
#'
#' @param metadata Sample-level data frame
#' @param skip_cols Column names already consumed (e.g. SampleID, panel col)
#' @return Tibble of characteristics columns (0 columns if none)
#' @keywords internal
#' @noRd
#'
sdrf_chars_from_meta <- function(metadata, skip_cols = character()) {
  extra <- setdiff(colnames(metadata), unique(c(skip_cols, "SampleID")))
  # drop internal helper columns
  extra <- extra[!startsWith(extra, ".")]
  if (length(extra) == 0L) {
    return(tibble::tibble(.rows = nrow(metadata)))
  }

  out <- metadata[, extra, drop = FALSE]
  out[] <- lapply(
    X = out,
    FUN = function(x) {
      return(ifelse(is.na(x), NA_character_, as.character(x)))
    }
  )

  already_sdrf <- grepl(
    pattern = "^(characteristics|comment|factor value)\\[.+\\]$",
    x = names(out)
  )
  names(out)[!already_sdrf] <- paste0(
    "characteristics[",
    names(out)[!already_sdrf],
    "]"
  )

  # avoid colliding with required headers we always write
  reserved <- c(
    "characteristics[sample type]",
    "characteristics[organism]",
    "characteristics[organism part]",
    "characteristics[biological replicate]"
  )
  keep <- !names(out) %in% reserved
  out <- out[, keep, drop = FALSE]

  return(tibble::as_tibble(out))
}

#' Write SDRF tibble and optionally validate
#'
#' @param sdrf Tibble
#' @param file Path or NULL
#' @param validate,validate_error,skip_ontology Logical
#' @return \code{sdrf}, possibly with \code{sdrf_validation} attribute
#' @keywords internal
#' @noRd
#'
sdrf_write_and_validate <- function(sdrf,
                                    file = NULL,
                                    validate = TRUE,
                                    validate_error = FALSE,
                                    skip_ontology = TRUE) {
  if (!is.null(file)) {
    utils::write.table(
      x = sdrf,
      file = file,
      sep = "\t",
      quote = FALSE,
      row.names = FALSE,
      col.names = TRUE,
      na = "not available"
    )
    cli::cli_alert_success(
      "Wrote SDRF ({nrow(sdrf)} row{?s}) to {.file {file}}."
    )

    if (isTRUE(validate)) {
      cli::cli_alert_info(
        "Validating written SDRF with {.fn olink_sdrf_validate} ..."
      )
      validation <- olink_sdrf_validate(
        sdrf_file = file,
        template = "affinity-proteomics",
        skip_ontology = skip_ontology,
        error = validate_error
      )
      attr(sdrf, "sdrf_validation") <- validation
      if (!isTRUE(validation$ok) && !isTRUE(validation$skipped)) {
        cli::cli_warn(
          c(
            "!" = "SDRF validation did not pass for {.file {file}}.",
            "i" = "See {.code attr(sdrf, \"sdrf_validation\")} for details."
          )
        )
      }
    }
  } else if (isTRUE(validate)) {
    cli::cli_alert_info(
      "Skipping validation: {.arg file} is NULL (nothing written to disk)."
    )
  }

  return(sdrf)
}
