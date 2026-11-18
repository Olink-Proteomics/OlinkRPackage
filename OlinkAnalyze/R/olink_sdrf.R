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
#' [olink_sdrf_validate()], [read_npx()], [olink_plate_randomizer()]
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

#' Validate an SDRF file with sdrf-pipelines
#'
#' @description
#' Soft-dependency wrapper around the
#' [sdrf-pipelines](https://github.com/bigbio/sdrf-pipelines)
#' CLI (`parse_sdrf validate-sdrf`). Use this to check that an SDRF TSV
#' produced by [olink_sdrf()] (or elsewhere) matches the official
#' affinity-proteomics template.
#'
#' Requires Python with `sdrf-pipelines` installed and `parse_sdrf` on
#' `PATH`, for example:
#' \preformatted{pip install "sdrf-pipelines[ontology]"}
#'
#' @param sdrf_file Path to an SDRF TSV file (typically `*.sdrf.tsv`).
#' @param template SDRF template name passed to `--template`. Default
#'   \code{"affinity-proteomics"} (Olink / SomaScan / NULISA).
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
#'   fails or `parse_sdrf` is missing. If \code{FALSE}, return the result
#'   list with \code{ok = FALSE}.
#'
#' @return A named list with:
#' \itemize{
#'   \item{\code{ok}:} Logical; \code{TRUE} if exit status is 0
#'   \item{\code{status}:} Integer exit status from `parse_sdrf`
#'   \item{\code{command}:} Character vector of the invoked command
#'   \item{\code{stdout}:} Captured standard output lines
#'   \item{\code{stderr}:} Captured standard error lines (may be empty)
#'   \item{\code{sdrf_file}:} Absolute path validated
#'   \item{\code{template}:} Template name used
#' }
#'
#' @seealso
#' [olink_sdrf()],
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
    return(
      list(
        ok = FALSE,
        status = NA_integer_,
        command = character(),
        stdout = character(),
        stderr = paste(
          "parse_sdrf not found on PATH.",
          "Install with: pip install \"sdrf-pipelines[ontology]\""
        ),
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
      "SDRF validation passed for {.file {sdrf_file}} (template {.val {template}})."
    )
  }

  result
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
          "i" = "Or pass {.arg parse_sdrf} with the full path to the executable."
        )
      )
    }
    return(NULL)
  }

  exe
}
