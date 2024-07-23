#' Check columns of a list of datasets to be normalized.
#'
#' @description
#' This function takes as input a named list of datasets and checks if their
#' columns allow the normalization to be performed. The input may contain
#' "tibble", "ArrowTable" or a mixture of them.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df Named list of datasets to be normalized.
#'
#' @return `NULL` if no warning or error.
#'
#' @examples
#' \donttest{
#' # One dataset
#' OlinkAnalyze:::olink_norm_check_input_cols(
#'   lst_df = list(
#'     "p1" = npx_data1
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#'
#' # Two datasets
#' OlinkAnalyze:::olink_norm_check_input_cols(
#'   lst_df = list(
#'     "p1" = npx_data1,
#'     "p2" = npx_data2
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#'
#' # Multiple datasets
#' OlinkAnalyze:::olink_norm_check_input_cols(
#'   lst_df = list(
#'     "p1" = npx_data1,
#'     "p2" = npx_data2,
#'     "p3" = npx_data1,
#'     "p4" = npx_data2
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#' }
olink_norm_check_input_cols <- function(lst_df) {
  # check required columns ----

  # this is a list of columns that are expected to be present in one or all
  # datasets, if 2 or more are provided as input. Some columns named have been
  # evolving; to handle this we have added all the possible column names
  # matching the same column as elements of a character vector. All elements of
  # the list should match at least one column name, except from "Normalization"
  # that is allowed to be missing from all.
  required_cols <- list(
    sample_id = "SampleID",
    olink_id = "OlinkID",
    uniprot = "UniProt",
    assay = "Assay",
    panel = "Panel",
    panel_version = c("Panel_Lot_Nr", "Panel_Version", "DataAnalysisRefID"),
    plate_id = "PlateID",
    qc_warn = c("QC_Warning", "SampleQC"),
    quant = c("Ct", "NPX", "Quantified_value"),
    normalization = "Normalization"
  )

  # intersect required column names with columns of df
  lst_req_col <- lapply(lst_df, function(l_df) {
    lapply(required_cols, function(r_col) r_col[r_col %in% names(l_df)])
  })

  ## normalization can be missing from both datasets ----

  # we only tolerate "Normalization" missing from all datasets, otherwise it is
  # an error
  col_norm <- lapply(lst_req_col, function(x) x$normalization) |>
    unlist()
  if (length(col_norm) != length(lst_req_col)
      && !identical(col_norm, character(0L))) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(length(lst_req_col) - length(col_norm))} Dataset{?s}
        {.val {setdiff(names(lst_req_col), names(col_norm))}} {?does/do}
        not contain a column named {.val {required_cols$normalization}}!",
        "i" = "The column should be present in all, or missing from all input
        datasets."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  } else if (identical(col_norm, character(0L))) {
    cli::cli_warn(
      c(
        "{cli::qty(names(lst_req_col))} Dataset{?s} {.val {names(lst_req_col)}}
        {cli::qty(names(lst_req_col))} {?does/do} not contain a column named
        {.val {required_cols$normalization}}."
      )
    )
  }

  ## check for missing columns ----

  # identify missing column names from the set of required_cols and prepare the
  # error to be thrown
  lst_col_miss <- lapply(lst_req_col, function(l_col) {
    lapply(l_col, function(r_col) {
      length(r_col) == 1L
    })
  }) |>
    # remove normalization as it was checked above
    lapply(function(sub_lst) {
      sub_lst[names(sub_lst) != "normalization"]
    }) |>
    # remove all elements that have no missing value
    lapply(function(sub_lst) {
      sub_lst[sub_lst == FALSE]
    }) |>
    # keep only elements with no or more than 1 matches to required_cols
    lapply(function(sub_lst) {
      required_cols[names(required_cols) %in% names(sub_lst)] |>
        # collapse columns whose names can differ in different datasets
        lapply(cli::ansi_collapse, sep = ", ", sep2 = " or ", last = " or ") |>
        # unlist for better error printing
        unlist() |>
        # collapse all missing column names for error printing
        cli::ansi_collapse(sep = "; ", sep2 = "; ", last = "; ")
    })
  lst_col_miss <- lst_col_miss[nchar(lst_col_miss) > 0L]

  # error message if there are missing columns
  if (!all(unlist(lapply(lst_col_miss, nchar)) == 0)) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_col_miss)} Dataset{?s} with missing column(s):",
        paste0("* ", names(lst_col_miss), ": ", unlist(lst_col_miss)),
        "i" = "The missing columns are separated by semicolon (;)."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  ## check that quant methods are the same ----

  quant_col <- lapply(lst_req_col, function(r_col) r_col$quant) |>
    unlist()

  # error message if not identical
  if (length(unique(quant_col)) != 1L) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(quant_col)} Dataset{?s} are not quantified with the
        same method:",
        paste0("* ", names(quant_col), ": ", quant_col),
        "i" = "Re-export data with identical quantifications."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # non-required column mismatches ----

  # this should work only if there are 2 or more datasets
  if (length(lst_df) > 1L) {

    # get non-required cols for each dataset by creating a data frame of all
    # combos of df names and non-required column names and checking if they are
    # present in all datasets.
    df_non_req_col <- tidyr::expand_grid(
      n_df = names(lst_df),
      n_col = lapply(names(lst_df), function(n_df) {
        df_cnames <- names(lst_df[[n_df]])
        df_cnames[!(df_cnames %in% lst_req_col[[n_df]])]
      }) |>
        unlist() |>
        unique()
    ) |>
      # go row by row and check if columns exists in dataset n_df
      dplyr::rowwise() |>
      dplyr::mutate(
        is_in = .data[["n_col"]] %in% names(lst_df[[.data[["n_df"]]]])
      ) |>
      dplyr::ungroup() |>
      # keep only missing ones
      dplyr::filter(
        .data[["is_in"]] == FALSE
      ) |>
      # print message
      dplyr::group_by(
        dplyr::pick(
          dplyr::all_of(
            c("n_df")
          )
        )
      ) |>
      dplyr::summarise(
        prnt_msg = cli::ansi_collapse(.data[["n_col"]]),
        .groups = "drop"
      )

    # warning message
    if (nrow(df_non_req_col) > 0L) {
      cli::cli_warn(
        c(
          "x" = "{cli::qty(df_non_req_col$n_df)} Column{?s} not present across
          datasets:",
          paste0("* ", df_non_req_col$n_df, ": ", df_non_req_col$prnt_msg),
          "i" = "Columns will be added with {.val {NA}} values."
        )
      )
    }

  }

}
