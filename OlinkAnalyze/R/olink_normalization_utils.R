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
#'
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
  if (!all(sapply(lst_col_miss, nchar) == 0L)) {
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

#' Check reference samples to be used for normalization
#'
#' @description
#' This function takes as input a two named lists of character vectors with
#' matching names and checks the validity of the reference samples. In case of 1
#' set of df samples, then all checks are skipped as reference median
#' normalization is to be performed.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df_samples Named list of all sample identifiers from datasets to
#' be normalized.
#' @param lst_ref_samples Named list of reference sample identifiers to be used
#' for normalization.
#' @param norm_mode Character string indicating the type of normalization to be
#' performed. Expecting one of
#' `r cli::ansi_collapse(x = OlinkAnalyze:::olink_norm_modes, sep2 = " or ", last = " or ")`. # nolint
#'
#' @return `NULL` if no warning or error.
#'
#' @examples
#' \donttest{
#' # Reference median normalization
#' OlinkAnalyze:::olink_norm_check_input_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID)
#'   ),
#'   norm_mode = "ref_median"
#' )
#'
#' # Bridge normalization
#' ref_samples_bridge <- intersect(x = npx_data1$SampleID,
#'                                 y = npx_data2$SampleID) |>
#'   (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()
#'
#' OlinkAnalyze:::olink_norm_check_input_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID),
#'     "p2" = unique(npx_data2$SampleID)
#'   ),
#'   lst_ref_samples = list(
#'     "p1" = ref_samples_bridge,
#'     "p2" = ref_samples_bridge
#'   ),
#'   norm_mode = "bridge"
#' )
#'
#' # Subset normalization
#' ref_samples_subset_1 <- npx_data1 |>
#'   dplyr::filter(
#'     !grepl(pattern = "CONTROL_SAMPLE",
#'            x = .data[["SampleID"]],
#'            fixed = TRUE)
#'     & .data[["QC_Warning"]] == "Pass"
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#' ref_samples_subset_2 <- npx_data2 |>
#'   dplyr::filter(
#'     !grepl(pattern = "CONTROL_SAMPLE",
#'            x = .data[["SampleID"]],
#'            fixed = TRUE)
#'     & .data[["QC_Warning"]] == "Pass"
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#'
#' OlinkAnalyze:::olink_norm_check_input_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID),
#'     "p2" = unique(npx_data2$SampleID)
#'   ),
#'   lst_ref_samples = list(
#'     "p1" = ref_samples_subset_1,
#'     "p2" = ref_samples_subset_2
#'   ),
#'   norm_mode = "subset"
#' )
#' }
#'
olink_norm_check_input_samples <- function(lst_df_samples,
                                           lst_ref_samples = NULL,
                                           norm_mode) {
  # check only if there are 2 datasets or more because if only one is provided,
  # it means that we are performing a reference median normalization and in this
  # case reference samples are not uneccessary.
  if (length(lst_df_samples) == 2L && length(lst_ref_samples) == 2L) {
    ## missing samples ----

    # find samples in lst_ref_samples that are not present in the dataset
    miss_samples <- lapply(names(lst_df_samples), function(n_df) {
      setdiff(
        x = lst_ref_samples[[n_df]],
        y = lst_df_samples[[n_df]]
      ) |>
        cli::ansi_collapse()
    }) |>
      unlist()
    names(miss_samples) <- names(lst_df_samples)
    # remove instances with no missing samples
    miss_samples <- miss_samples[nchar(miss_samples) > 0L]

    # error message if there are missing samples
    if (!all(sapply(miss_samples, nchar) == 0L)) {
      cli::cli_abort(
        c(
          "x" = "Normalization sample(s) missing from {cli::qty(miss_samples)}
        dataset{?s}:",
          paste0("* ", names(miss_samples), ": ", unlist(miss_samples)),
          "i" = "Sample identifiers are separated by comma (,)."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    ## duplicate samples ----

    # check that there are no duplicate sample identifiers within vectors of
    # lst_ref_samples
    if (lst_ref_samples |> lapply(duplicated) |> sapply(any) |> any()) {
      # get duplicated samples
      lst_sample_dups <- lst_ref_samples |>
        lapply(duplicated) |>
        sapply(any) |>
        (\(x) {
          lst_ref_samples[x] |>
            lapply(function(y) {
              y[duplicated(y)] |>
                unique() |>
                cli::ansi_collapse()
            })
        })()

      # error message for duplicated samples
      cli::cli_abort(
        c(
          "x" = "Duplicated reference sample identifier(s) detected in
          {cli::qty(lst_sample_dups)} vector{?s}:",
          paste0("* ", names(lst_sample_dups), ": ", unlist(lst_sample_dups)),
          "i" = "Expected no duplicates."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    ## equal number of bridge samples ----

    # check the number of samples is equal if bridge normalization
    if (tolower(norm_mode) == olink_norm_modes$bridge
        && sapply(lst_ref_samples, length) |> unique() |> length() != 1L) {
      # error message for uneven number of bridge samples
      cli::cli_abort(
        c(
          "x" = "There are {length(lst_ref_samples[[1L]])} bridge samples for
          dataset {.var {names(lst_ref_samples)[1L]}} and
          {length(lst_ref_samples[[2L]])} bridge samples for dataset
          {.var {names(lst_ref_samples)[2L]}}!",
          "i" = "Expected the same number of samples."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

  } else if (length(lst_df_samples) == 2L && length(lst_ref_samples) != 2L) {

    # if lst_df_samples is 2 but lst_ref_samples is anything else then
    # lst_df_samples and lst_ref_samples do not match
    cli::cli_abort(
      c(
        "x" = "Number of sample vectors in {.var lst_df_samples} differs from
        the number of reference sample vectors in {.var lst_ref_samples}!",
        "i" = "Expected equal number of vectors of samples."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (length(lst_df_samples) != 1L) {

    # if 0 or more than 2 datasets are provided
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_df_samples)} {?No/One/More than 2} set{?s} of
        samples provided in {.var lst_df_samples}!",
        "i" = "Expected 1 or 2 sets."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }
}
