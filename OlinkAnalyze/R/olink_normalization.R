#' Normalize two Olink datasets
#'
#' @description
#' Normalizes two Olink datasets to each other, or one Olink dataset to a
#' reference set of medians values.
#'
#' @details
#' The function handles three different types of normalization:
#'  - \strong{Bridge normalization}: One of the datasets is adjusted to another
#'  using overlapping samples (bridge samples). Overlapping samples need to have
#'  the same identifiers in both datasets. Normalization is performed using the
#'  median of the pair-wise differences between the bridge samples in the two
#'  datasets. The two datasets are provided as `df1` and `df2`, and the one
#'  being adjusted to is specified in the input `reference_project`; overlapping
#'  samples are specified in `overlapping_samples_df1`. Only
#'  `overlapping_samples_df1` should be provided regardless of the dataset used
#'  as `reference_project`.
#'  - \strong{Subset normalization}: One of the datasets is adjusted to another
#'  using a subset of samples from each. Normalization is performed using the
#'  differences of the medians between the subsets from the two datasets. Both
#'  `overlapping_samples_df1` and `overlapping_samples_df2` need to be provided,
#'  and sample identifiers do not need to be the same.
#'    - A special case of subset normalization occurs when all samples (except
#'    control samples and samples with QC warnings) from each dataset are used
#'    for normalization; this special case is called intensity normalization. In
#'    intensity normalization all unique sample identifiers from `df1` are
#'    provided as input in `overlapping_samples_df1` and all unique sample
#'    identifiers from `df2` are provided as input in `overlapping_samples_df2`.
#'  - \strong{Reference median normalization}: One of the datasets (`df1`) is
#'  adjusted to a predefined set of adjustement factors. This is effectively
#'  subset normalization, but using differences of medians to pre-recorded
#'  median values. `df1`, `overlapping_samples_df1`, `df1_project_nr` and
#'  `reference_medians` need to be specified. Dataset `df1` is normalized using
#'  the differences in median between the overlapping samples and the reference
#'  medians.
#'
#' The output dataset is `df1` if reference median normalization, or `df2`
#' appended to `df1` if bridge or subset normalization. In either case, the
#' output dataset contains all original columns from the original dataset(s),
#' and the columns "Project" and "Adj_factor". The former marks the project of
#' origin based on `df1_project_nr` and `df2_project_nr`, and the latter the
#' adjustment factor that was applied to the non-reference dataset.
#'
#' @param df1 First dataset to be used for normalization (required).
#' @param df2 Second dataset to be used for normalization. Required for bridge
#' and subset normalization.
#' @param df1_project_nr Project name of first dataset (required).
#' @param df2_project_nr Project name of second dataset. Required for bridge and
#' subset normalization.
#' @param overlapping_samples_df1 Character vector of samples to be used for the
#' calculation of adjustment factors in `df1` (required).
#' @param overlapping_samples_df2 Character vector of samples to be used for the
#' calculation of adjustment factors in `df2`. Required for subset
#' normalization.
#' @param reference_project Project to be used as reference project. Should
#' be one of `df1_project_nr` and `df2_project_nr`. Required for bridge and
#' subset normalization.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Required for reference median normalization.
#'
#' @return Tibble or ArrowObject with the normalized dataset.
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#' # prepare datasets
#' npx_df1 <- npx_data1 |>
#'   dplyr::mutate(
#'     Normalization = "Intensity"
#'   )
#' npx_df2 <- npx_data2 |>
#'   dplyr::mutate(
#'     Normalization = "Intensity"
#'   )
#'
#' # bridge normalization
#'
#' # overlapping samples - exclude control samples
#' overlap_samples <- intersect(x = npx_df1$SampleID,
#'                              y = npx_df2$SampleID) |>
#'   (\(x) x[!grepl("^CONTROL_SAMPLE", x)])()
#'
#' # normalize
#' olink_normalization(
#'   df1 = npx_df1,
#'   df2 = npx_df2,
#'   overlapping_samples_df1 = overlap_samples,
#'   df1_project_nr = "P1",
#'   df2_project_nr = "P2",
#'   reference_project = "P1"
#' )
#'
#' # subset normalization
#'
#' # find a suitable subset of samples from each dataset:
#' # exclude control samples
#' # exclude samples that do not pass QC
#' df1_samples <- npx_df1 |>
#'   dplyr::group_by(
#'     dplyr::pick(
#'       dplyr::all_of("SampleID")
#'     )
#'   )|>
#'   dplyr::filter(
#'     all(.data[["QC_Warning"]] == 'Pass')
#'   ) |>
#'   dplyr::ungroup() |>
#'   dplyr::filter(
#'     !grepl(pattern = "^CONTROL_SAMPLE", x = .data[["SampleID"]])
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#' df2_samples <- npx_df2 |>
#'   dplyr::group_by(
#'     dplyr::pick(
#'       dplyr::all_of("SampleID")
#'     )
#'   )|>
#'   dplyr::filter(
#'     all(.data[["QC_Warning"]] == 'Pass')
#'   ) |>
#'   dplyr::ungroup() |>
#'   dplyr::filter(
#'     !grepl(pattern = "^CONTROL_SAMPLE", x = .data[["SampleID"]])
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#'
#' # select a subset of samples from each set from above
#' df1_subset <- sample(x = df1_samples, size = 16L)
#' df2_subset <- sample(x = df2_samples, size = 20L)
#'
#' # normalize
#' olink_normalization(
#'   df1 = npx_df1,
#'   df2 = npx_df2,
#'   overlapping_samples_df1 = df1_subset,
#'   overlapping_samples_df2 = df2_subset,
#'   df1_project_nr = "P1",
#'   df2_project_nr = "P2",
#'   reference_project = "P1"
#' )
#'
#' # special case of subset normalization using all samples
#' olink_normalization(
#'   df1 = npx_df1,
#'   df2 = npx_df2,
#'   overlapping_samples_df1 = df1_samples,
#'   overlapping_samples_df2 = df2_samples,
#'   df1_project_nr = "P1",
#'   df2_project_nr = "P2",
#'   reference_project = "P1"
#' )
#'
#' # reference median normalization
#'
#' # For the sake of this example, set the reference median to 1
#' ref_med_df <- npx_data1 |>
#'   dplyr::select(
#'     dplyr::all_of(
#'       c("OlinkID")
#'     )
#'   ) |>
#'   dplyr::distinct() |>
#'   dplyr::mutate(
#'     Reference_NPX = runif(n = dplyr::n(),
#'                           min = -1,
#'                           max = 1)
#'   )
#'
#' # normalize
#' olink_normalization(
#'   df1 = npx_df1,
#'   overlapping_samples_df1 = df1_subset,
#'   reference_medians = ref_med_df
#' )
#' }
#'
olink_normalization <- function(df1,
                                df2 = NULL,
                                overlapping_samples_df1,
                                overlapping_samples_df2 = NULL,
                                df1_project_nr = "P1",
                                df2_project_nr = "P2",
                                reference_project = "P1",
                                reference_medians = NULL) {

  # check input ----
  lst_check <- olink_norm_input_check(
    df1 = df1,
    df2 = df2,
    overlapping_samples_df1 = overlapping_samples_df1,
    overlapping_samples_df2 = overlapping_samples_df2,
    df1_project_nr = df1_project_nr,
    df2_project_nr = df2_project_nr,
    reference_project = reference_project,
    reference_medians = reference_medians
  )

  # normalize ----

  if (lst_check$norm_mode == olink_norm_modes$ref_median) {
    ## reference median normalization ----

    df_norm <- norm_internal_reference_median(
      ref_df = lst_check$ref_df,
      ref_samples = lst_check$ref_samples,
      ref_name = lst_check$ref_name,
      ref_cols = lst_check$ref_cols,
      reference_medians = lst_check$reference_medians
    )

  } else {
    ## rename non-reference columns to reference columns ----

    # update selected colnames of not_ref_df based on colnames of ref_df
    lst_check$not_ref_df <- norm_internal_rename_cols(
      ref_cols = lst_check$ref_cols,
      not_ref_cols = lst_check$not_ref_cols,
      not_ref_df = lst_check$not_ref_df
    )
    # update not_ref_cols, which is the non-reference df
    lst_check$not_ref_cols <- olink_norm_input_check_df_cols(
      lst_df = list(
        "non_ref" = lst_check$not_ref_df
      )
    )[[1L]] |>
      suppressWarnings() |>
      suppressMessages()

    ## normalize bridge or subest ----

    if (lst_check$norm_mode == olink_norm_modes$bridge) {
      # bridge normalization ----

      df_norm <- norm_internal_bridge(
        ref_df = lst_check$ref_df,
        ref_samples = lst_check$ref_samples,
        ref_name = lst_check$ref_name,
        ref_cols = lst_check$ref_cols,
        not_ref_df = lst_check$not_ref_df,
        not_ref_name = lst_check$not_ref_name,
        not_ref_cols = lst_check$not_ref_cols
      )

    } else if (lst_check$norm_mode == olink_norm_modes$norm_ht_3k) {
      # HT-3K normalization ----

      df_norm <- norm_internal_cross_product(
        ref_df = lst_check$ref_df,
        ref_samples = lst_check$ref_samples,
        ref_name = lst_check$ref_name,
        ref_cols = lst_check$ref_cols,
        not_ref_df = lst_check$not_ref_df,
        not_ref_name = lst_check$not_ref_name,
        not_ref_cols = lst_check$not_ref_cols
      )

    } else if (lst_check$norm_mode == olink_norm_modes$subset) {
      # subset normalization ----

      df_norm <- norm_internal_subset(
        ref_df = lst_check$ref_df,
        ref_samples = lst_check$ref_samples,
        ref_name = lst_check$ref_name,
        ref_cols = lst_check$ref_cols,
        not_ref_df = lst_check$not_ref_df,
        not_ref_samples = lst_check$not_ref_samples,
        not_ref_name = lst_check$not_ref_name,
        not_ref_cols = lst_check$not_ref_cols
      )

    }
  }

  return(df_norm)
}

#' Update column names of non-reference dataset based on those of reference
#' dataset
#'
#' @description
#' This function handles cases when specific columns referring to the same thing
#' are named differently in `df1` and `df2` normalization datasets. It only
#' renames columns
#' `r cli::ansi_collapse(c("panel_version", "qc_warn", "assay_warn"))` based on
#' their names in the reference dataset.#'
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_cols Named list of column names identified in the reference
#' dataset.
#' @param not_ref_cols Named list of column names identified in the
#' non-reference dataset.
#' @param not_ref_df Non-reference dataset to be used in normalization.
#'
#' @return `not_ref_df` with updated column names.
#'
norm_internal_rename_cols <- function(ref_cols,
                                      not_ref_cols,
                                      not_ref_df) {

  # only these columns can be updated to the reference df
  cols_to_update <- c("panel_version", "qc_warn", "assay_warn")

  # tibble with 2 columns, one from reference and the other one from the
  # non-reference df. Used next to rename all columns of non-reference df
  # according to the ones from reference.
  df_nonref_cols_rename <- lapply(names(ref_cols), function(c_to_u) {
    if (c_to_u %in% cols_to_update) {
      if (length(ref_cols[[c_to_u]]) != length(not_ref_cols[[c_to_u]])) {
        cli::cli_abort(
          c(
            "x" = "Cannot rename {cli::qty(not_ref_cols[[c_to_u]])}
            column{?s} {.val {not_ref_cols[[c_to_u]]}}, with
            {cli::qty(ref_cols[[c_to_u]])} column{?s}
            {.val {ref_cols[[c_to_u]]}}!",
            "i" = "Sizes of vectors do not match!"
          )
        )
      } else {
        dplyr::tibble(ref = ref_cols[[c_to_u]],
                      non_ref = not_ref_cols[[c_to_u]])
      }
    } else {
      dplyr::tibble(ref = not_ref_cols[[c_to_u]],
                    non_ref = not_ref_cols[[c_to_u]])
    }
  }) |>
    dplyr::bind_rows()

  # rename columns from non-reference df
  not_ref_df <- not_ref_df |>
    dplyr::rename_with(
      .fn = ~ df_nonref_cols_rename$ref,
      .cols = dplyr::all_of(df_nonref_cols_rename$non_ref)
    )

  return(not_ref_df)
}

#' Compute median value of the quantification method for each Olink assay
#'
#' @description
#' The function computes the median value of the the quantification method for
#' each Olink assay in the set of samples `samples`, and it adds the column
#' `Project`.
#'
#' @details
#' This function is typically used by internal functions
#' \code{\link{norm_internal_subset}} and
#' \code{\link{norm_internal_reference_median}} that compute median
#' quantification value for each assay across multiple samples specified by
#' `samples`.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df The dataset to calculate medians from (required).
#' @param samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the dataset `df` (required).
#' @param name Project name of the dataset that will be added in the column
#' `Project` (required).
#' @param cols Named list of column names identified in the dataset `df`
#' (required).
#'
#' @return Tibble or ArrowObject with one row per Olink assay and the columns
#' `r cli::ansi_collapse(x = c("OlinkID", "Project", "assay_med"))`
#'
norm_internal_assay_median <- function(df,
                                       samples,
                                       name,
                                       cols) {
  df |>
    # filter out df samples not in samples
    dplyr::filter(
      .data[[cols$sample_id]] %in% .env[["samples"]]
    ) |>
    # add project name for df
    dplyr::mutate(
      Project = .env[["name"]]
    ) |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(
          c(cols$olink_id, "Project")
        )
      )
    ) |>
    dplyr::summarise(
      assay_med = median(x = .data[[cols$quant]], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Internal reference median normalization function
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the reference dataset (required).
#' @param ref_name Project name of the reference dataset (required).
#' @param ref_cols Named list of column names in the reference dataset
#' (required).
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX"
#' (required). Used for reference median normalization.
#'
#' @return Tibble or ArrowObject with the normalized dataset.
#'
norm_internal_reference_median <- function(ref_df,
                                           ref_samples,
                                           ref_name,
                                           ref_cols,
                                           reference_medians) {
  # calculate adjustment factors ----

  # named vector to join ref_df to reference_medians
  join_by <- "OlinkID"
  names(join_by) <- ref_cols$olink_id

  adj_fct_df <- norm_internal_assay_median(
    df = ref_df,
    samples = ref_samples,
    name = ref_name,
    cols = ref_cols
  ) |>
    dplyr::left_join(
      reference_medians,
      by = join_by,
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      Adj_factor = .data[["Reference_NPX"]] - .data[["assay_med"]],
      Adj_factor = dplyr::if_else(is.na(.data[["Adj_factor"]]),
                                  0,
                                  .data[["Adj_factor"]])
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$olink_id, "Adj_factor")
      )
    )

  # normalize dataset ----

  df_norm <- norm_internal_adjust_not_ref(
    df = ref_df,
    name = ref_name,
    cols = ref_cols,
    adj_fct_df = adj_fct_df,
    adj_fct_cols = ref_cols
  )

  # return ----

  return(df_norm)
}

#' Internal bridge normalization function
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the reference dataset (required).
#' @param ref_name Project name of the reference dataset (required).
#' @param ref_cols Named list of column names in the reference dataset
#' (required).
#' @param not_ref_df The non-reference dataset to be used in normalization
#' (required).
#' @param not_ref_name Project name of the non-reference dataset (required).
#' @param not_ref_cols Named list of column names in the non-reference dataset
#' (required).
#'
#' @return Tibble or ArrowObject with the normalized dataset.
#'
norm_internal_bridge <- function(ref_df,
                                 ref_samples,
                                 ref_name,
                                 ref_cols,
                                 not_ref_df,
                                 not_ref_name,
                                 not_ref_cols) {
  # calculate adjustment factors ----

  adj_fct_df <- ref_df |>
    # add project name for ref_df
    dplyr::mutate(
      Project = .env[["ref_name"]]
    ) |>
    # append not_ref_df to ref_df
    dplyr::bind_rows(
      not_ref_df |>
        # add project name for not_ref_df
        dplyr::mutate(
          Project = .env[["not_ref_name"]]
        )
    ) |>
    # keep only bridge samples
    dplyr::filter(
      .data[[ref_cols$sample_id]] %in% .env[["ref_samples"]]
    ) |>
    # keep only relevant column to compute pair-wise differences
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$sample_id, ref_cols$olink_id, ref_cols$quant, "Project")
      )
    ) |>
    # pivot quantification to 2 columns to compute pair-wise differences
    tidyr::pivot_wider(
      names_from = dplyr::all_of("Project"),
      values_from = dplyr::all_of(ref_cols$quant),
    ) |>
    # compute pair-wise differences as reference - non-reference
    dplyr::mutate(
      quant_diff = .data[[ref_name]] - .data[[not_ref_name]]
    ) |>
    # compute median of pair-wise differences
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(ref_cols$olink_id)
      )
    ) |>
    dplyr::summarise(
      Adj_factor = median(x = .data[["quant_diff"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Adj_factor = dplyr::if_else(is.na(.data[["Adj_factor"]]),
                                  0,
                                  .data[["Adj_factor"]])
    )

  # normalize datasets ----

  df_norm <- norm_internal_adjust(
    ref_df = ref_df,
    ref_name = ref_name,
    ref_cols = ref_cols,
    not_ref_df = not_ref_df,
    not_ref_name = not_ref_name,
    not_ref_cols = not_ref_cols,
    adj_fct_df = adj_fct_df
  )

  # return ----

  return(df_norm)
}

#' Internal function normalizing Olink Explore 3k to Olink Explore 3072
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the reference dataset (required).
#' @param ref_name Project name of the reference dataset (required).
#' @param ref_cols Named list of column names in the reference dataset
#' (required).
#' @param not_ref_df The non-reference dataset to be used in normalization
#' (required).
#' @param not_ref_name Project name of the non-reference dataset (required).
#' @param not_ref_cols Named list of column names in the non-reference dataset
#' (required).
#'
#' @return Tibble or ArrowObject with a dataset with the following additional
#' columns:
#' \itemize{
#'    \item{OlinkID_E3072:} Corresponding assay identifier from Olink Explore
#'    3072.
#'    \item{Project:} Project of origin.
#'    \item{BridgingRecommendation:} Recommendation of whether the assay is
#'    bridgeable or not. One of "NotBridgeable", "MedianCentering", or
#'    "QuantileSmoothing".
#'    \item{MedianCenteredNPX:} NPX values adjusted based on the median of the
#'    pair-wise differences of NPX values between bridge samples.
#'    \item{QSNormalizedNPX:} NPX values adjusted based on the quantile-quantile
#'    normalization among bridge samples.
#' }
#'
norm_internal_cross_product <- function(ref_df,
                                        ref_samples,
                                        ref_name,
                                        ref_cols,
                                        not_ref_df,
                                        not_ref_name,
                                        not_ref_cols) {
  # prepare inputs ----

  lst_df <- list(
    ref_df,
    not_ref_df
  )
  names(lst_df) <- c(ref_name, not_ref_name)

  # is bridgeable ----

  df_is_bridgeable <- olink_normalization_bridgeable(
    lst_df = lst_df |> # keep only bridge samples
      lapply(function(l_df) {
        l_df |>
          dplyr::filter(
            .data[[ref_cols$sample_id]] %in% .env[["ref_samples"]]
          )
      }),
    ref_cols = ref_cols
  )

  # bridge normalize HT-3k ----

  df_norm_bridge <- norm_internal_bridge(
    ref_df = ref_df,
    ref_samples = ref_samples,
    ref_name = ref_name,
    ref_cols = ref_cols,
    not_ref_df = not_ref_df,
    not_ref_name = not_ref_name,
    not_ref_cols = not_ref_cols
  ) |>
    # keep only relevant columns
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$sample_id, ref_cols$olink_id, "Project",
          # rename bridge-normalized column
          "MedianCenteredNPX" = ref_cols$quant)
      )
    )

  # quantile normalize HT-3k ----

  df_norm_qq <- olink_normalization_qs(
    lst_df = lst_df,
    ref_cols = ref_cols,
    bridge_samples = ref_samples
  )

  # integrate normalization approaches ----

  # combines the original dataset with df_is_bridgeable, df_norm_bridge and
  # df_norm_qq, and stores the outcome in df_norm
  df_norm <- lst_df |>
    # append original datasets
    dplyr::bind_rows(
      .id = "Project"
    ) |>
    # when ref and non-ref datasets are merged during bridging, assay
    # identifiers from each platform (e.g. OlinkID_HT and OlinkID_E3072) will be
    # NA. Here we fill them in for completion.
    dplyr::group_by(
      .data[[ref_cols$olink_id]]
    ) |>
    tidyr::fill(
      dplyr::starts_with(ref_cols$olink_id),
      .direction = "updown"
    ) |>
    dplyr::ungroup() |>
    # add bridge normalized values
    dplyr::left_join(
      df_norm_bridge,
      by = c(ref_cols$sample_id, ref_cols$olink_id, "Project"),
      relationship = "one-to-one"
    ) |>
    # add QS values
    dplyr::left_join(
      df_norm_qq,
      by = c(ref_cols$sample_id, ref_cols$olink_id, "Project"),
      relationship = "one-to-one"
    ) |>
    # add bridgeable
    dplyr::left_join(
      df_is_bridgeable,
      by = ref_cols$olink_id,
      relationship = "many-to-one"
    ) |>
    # reorder columns
    dplyr::select(
      dplyr::all_of(names(ref_df)), dplyr::everything()
    ) |>
    # cleanup columns
    dplyr::select(
      -dplyr::all_of(
        c(ref_cols$olink_id)
      )
    ) |>
    dplyr::rename(
      !!ref_cols$olink_id := "OlinkID_HT"
    )

  # return ----

  return(df_norm)

}

#' Internal subset normalization function
#'
#' @description
#' This function performs subset normalization using a subset of the samples
#' from either or both reference and non-reference datasets. When all samples
#' from each dataset are used, the function performs intensity normalization.
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the reference dataset (required).
#' @param ref_name Project name of the reference dataset (required).
#' @param ref_cols Named list of column names in the reference dataset
#' (required).
#' @param not_ref_df The non-reference dataset to be used in normalization
#' (required).
#' @param not_ref_samples Character vector of sample identifiers to be used for
#' adjustment factor calculation in the non-reference dataset (required).
#' @param not_ref_name Project name of the non-reference dataset (required).
#' @param not_ref_cols Named list of column names in the non-reference dataset
#' (required).
#'
#' @return Tibble or ArrowObject with the normalized dataset.
#'
norm_internal_subset <- function(ref_df,
                                 ref_samples,
                                 ref_name,
                                 ref_cols,
                                 not_ref_df,
                                 not_ref_samples,
                                 not_ref_name,
                                 not_ref_cols) {
  # calculate adjustment factors ----

  adj_fct_df <- dplyr::bind_rows(
    # ref_df
    norm_internal_assay_median(df = ref_df,
                               samples = ref_samples,
                               name = ref_name,
                               cols = ref_cols),
    # not_ref_df
    norm_internal_assay_median(df = not_ref_df,
                               samples = not_ref_samples,
                               name = not_ref_name,
                               cols = not_ref_cols)
  ) |>
    # pivot quantification to 2 columns to compute pair-wise differences
    tidyr::pivot_wider(
      names_from = dplyr::all_of("Project"),
      values_from = dplyr::all_of("assay_med"),
    ) |>
    # compute differences of medians as reference - non-reference
    dplyr::mutate(
      Adj_factor = .data[[ref_name]] - .data[[not_ref_name]],
      Adj_factor = dplyr::if_else(is.na(.data[["Adj_factor"]]),
                                  0,
                                  .data[["Adj_factor"]])
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$olink_id, "Adj_factor")
      )
    )

  # normalize datasets ----

  df_norm <- norm_internal_adjust(
    ref_df = ref_df,
    ref_name = ref_name,
    ref_cols = ref_cols,
    not_ref_df = not_ref_df,
    not_ref_name = not_ref_name,
    not_ref_cols = not_ref_cols,
    adj_fct_df = adj_fct_df
  )

  # return ----

  return(df_norm)
}

#' Combine reference and non-reference datasets
#'
#' @description
#' The function is used by \code{\link{norm_internal_subset}} and
#' \code{\link{norm_internal_bridge}} to combine the reference dataset that has
#' `Adj_factor = 0` and the non-reference dataset that used the adjustment
#' factors provided in `adj_fct_df`.
#'
#' @details
#' The function calls \code{\link{norm_internal_adjust_ref}} and
#' \code{\link{norm_internal_adjust_not_ref}} and combines their outputs.
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_name Project name of the reference dataset (required).
#' @param ref_cols Named list of column names in the reference dataset
#' (required).
#' @param not_ref_df The non-reference dataset to be used in normalization
#' (required).
#' @param not_ref_name Project name of the non-reference dataset (required).
#' @param not_ref_cols Named list of column names in the non-reference dataset
#' (required).
#' @param adj_fct_df Dataset containing the adjustment factors to be applied to
#' the non-reference dataset for (required).
#'
#' @return Tibble or ArrowObject with the normalized dataset.
#'
norm_internal_adjust <- function(ref_df,
                                 ref_name,
                                 ref_cols,
                                 not_ref_df,
                                 not_ref_name,
                                 not_ref_cols,
                                 adj_fct_df) {
  dplyr::bind_rows(
    # reference dataset
    norm_internal_adjust_ref(
      ref_df = ref_df,
      ref_name = ref_name
    ),
    # not reference dataset
    norm_internal_adjust_not_ref(
      df = not_ref_df,
      name = not_ref_name,
      cols = not_ref_cols,
      adj_fct_df = adj_fct_df,
      adj_fct_cols = ref_cols
    )
  )
}

#' Modify the reference dataset to be combined with the non-reference normalized
#' dataset
#'
#' @author
#'   Klev Diamanti
#'
#' @param ref_df The reference dataset to be used in normalization (required).
#' @param ref_name Project name of the reference dataset (required).
#'
#' @return Tibble or ArrowObject with the reference dataset with additional
#' columns "Project" and "Adj_factor".
#'
norm_internal_adjust_ref <- function(ref_df,
                                     ref_name) {
  ref_df |>
    # add project name and Adj_factor = 0 to ref_df
    dplyr::mutate(
      Project = .env[["ref_name"]],
      Adj_factor = 0
    )
}

#' Add adjustment factors to a dataset
#'
#' @author
#'   Klev Diamanti
#'
#' @param df The dataset to be normalized (required).
#' @param name Project name of the dataset (required).
#' @param cols Named list of column names in the dataset (required).
#' @param adj_fct_df Dataset containing the adjustment factors to be applied to
#' the dataset `not_ref_df` (required).
#' @param adj_fct_cols Named list of column names in the dataset containing
#' adjustment factors (required).
#'
#' @return Tibble or ArrowObject with the normalized dataset with additional
#' columns "Project" and "Adj_factor".
#'
norm_internal_adjust_not_ref <- function(df,
                                         name,
                                         cols,
                                         adj_fct_df,
                                         adj_fct_cols) {
  # named vector to join dfs
  join_by <- adj_fct_cols$olink_id
  names(join_by) <- cols$olink_id

  # adjust non reference df
  df |>
    dplyr::left_join(
      adj_fct_df,
      by = join_by,
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      Project = .env[["name"]],
      dplyr::across(
        c(cols$quant, cols$lod),
        ~ .x + .data[["Adj_factor"]]
      )
    )
}
