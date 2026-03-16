#' Function to plot an overview of a sample cohort per Panel
#'
#' Generates a facet plot per Panel using ggplot2::ggplot and
#' ggplot2::geom_point and stats::IQR plotting IQR vs. median for all samples.
#' Horizontal dashed lines indicate +/-IQR_outlierDef standard deviations from
#' the mean IQR (default 3).
#' Vertical dashed lines indicate +/-median_outlierDef standard deviations from
#' the mean sample median (default 3).
#'
#' @param df NPX data frame in long format.
#' Must have columns SampleID, NPX and Panel
#' @param check_log A named list returned by [`check_npx()`].
#' If `NULL`, [`check_npx()`] will be run internally using `df`.
#' @param color_g Character value indicating which column to use as fill color
#'  (default QC_Warning). Continuous color scale for Olink(R) Sample Index
#'  (OSI) columns OSITimeToCentrifugation, OSIPreparationTemperature and
#'  OSISummary is also supported.
#' @param plot_index Boolean. If FALSE (default), a point will be plotted
#' for a sample. If TRUE,
#' a sample's unique index number is displayed.
#' @param label_outliers Boolean. If TRUE, an outlier sample will be labelled
#' with its SampleID.
#' @param IQR_outlierDef The number of standard deviations from the mean IQR
#' that defines an outlier (default 3)
#' @param median_outlierDef The number of standard deviations from the mean
#' sample median that defines an outlier. (default 3)
#' @param outlierLines Draw dashed lines at +/-IQR_outlierDef and
#' +/-median_outlierDef standard deviations from the mean IQR and sample median
#' respectively (default TRUE)
#' @param facetNrow The number of rows that the panels are arranged on
#' @param facetNcol The number of columns that the panels are arranged on
#' @param ... coloroption passed to specify color order
#' @return An object of class "ggplot". Scatterplot shows IQR vs median for all
#' samples per panel
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#'
#'if (rlang::is_installed(pkg = c("ggrepel"))) {
#'
#' olink_qc_plot(npx_data1,
#' color_g = "QC_Warning",
#' label_outliers = TRUE)
#'
#' #Change the outlier threshold to +-4SD
#' olink_qc_plot(npx_data1,
#' color_g = "QC_Warning",
#'  IQR_outlierDef = 4,
#'  median_outlierDef = 4,
#'  label_outliers = TRUE)
#'
#' #Identify the outliers
#' qc <- olink_qc_plot(npx_data1,
#' color_g = "QC_Warning",
#' IQR_outlierDef = 4,
#' median_outlierDef = 4,
#' label_outliers = TRUE)
#'
#' outliers <- qc$data |> filter(Outlier == 1)
#' }
#' }
#' @importFrom dplyr group_by mutate ungroup select distinct if_else filter case_when
#' @importFrom rlang ensym
#' @importFrom ggplot2 ggplot geom_hline geom_vline xlab facet_wrap geom_text geom_point
#' @importFrom ggrepel geom_label_repel
#' @importFrom stringr str_detect str_replace

olink_qc_plot <- function(df,
                          check_log = NULL,
                          color_g = "QC_Warning",
                          plot_index = FALSE,
                          label_outliers = FALSE,
                          IQR_outlierDef = 3L, #nolint object_name_linter
                          median_outlierDef = 3L, #nolint object_name_linter
                          outlierLines = TRUE, #nolint object_name_linter
                          facetNrow = NULL, #nolint object_name_linter
                          facetNcol = NULL, #nolint object_name_linter
                          ...) {

  #checking ellipsis
  if (length(list(...)) > 0L) {

    ellipsis_variables <- names(list(...))

    if (length(ellipsis_variables) == 1L) {

      if (!(ellipsis_variables == "coloroption")) {

        stop(paste0("The ... option only takes the coloroption argument. ",
                    "... currently contains the variable ",
                    ellipsis_variables,
                    "."))

      }

    } else {

      stop(paste0("The ... option only takes one argument.
                  ... currently contains the variables ",
                  paste(ellipsis_variables, collapse = ", "),
                  "."))
    }
  }

  # Check if check_log is correct
  check_log <- run_check_npx(df = df, check_log = check_log)

  # Remove invalid OlinkID, assays with all NA values, and convert non-unique
  # Uniprot IDs. Note that we do not remove samples with duplicate SampleID,
  # control samples or assays, or samples/assays with QC warnings, as this
  # would be the user's decision.
  df <- clean_npx(
    df,
    check_log = check_log,
    remove_assay_na = TRUE,
    remove_invalid_oid = TRUE,
    remove_dup_sample_id = FALSE,
    remove_control_assay = FALSE,
    remove_control_sample = FALSE,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE,
    convert_nonunique_uniprot = TRUE,
    out_df = "tibble",
    verbose = FALSE
  ) |>
    suppressMessages()

  #Check that IQR_outlierDef and median_outlierDef are both numeric
  if (!all(is.numeric(IQR_outlierDef), is.numeric(median_outlierDef))) {
    stop("IQR_outlierDef and median_outlierDef have to be numeric values")
  }

  if (plot_index == TRUE && !("Index" %in% names(df))) {
    warning("Index not available. Setting plot_index to FALSE.")
    plot_index <- FALSE
  }

  # OSI checks - ran only if OSI columns selected to color
  osi_cat_cols <- c("OSICategory")
  osi_cont_cols <- c("OSITimeToCentrifugation",
                     "OSIPreparationTemperature",
                     "OSISummary")

  if (color_g %in% c(osi_cat_cols, osi_cont_cols)) {

    # Check for invalid values
    v_chr <- df |>
      dplyr::select(dplyr::all_of(color_g)) |>
      dplyr::pull() |>
      as.character()

    # ERROR if column exists but is entirely NA
    if (all(is.na(v_chr))) {
      cli::cli_abort(
        "All values are NA in {color_g}. Please provide at least one
        non-missing value."
      )
    }

    # Categorical
    if (color_g %in% osi_cat_cols) {

      # Check that values are in allowed range
      allowed <- as.character(0L:4L)
      invalid_vals <- unique(v_chr[!is.na(v_chr) & !(v_chr %in% allowed)])

      if (length(invalid_vals) > 0L) {
        cli::cli_abort(
          "Invalid values detected in {color_g}. Expected only 0, 1, 2, 3, or 4.
          Found: {.val {invalid_vals}}."
        )
      }

      # Convert to factor if needed
      if (!is.factor(df[[color_g]])) {
        df[[color_g]] <- factor(as.character(df[[color_g]]), levels = allowed)

        df <- df |>
          dplyr::mutate(
            !!color_g := as.character(factor(df[[color_g]], levels = allowed))
          )
      }
    }

    if (color_g %in% osi_cont_cols) {

      # Check if numeric
      if (!all(is.numeric(df[[color_g]]))) {

        # Detect non-numeric entries (introduced NA after coercion)
        v_num <- suppressWarnings(as.numeric(df[[color_g]]))

        non_numeric_idx <- which(
          !is.na(df[[color_g]]) & is.na(v_num)
        )
        bad_vals <- unique(df[[color_g]][non_numeric_idx]) #nolint object_name_linter
        cli::cli_abort(
          "Invalid values detected in {color_g}. Expected continuous numeric
          values between 0 and 1. Found non-numeric value(s):
          {.val {bad_vals}}."
        )
      }

      # Detect out-of-range values
      out_of_range_idx <- which(
        !is.na(df[[color_g]]) & (df[[color_g]] < 0L | df[[color_g]] > 1L)
      )
      if (length(out_of_range_idx) > 0L) {
        bad_vals <- unique(df[[color_g]][out_of_range_idx]) #nolint object_name_linter
        cli::cli_abort(
          "Invalid values detected in {.field {color_g}}. Expected continuous
          numeric values between 0 and 1. Found out-of-range value(s):
          {.val {bad_vals}}."
        )
      }
    }
  }

  if ("QC_Warning" %in% names(df)) {
    df_qr <- df |>
      dplyr::group_by(.data[["Panel"]], .data[["SampleID"]]) |>
      dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(.data[["QC_Warning"]]) == "PASS"), #nolint line_length_linter
                                                "Pass",
                                                "Warning"))
  } else {
    df_qr <- df |>
      dplyr::group_by(.data[["Panel"]], .data[["SampleID"]])
  }

  df_qr <- df_qr |>
    dplyr::mutate(IQR = IQR(.data[["NPX"]], na.rm = TRUE),
                  sample_median = median(.data[["NPX"]], na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(.data[["SampleID"]],
                  .data[["Panel"]],
                  .data[["IQR"]],
                  .data[["sample_median"]],
                  !!rlang::ensym(color_g)) |>
    dplyr::distinct() |>
    dplyr::group_by(.data[["Panel"]]) |>
    dplyr::mutate(median_low = mean(.data[["sample_median"]],
                                    na.rm = TRUE) -
                    .env[["median_outlierDef"]] * sd(.data[["sample_median"]],
                                                     na.rm = TRUE),
                  median_high = mean(.data[["sample_median"]], na.rm = TRUE) +
                    .env[["median_outlierDef"]] * sd(.data[["sample_median"]],
                                                     na.rm = TRUE),
                  iqr_low = mean(.data[["IQR"]], na.rm = TRUE) -
                    .env[["IQR_outlierDef"]] * sd(.data[["IQR"]],
                                                  na.rm = TRUE),
                  iqr_high = mean(.data[["IQR"]], na.rm = TRUE) +
                    .env[["IQR_outlierDef"]] * sd(.data[["IQR"]],
                                                  na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Outlier = dplyr::if_else(.data[["sample_median"]] < .data[["median_high"]] & #nolint line_length_linter
                                             .data[["sample_median"]] > .data[["median_low"]] & #nolint line_length_linter
                                             .data[["IQR"]] > .data[["iqr_low"]] & #nolint line_length_linter
                                             .data[["IQR"]] < .data[["iqr_high"]], #nolint line_length_linter
                                           0L, 1L))


  qc_plot <- df_qr |>
    dplyr::mutate(Panel = .data[["Panel"]] |>
                    stringr::str_replace("Olink ", "")) |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["sample_median"]],
                                 y = .data[["IQR"]])) +
    ggplot2::xlab("Sample Median") +
    ggplot2::facet_wrap(~Panel,
                        scale = "free",
                        nrow = facetNrow,
                        ncol = facetNcol) +
    OlinkAnalyze::set_plot_theme()

  if (color_g %in% osi_cont_cols) {

    qc_plot <- qc_plot +
      ggplot2::scale_color_gradient(
        low = "#FFB200FF",
        high = "#332D56FF",
        limits = c(0L, 1L),
        breaks = seq(from = 0L, to = 1L, by = 0.25),
        oob = scales::squish
      )

  } else {
    qc_plot <- qc_plot +
      OlinkAnalyze::olink_color_discrete(...)
  }

  #Ad d outlier lines
  if (outlierLines) {
    qc_plot <- qc_plot +
      ggplot2::geom_hline(ggplot2::aes(yintercept = .data[["iqr_low"]]),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = .data[["iqr_high"]]),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data[["median_low"]]),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data[["median_high"]]),
                          linetype = "dashed",
                          color = "grey")
  }

  if (plot_index) {
    qc_plot <- qc_plot +
      ggplot2::geom_text(ggplot2::aes(color = !!rlang::ensym(color_g),
                                      label = .data[["Index"]]),
                         size = 3L)
  } else {
    qc_plot <- qc_plot +
      ggplot2::geom_point(ggplot2::aes(color = !!rlang::ensym(color_g)),
                          size = 2.5)
  }

  if (label_outliers) {

    rlang::check_installed(
      pkg = c("lme4", "lmerTest", "broom"),
      call = rlang::caller_env()
    )

    outlier_labels <- qc_plot@data |>
      dplyr::mutate(SampleIDPlot = dplyr::case_when(Outlier == 1 ~ SampleID,
                                                    TRUE ~ ""))

    qc_plot <- qc_plot +
      ggrepel::geom_label_repel(ggplot2::aes(label = outlier_labels$SampleIDPlot), #nolint line_length_linter
                                box.padding = 0.5,
                                min.segment.length = 0.1,
                                show.legend = FALSE,
                                size = 3L)

  }

  return(qc_plot)

}
