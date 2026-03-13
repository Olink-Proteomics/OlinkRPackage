#' Function to plot an overview of a sample cohort per Panel
#'
#' Generates a facet plot per Panel using ggplot2::ggplot and ggplot2::geom_point and stats::IQR plotting IQR vs. median for all samples.
#' Horizontal dashed lines indicate +/-IQR_outlierDef standard deviations from the mean IQR (default 3).
#' Vertical dashed lines indicate +/-median_outlierDef standard deviations from the mean sample median (default 3).
#'
#' @param df NPX data frame in long format. Must have columns SampleID, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning). Continuous color scale for Olink(R) Sample Index (OSI) columns OSITimeToCentrifugation, OSIPreparationTemperature and OSISummary is also supported.
#' @param plot_index Boolean. If FALSE (default), a point will be plotted for a sample. If TRUE,
#' a sample's unique index number is displayed.
#' @param label_outliers Boolean. If TRUE, an outlier sample will be labelled with its SampleID.
#' @param IQR_outlierDef The number of standard deviations from the mean IQR that defines an outlier (default 3)
#' @param median_outlierDef The number of standard deviations from the mean sample median that defines an outlier. (default 3)
#' @param outlierLines Draw dashed lines at +/-IQR_outlierDef and +/-median_outlierDef standard deviations from the mean IQR and sample median respectively (default TRUE)
#' @param facetNrow The number of rows that the panels are arranged on
#' @param facetNcol The number of columns that the panels are arranged on
#' @param ... coloroption passed to specify color order
#' @return An object of class "ggplot". Scatterplot shows IQR vs median for all samples per panel
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#'
#' olink_qc_plot(npx_data1, color_g = "QC_Warning")
#'
#' #Change the outlier threshold to +-4SD
#' olink_qc_plot(npx_data1, color_g = "QC_Warning", IQR_outlierDef = 4, median_outlierDef = 4)
#'
#' #Identify the outliers
#' qc <- olink_qc_plot(npx_data1, color_g = "QC_Warning", IQR_outlierDef = 4, median_outlierDef = 4)
#' outliers <- qc$data |> filter(Outlier == 1)
#' }
#' @importFrom dplyr group_by mutate ungroup select distinct if_else filter case_when
#' @importFrom rlang ensym
#' @importFrom ggplot2 ggplot geom_hline geom_vline xlab facet_wrap geom_text geom_point
#' @importFrom ggrepel geom_label_repel
#' @importFrom stringr str_detect str_replace

olink_qc_plot <- function(df,
                          color_g = "QC_Warning",
                          plot_index = FALSE,
                          label_outliers = TRUE,
                          IQR_outlierDef = 3,
                          median_outlierDef = 3,
                          outlierLines = TRUE,
                          facetNrow = NULL,
                          facetNcol = NULL,
                          ...){

  #checking ellipsis
  if(length(list(...)) > 0){

    ellipsis_variables <- names(list(...))

    if(length(ellipsis_variables) == 1){

      if(!(ellipsis_variables == 'coloroption')){

        stop(paste0('The ... option only takes the coloroption argument. ',
                    '... currently contains the variable ',
                    ellipsis_variables,
                    '.'))

      }

    }else{

      stop(paste0('The ... option only takes one argument.
                  ... currently contains the variables ',
                  paste(ellipsis_variables, collapse = ', '),
                  '.'))
    }
  }

  #Check data format
  npxCheck <- npxCheck(df)

  # Rename duplicate UniProts
  df <- uniprot_replace(df, npxCheck)

  #Check that IQR_outlierDef and median_outlierDef are both numeric
  if(!all(is.numeric(IQR_outlierDef), is.numeric(median_outlierDef))){
    stop('IQR_outlierDef and median_outlierDef have to be numeric values')
  }

  if(plot_index == TRUE & !("Index" %in% names(df))){
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
      allowed <- as.character(0:4)
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
        !is.na(df[[color_g]]) & (df[[color_g]] < 0 | df[[color_g]] > 1)
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
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) |> #Exclude assays that have all NA:s
      dplyr::group_by(Panel, SampleID) |>
      dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(QC_Warning) == 'PASS'),
                                                'Pass',
                                                'Warning'))
  } else {
    df_qr <- df |>
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) |> #Exclude assays that have all NA:s
      dplyr::group_by(Panel, SampleID)
  }

  df_qr <- df_qr |>
    dplyr::mutate(IQR = IQR(NPX, na.rm = TRUE),
                  sample_median = median(NPX, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(SampleID,
                  Panel,
                  IQR,
                  sample_median,
                  !!rlang::ensym(color_g)) |>
    dplyr::distinct() |>
    dplyr::group_by(Panel) |>
    dplyr::mutate(median_low = mean(sample_median, na.rm = TRUE) -
                    median_outlierDef*sd(sample_median, na.rm = TRUE),
                  median_high = mean(sample_median, na.rm = TRUE) +
                    median_outlierDef*sd(sample_median, na.rm = TRUE),
                  iqr_low = mean(IQR, na.rm = TRUE) -
                    IQR_outlierDef*sd(IQR, na.rm = TRUE),
                  iqr_high = mean(IQR, na.rm = TRUE) +
                    IQR_outlierDef*sd(IQR, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Outlier = dplyr::if_else(sample_median < median_high &
                                             sample_median > median_low &
                                             IQR > iqr_low &
                                             IQR < iqr_high,
                                           0, 1))


  qc_plot <- df_qr |>
    dplyr::mutate(Panel = Panel  |> stringr::str_replace("Olink ", "")) |>
    ggplot2::ggplot(ggplot2::aes(x = sample_median, y = IQR)) +
    ggplot2::xlab('Sample Median') +
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

  #Add outlier lines
  if(outlierLines){
    qc_plot <- qc_plot +
      ggplot2::geom_hline(ggplot2::aes(yintercept=iqr_low),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_hline(ggplot2::aes(yintercept=iqr_high),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = median_low),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = median_high),
                          linetype = 'dashed',
                          color = 'grey')
  }

  if(plot_index){
    qc_plot <- qc_plot +
      ggplot2::geom_text(ggplot2::aes(color = !!rlang::ensym(color_g),
                                      label = Index),
                         size = 3)
  }else{
    qc_plot <- qc_plot +
      ggplot2::geom_point(ggplot2::aes(color = !!rlang::ensym(color_g)),
                          size = 2.5)
  }

  if(label_outliers){

    outlier_labels <- qc_plot@data |>
      dplyr::mutate(SampleIDPlot = dplyr::case_when(Outlier == 1 ~ SampleID,
                                                    TRUE ~ ""))

    qc_plot <- qc_plot +
      ggrepel::geom_label_repel(ggplot2::aes(label=outlier_labels$SampleIDPlot),
                                box.padding = 0.5,
                                min.segment.length = 0.1,
                                show.legend=FALSE,
                                size = 3)

  }

  return(qc_plot)

}
