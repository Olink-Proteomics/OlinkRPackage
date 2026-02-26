#' Function to make a UMAP plot from the data
#'
#' Computes a manifold approximation and projection using umap::umap and plots
#' the two specified components.
#' Unique sample names are required and imputation by the median is done for
#' assays with missingness <10\% for multi-plate projects and <5\% for single
#' plate projects.
#'
#' The plot is printed, and a list of ggplot objects is returned. \cr\cr
#' If byPanel = TRUE, the data processing (imputation of missing values etc)
#' and subsequent UMAP is performed separately per panel. A faceted plot is
#' printed, while the individual ggplot objects are returned. \cr\cr
#' The arguments outlierDefX and outlierDefY can be used to identify outliers
#' in the UMAP results. Samples more than +/- outlierDefX and outlierDefY
#' standard deviations from the mean of the plotted UMAP component will be
#' labelled. Both arguments have to be specified.
#' NOTE: UMAP is a non-linear data transformation that might not accurately
#' preserve the properties of the data. Distances in the UMAP plane should
#' therefore be interpreted with caution.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice
#' for colors
#' @param color_g Character value indicating which column to use for
#' colors (default QC_Warning). Continuous color scale for Olink(R)
#' Sample Index (OSI) columns OSITimeToCentrifugation,
#' OSIPreparationTemperature and OSISummary is also supported.
#' @param x_val Integer indicating which UMAP component to plot along the
#' x-axis (default 1)
#' @param y_val Integer indicating which UMAP component to plot along the
#' y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with
#' SampleID (default FALSE)
#' @param config object of class umap.config, specifying the parameters for
#' the UMAP algorithm (default umap::umap.defaults)
#' @param drop_assays Logical. All assays with any missing values will be
#' dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values will be
#' dropped.
#' @param byPanel Perform the UMAP per panel (default FALSE)
#' @param outlierDefX The number standard deviations along the UMAP dimension
#' plotted on the x-axis that defines an outlier. See also 'Details"
#' @param outlierDefY The number standard deviations along the UMAP dimension
#' plotted on the y-axis that defines an outlier. See also 'Details"
#' @param outlierLines Draw dashed lines at +/- outlierDefX and outlierDefY
#' standard deviations from the mean of the plotted PCs (default FALSE)
#' @param label_outliers Use ggrepel to label samples lying outside the limits
#' set by the outlierLines (default TRUE)
#' @param quiet Logical. If TRUE, the resulting plot is not printed
#' @param verbose Logical. Whether warnings about the number of samples and/or
#' assays dropped or imputed should be printed to the console.
#' @param ... coloroption passed to specify color order.
#' @return A list of objects of class "ggplot", each plot contains scatter
#' plot of UMAPs
#' @keywords NPX UMAP
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_data <- npx_data1 |>
#'     mutate(SampleID = paste(SampleID, "_", Index, sep = ""))
#' try({ # Requires umap package dependency
#' #UMAP using all the data
#' olink_umap_plot(df=npx_data, color_g = "QC_Warning")
#'
#' #UMAP per panel
#' g <- olink_umap_plot(df=npx_data, color_g = "QC_Warning", byPanel = TRUE)
#' g$Inflammation #Plot only the Inflammation panel
#'
#' #Label outliers
#' olink_umap_plot(df=npx_data, color_g = "QC_Warning",
#'                outlierDefX = 2, outlierDefY = 4) #All data
#' olink_umap_plot(df=npx_data, color_g = "QC_Warning",
#'                outlierDefX = 3, outlierDefY = 2, byPanel = TRUE) #Per panel
#'
#' #Retrieve the outliers
#' g <- olink_umap_plot(df=npx_data, color_g = "QC_Warning",
#'                     outlierDefX = 3, outlierDefY = 2, byPanel = TRUE)
#' outliers <- lapply(g, function(x){x$data}) |>
#'     bind_rows() |>
#'     filter(Outlier == 1)
#' })
#' }
#' @importFrom dplyr filter select group_by ungroup mutate mutate_at if_else
#' n_distinct summarise left_join arrange distinct
#' @importFrom stringr str_detect
#' @importFrom tidyr spread
#' @importFrom tidyselect all_of
#' @importFrom rlang ensym
#' @importFrom tibble column_to_rownames
#' @importFrom stats prcomp
#' @importFrom ggplot2 ggplot aes xlab ylab geom_text geom_point geom_segment
#' labs guides arrow
#' @importFrom ggrepel geom_label_repel
#' @importFrom utils head
#' @importFrom grid unit

olink_umap_plot <- function(df,
                            check_log = NULL,
                            color_g = "QC_Warning",
                            x_val = 1,
                            y_val = 2,
                            config = NULL,
                            label_samples = FALSE,
                            drop_assays = FALSE,
                            drop_samples = FALSE,
                            byPanel = FALSE, # nolint object_name_linter
                            outlierDefX = NA, # nolint object_name_linter
                            outlierDefY = NA, # nolint object_name_linter
                            outlierLines = FALSE, # nolint object_name_linter
                            label_outliers = TRUE,
                            quiet = FALSE,
                            verbose = TRUE,
                            ...) {
  #Is the umap package installed?
  if (!requireNamespace("umap")) {
    stop("Could not load the package umap")
  }

  if (is.null(config)) {
    config <- umap::umap.defaults
  }

  #checking ellipsis
  if (length(list(...)) > 0) {

    ellipsis_variables <- names(list(...))

    if (length(ellipsis_variables) == 1) {

      if (!(ellipsis_variables == "coloroption")) {

        stop(paste0("The ... option only takes the coloroption argument. ... currently contains the variable ", #nolint
                    ellipsis_variables,
                    ".")) #nolint

      }

    }else {

      stop(paste0("The ... option only takes one argument. ... currently contains the variables ", #nolint
                  paste(ellipsis_variables, collapse = ", "),
                  "."))
    }
  }

  # Check if check_log is correct
  check_log <- run_check_npx(df = df, check_log = check_log)
  # Make sure data is a tibble
  df <- convert_read_npx_output(df = df, out_df = "tibble")

  # Stop if duplicate sample ID's detected
  if (length(check_log$sample_id_dups) > 0) {
    cli::cli_abort(
      "Duplicate SampleID(s) detected:
      {paste(check_log$sample_id_dups, collapse = ', ')}.
      Each sample ID must be unique. Please check your data and ensure that
      each sample has a unique identifier."
    )
  }

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
    convert_nonunique_uniprot = TRUE
  )

  # Validate OSI category column: must contain only 0,1,2,3,4; then convert to factor if not already #nolint
  osi_cat_candidates <- "OSICategory"
  osi_cat_found <- intersect(osi_cat_candidates, colnames(df))

  if (length(osi_cat_found) > 0) {
    cat_col <- osi_cat_found[1]

    v_raw <- df[[cat_col]]

    # ERROR if column exists but is entirely NA
    if (all(is.na(v_raw))) {
      stop(
        paste0(
          "All values are NA in ", cat_col,
          ". Please provide at least one non-missing value."
        )
      )
    }

    v_chr <- if (is.factor(v_raw)) as.character(v_raw) else as.character(v_raw)

    allowed <- as.character(0:4)

    invalid_vals <- unique(v_chr[!is.na(v_chr) & !(v_chr %in% allowed)])
    if (length(invalid_vals) > 0) {
      stop(
        paste0(
          "Invalid values detected in ", cat_col,
          ". Expected only 0, 1, 2, 3, or 4. Found: ",
          paste(invalid_vals, collapse = ", ")
        )
      )
    }

    if (!is.factor(df[[cat_col]])) {
      df[[cat_col]] <- factor(v_chr, levels = allowed)
    }
  }

  # Validate OSI continuous columns: must be numeric and within [0, 1]
  osi_cont_cols <- c("OSITimeToCentrifugation", "OSIPreparationTemperature",
                     "OSISummary")
  osi_cont_found <- intersect(osi_cont_cols, colnames(df))

  if (length(osi_cont_found) > 0) {
    for (cc in osi_cont_found) {

      v_raw <- df[[cc]]

      # ERROR if column exists but is entirely NA
      if (all(is.na(v_raw))) {
        stop(
          paste0(
            "All values are NA in ", cc,
            ". Please provide at least one non-missing value."
          )
        )
      }

      v_chr <- if (is.factor(v_raw)) as.character(v_raw) else
        as.character(v_raw)

      v_num <- suppressWarnings(as.numeric(v_chr))

      # Detect non-numeric entries (introduced NA after coercion)
      non_numeric_idx <- which(!is.na(v_chr) & is.na(v_num))
      if (length(non_numeric_idx) > 0) {
        bad_vals <- unique(v_chr[non_numeric_idx])
        stop(
          paste0(
            "Invalid values detected in ", cc,
            ". Expected continuous numeric values between 0 and 1. ",
            "Found non-numeric value(s): ", paste(bad_vals, collapse = ", ")
          )
        )
      }

      # Detect out-of-range values
      out_of_range_idx <- which(!is.na(v_num) & (v_num < 0 | v_num > 1))
      if (length(out_of_range_idx) > 0) {
        bad_vals <- unique(v_num[out_of_range_idx])
        stop(
          paste0(
            "Invalid values detected in ", cc,
            ". Expected continuous numeric values between 0 and 1. ",
            "Found out-of-range value(s): ", paste(bad_vals, collapse = ", ")
          )
        )
      }
    }
  }

  #Check that the user didn't specify just one of outlierDefX and outlierDefY
  if (sum(c(is.numeric(outlierDefX), is.numeric(outlierDefY))) == 1) {
    stop("To label outliers, both outlierDefX and outlierDefY have to be specified as numerical values") #nolint
  }

  #If outlierLines == TRUE, both outlierDefX and outlierDefY have to be specified #nolint
  if (outlierLines) {
    if (!all(is.numeric(outlierDefX), is.numeric(outlierDefY))) {
      stop("outlierLines requested but boundaries not specified. To draw lines, both outlierDefX and outlierDefY have to be specified as numerical values") #nolint
    }
  }

  if (byPanel) {
    # Convert color_g variable to factor (but keep OSI columns continuous)
    osi_cols <- c("OSITimeToCentrifugation", "OSIPreparationTemperature",
                  "OSISummary")
    if (!(color_g %in% osi_cols)) {
      if (!is.factor(df[[paste(color_g)]])) {
        df[[paste(color_g)]] <- as.factor(df[[paste(color_g)]])
      }
    }

    # Strip "Olink" from the panel names
    df <- df |>
      dplyr::mutate(Panel = .data$Panel  |> stringr::str_replace("Olink ", ""))

    plotList <- lapply(unique(df$Panel), function(x) { # nolint object_name_linter
      g <- df |>
        dplyr::filter(.data$Panel == x) |>
        olink_umap_plot.internal(color_g = color_g,
                                 x_val = x_val,
                                 y_val = y_val,
                                 label_samples = label_samples,
                                 config = config,
                                 drop_assays = drop_assays,
                                 drop_samples = drop_samples,
                                 outlierDefX = outlierDefX,
                                 outlierDefY = outlierDefY,
                                 outlierLines = outlierLines,
                                 label_outliers = label_outliers,
                                 verbose = verbose,
                                 ...) +
        ggplot2::labs(title = x)

      #Add Panel info inside the ggplot object
      g$data <- g$data |>
        dplyr::mutate(Panel = x)

      return(g)
    })
    names(plotList) <- unique(df$Panel) # nolint object_name_linter
    if (!quiet) print(ggpubr::ggarrange(plotlist = plotList,
                                        common.legend = TRUE))

  } else {
    umap_plot <- olink_umap_plot.internal(df = df,
                                          color_g = color_g,
                                          x_val = x_val,
                                          y_val = y_val,
                                          label_samples = label_samples,
                                          config = config,
                                          drop_assays = drop_assays,
                                          drop_samples = drop_samples,
                                          outlierDefX = outlierDefX,
                                          outlierDefY = outlierDefY,
                                          outlierLines = outlierLines,
                                          label_outliers = label_outliers,
                                          verbose = verbose,
                                          ...)
    if (!quiet) print(umap_plot)
    #For consistency, return a list even when there's just one plot
    plotList <- list(umap_plot) # nolint
  }

  return(invisible(plotList))
}

olink_umap_plot.internal <- function(df, #nolint
                                     color_g,
                                     x_val,
                                     y_val,
                                     label_samples,
                                     config,
                                     drop_assays,
                                     drop_samples,
                                     outlierDefX, # nolint object_name_linter
                                     outlierDefY, # nolint object_name_linter
                                     outlierLines, # nolint object_name_linter
                                     label_outliers,
                                     verbose = TRUE,
                                     ...) {

  ### Data pre-processing ###
  procData <- npxProcessing_forDimRed(df = df, #nolint
                                      color_g = color_g,
                                      drop_assays = drop_assays,
                                      drop_samples = drop_samples,
                                      verbose = verbose)

  #### UMAP ####
  #Determine number of UMAP components
  n_components <- config$n_components
  if (max(c(x_val, y_val)) > n_components) {
    n_components <- max(c(x_val, y_val))
  }

  umap_fit <- umap::umap(procData$df_wide_matrix, config = config,
                         n_components = n_components)
  umapX <- umap_fit$layout[, x_val] # nolint object_name_linter
  umapY <- umap_fit$layout[, y_val] # nolint object_name_linter
  observation_names <- procData$df_wide$SampleID
  observation_colors <- procData$df_wide$colors

  scores <- data.frame(
    SampleID = observation_names,
    umapX    = umapX,
    umapY    = umapY,
    colors   = observation_colors,
    stringsAsFactors = FALSE
  )

  #Identify outliers
  if (!is.na(outlierDefX) && !is.na(outlierDefY)) {
    scores <- scores |>
      dplyr::mutate(umapX_low = mean(umapX, na.rm = TRUE) -
                      outlierDefX * sd(umapX, na.rm = TRUE),
                    umapX_high = mean(umapX, na.rm = TRUE) +
                      outlierDefX * sd(umapX, na.rm = TRUE),
                    umapY_low = mean(umapY, na.rm = TRUE) -
                      outlierDefY * sd(umapY, na.rm = TRUE),
                    umapY_high = mean(umapY, na.rm = TRUE) +
                      outlierDefY * sd(umapY, na.rm = TRUE)) |>
      dplyr::mutate(Outlier = dplyr::if_else(umapX < .data$umapX_high &
                                               umapX > .data$umapX_low &
                                               umapY > .data$umapY_low &
                                               umapY < .data$umapY_high,
                                             0, 1))
  }

  #### Plotting ####
  umap_plot <- ggplot2::ggplot(scores, ggplot2::aes(x = umapX, y = umapY)) +
    ggplot2::xlab(paste0("UMAP", x_val)) +
    ggplot2::ylab(paste0("UMAP", y_val))


  #Drawing scores
  if (label_samples) {
    umap_plot <- umap_plot +
      ggplot2::geom_text(ggplot2::aes(label = .data$SampleID, color = colors),
                         size = 3) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")
  } else {
    umap_plot <- umap_plot +
      ggplot2::geom_point(ggplot2::aes(color = colors), size = 2.5) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")
  }

  # Continuous color scale for OSI columns
  osi_cols <- c("OSITimeToCentrifugation", "OSIPreparationTemperature",
                "OSISummary")
  if (color_g %in% osi_cols) {

    # Ensure numeric for continuous scale (handles character/factor inputs)
    if (is.factor(scores$colors)) scores$colors <- as.character(scores$colors)
    scores$colors <- suppressWarnings(as.numeric(scores$colors))

    umap_plot <- umap_plot +
      ggplot2::scale_color_gradient(
        low = "#FFB200FF",
        high = "#332D56FF",
        limits = c(0L, 1L),
        breaks = seq(from = 0L, to = 1L, by = 0.25),
        oob = scales::squish
      )
  }


  #Label outliers in figure
  if (!is.na(outlierDefX) && !is.na(outlierDefY) && label_outliers) {
    umap_plot <- umap_plot +
      ggrepel::geom_label_repel(
        data = scores |>
          dplyr::mutate(
            SampleIDPlot = dplyr::case_when(
              .data$Outlier == 1 ~ .data$SampleID,
              TRUE ~ ""
            )
          ),
        ggplot2::aes(label = .data$SampleIDPlot),
        box.padding = 0.5,
        min.segment.length = 0.1,
        show.legend = FALSE,
        size = 3
      )
  }

  #Add outlier lines
  if (outlierLines) {
    umap_plot <- umap_plot +
      ggplot2::geom_hline(ggplot2::aes(yintercept = .data$umapY_low),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = .data$umapY_high),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data$umapX_low),
                          linetype = "dashed",
                          color = "grey") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = .data$umapX_high),
                          linetype = "dashed",
                          color = "grey")
  }



  umap_plot <- umap_plot +
    OlinkAnalyze::set_plot_theme()

  if (!(color_g %in% osi_cols)) {
    umap_plot <- umap_plot +
      OlinkAnalyze::olink_color_discrete(..., drop = FALSE)
  }

  return(umap_plot)
}
