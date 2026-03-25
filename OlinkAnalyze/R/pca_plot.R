#' Function to plot a PCA of the data
#'
#' @description
#' Generates a PCA projection of all samples from NPX data along two
#' principal components (default PC2 vs. PC1) including the explained
#' variance and dots colored by QC_Warning using
#' \code{stats::prcomp} and \code{ggplot2::ggplot}.
#'
#' The values are by default scaled and centered in the PCA and
#' proteins with missing NPX values are by default removed from the
#' corresponding assay.
#'
#' Unique sample names are required.
#'
#' Imputation by the median is done for assays with missingness <10\%
#' for multi-plate projects and <5\% for single plate projects.
#'
#' The plot is printed, and a list of ggplot objects is returned.
#' If byPanel = TRUE, the data processing (imputation of missing
#' values etc) and subsequent PCA is performed separately per panel.
#' A faceted plot is printed, while the individual ggplot objects
#' are returned.
#'
#' The arguments outlierDefX and outlierDefY can be used to identify
#' outliers in the PCA. Samples more than +/- outlierDefX and
#' outlierDefY standard deviations from the mean of the plotted PC
#' will be labelled. Both arguments have to be specified.
#'
#' @param df data frame in long format with Sample Id, NPX and column
#' of choice for colors.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param color_g Character value indicating which column to use for
#' colors (default QC_Warning). Continuous color scale for Olink(R)
#' Sample Index (OSI) columns OSITimeToCentrifugation,
#' OSIPreparationTemperature and OSISummary is also supported.
#' @param x_val Integer indicating which principal component to plot
#' along the x-axis (default 1)
#' @param y_val Integer indicating which principal component to plot
#' along the y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with
#' SampleID (default FALSE)
#' @param drop_assays Logical. All assays with any missing values will
#' be dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values
#' will be dropped.
#' @param n_loadings Integer. Will plot the top n_loadings based on
#' size.
#' @param loadings_list Character vector indicating for which
#' OlinkID's to plot as loadings. It is possible to use n_loadings
#' and loadings_list simultaneously.
#' @param byPanel Perform the PCA per panel (default FALSE)
#' @param outlierDefX The number standard deviations along the PC
#' plotted on the x-axis that defines an outlier. See also 'Details"
#' @param outlierDefY The number standard deviations along the PC
#' plotted on the y-axis that defines an outlier. See also 'Details"
#' @param outlierLines Draw dashed lines at +/- outlierDefX and
#' outlierDefY standard deviations from the mean of the plotted PCs
#' (default FALSE)
#' @param label_outliers Use ggrepel to label samples lying outside
#' the limits set by the outlierLines (default TRUE)
#' @param quiet Logical. If TRUE, the resulting plot is not printed
#' @param verbose Logical. Whether warnings about the number of
#' samples and/or assays dropped or imputed should be printed to
#' the console.
#' @param ... coloroption passed to specify color order.
#' @return A list of objects of class "ggplot", each plot contains
#' scatter plot of PCs
#' @keywords NPX PCA
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' }
#' npx_data <- npx_data1 |>
#'   filter(!grepl("CONTROL", SampleID))
#'
#' # PCA using all the data
#' olink_pca_plot(df = npx_data, color_g = "QC_Warning")
#'
#' # PCA per panel
#' g <- olink_pca_plot(df = npx_data, color_g = "QC_Warning", byPanel = TRUE)
#' g[[2]] # Plot only the second panel
#'
#' # Label outliers
#' olink_pca_plot(
#'   df = npx_data, color_g = "QC_Warning",
#'   outlierDefX = 2, outlierDefY = 4
#' ) # All data
#' olink_pca_plot(
#'   df = npx_data, color_g = "QC_Warning",
#'   outlierDefX = 2.5, outlierDefY = 4, byPanel = TRUE
#' ) # Per panel
#'
#' # Retrieve the outliers
#' g <- olink_pca_plot(
#'   df = npx_data, color_g = "QC_Warning",
#'   outlierDefX = 2.5, outlierDefY = 4, byPanel = TRUE
#' )
#' outliers <- lapply(g, function(x) {
#'   x$data
#' }) |>
#'   bind_rows() |>
#'   filter(Outlier == 1)
#'
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

olink_pca_plot <- function(df,
                           check_log = NULL,
                           color_g = "QC_Warning",
                           x_val = 1,
                           y_val = 2,
                           label_samples = FALSE,
                           drop_assays = FALSE,
                           drop_samples = FALSE,
                           n_loadings = 0,
                           loadings_list = NULL,
                           byPanel = FALSE, # nolint object_name_linter
                           outlierDefX = NA, # nolint object_name_linter
                           outlierDefY = NA, # nolint object_name_linter
                           outlierLines = FALSE, # nolint object_name_linter
                           label_outliers = TRUE,
                           quiet = FALSE,
                           verbose = TRUE,
                           ...) {
  # checking ellipsis
  if (length(list(...)) > 0) {
    ellipsis_variables <- names(list(...))

    if (length(ellipsis_variables) == 1) {
      if (!(ellipsis_variables == "coloroption")) {
        cli::cli_abort(
          "The ... option only takes the coloroption argument. ... currently
          contains the variable {ellipsis_variables}."
        )
      }
    } else {
      cli::cli_abort(
        "The ... option only takes the coloroption argument. ... currently
        contains the variables {paste(ellipsis_variables, collapse = ', ')}."
      )
    }
  }

  # Check if check_log is correct
  check_log <- run_check_npx(df = df, check_log = check_log)

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
    convert_nonunique_uniprot = TRUE,
    out_df = "tibble",
    verbose = FALSE
  ) |>
    suppressMessages()

  # OSI checks - ran only if OSI columns selected to color
  osi_cat_cols <- c("OSICategory")
  osi_cont_cols <- c("OSITimeToCentrifugation",
                     "OSIPreparationTemperature",
                     "OSISummary")

  if (color_g %in% c(osi_cat_cols, osi_cont_cols)) {

    # Check for invalid values and NA columns
    df <- check_osi(df = df,
                    check_log = check_log,
                    osi_score = color_g)
  }

  # Check that the user didn't specify just one of outlierDefX and outlierDefY
  if (sum(c(is.numeric(outlierDefX), is.numeric(outlierDefY))) == 1) {
    cli::cli_abort(
      "To label outliers, both outlierDefX and outlierDefY have to be specified
      as numerical values."
    )
  }

  # If outlierLines == TRUE, both outlierDefX and outlierDefY have to be
  # specified
  if (outlierLines) {
    if (!all(is.numeric(outlierDefX), is.numeric(outlierDefY))) {
      cli::cli_abort(
        "outlierLines requested but boundaries not specified. To draw lines,
        both outlierDefX and outlierDefY have to be specified as numerical
        values"
      )
    }
  }

  if (byPanel) {

    # Keep continuous OSI cols unchanged; only convert other color_g to factor
    if (!color_g %in% osi_cont_cols) {
      if (!is.factor(df[[color_g]])) {
        df[[color_g]] <- as.factor(df[[color_g]])
      }
    }

    df <- df |>
      dplyr::mutate(
        Panel = stringr::str_replace(Panel, "Olink ", "") # nolint object_usage_linter
      ) # Strip "Olink" from the panel names

    plotList <- lapply(unique(df$Panel), function(x) { # nolint object_name_linter
      g <- olink_pca_plot.internal(
        df = df |> dplyr::filter(Panel == x), # nolint object_usage_linter
        color_g = color_g,
        x_val = x_val,
        y_val = y_val,
        label_samples = label_samples,
        drop_assays = drop_assays,
        drop_samples = drop_samples,
        n_loadings = n_loadings,
        loadings_list = loadings_list,
        outlierDefX = outlierDefX,
        outlierDefY = outlierDefY,
        outlierLines = outlierLines,
        label_outliers = label_outliers,
        verbose = verbose,
        ...
      ) +
        ggplot2::labs(title = x)

      # Add Panel info inside the ggplot object
      g$data <- g$data |>
        dplyr::mutate(Panel = x)

      return(g)
    })
    names(plotList) <- unique(df$Panel) # nolint object_name_linter
    if (!quiet) {
      print(ggpubr::ggarrange(
        plotlist = plotList,
        common.legend = TRUE
      ))
    }
  } else {
    pca_plot <- olink_pca_plot.internal(
      df = df,
      color_g = color_g,
      x_val = x_val,
      y_val = y_val,
      label_samples = label_samples,
      drop_assays = drop_assays,
      drop_samples = drop_samples,
      n_loadings = n_loadings,
      loadings_list = loadings_list,
      outlierDefX = outlierDefX,
      outlierDefY = outlierDefY,
      outlierLines = outlierLines,
      label_outliers = label_outliers,
      verbose = verbose,
      ...
    )
    if (!quiet) print(pca_plot)
    plotList <- list(pca_plot) # nolint object_name_linter # For consistency, return a list even when there's just one plot
  }
  return(invisible(plotList))
}


olink_calculate_pca <- function(
    procData, # nolint object_name_linter
    x_val = 1,
    y_val = 2,
    outlierDefX = NA, # nolint object_name_linter
    outlierDefY = NA # nolint object_name_linter
) {
  #### PCA ####
  pca_fit <- stats::prcomp(
    procData$df_wide_matrix,
    scale. = TRUE,
    center = TRUE
  )

  # Standardizing and selecting components
  scaling_factor_lambda <- pca_fit$sdev * sqrt(nrow(procData$df_wide_matrix))

  PCX <- pca_fit$x[, x_val] / scaling_factor_lambda[x_val] # nolint object_name_linter
  PCY <- pca_fit$x[, y_val] / scaling_factor_lambda[y_val] # nolint object_name_linter
  PoV <- pca_fit$sdev^2 / sum(pca_fit$sdev^2) # nolint object_name_linter
  LX <- pca_fit$rotation[, x_val] # nolint object_name_linter
  LY <- pca_fit$rotation[, y_val] # nolint object_name_linter


  # Sort order is dependent on locale -> set locale here to make code
  # deterministic
  old_collate <- Sys.getlocale("LC_COLLATE")
  Sys.setlocale("LC_COLLATE", "C")

  scores <- data.frame(cbind(PCX, PCY)) |>
    tibble::rownames_to_column() |>
    dplyr::arrange(rowname, .locale = "C") |> # nolint object_usage_linter
    tibble::column_to_rownames()
  loadings <- data.frame(variables = rownames(pca_fit$rotation), LX, LY)

  Sys.setlocale("LC_COLLATE", old_collate)


  range_PX <- c(-abs(min(PCX, na.rm = TRUE)), abs(max(PCX, na.rm = TRUE))) # nolint object_name_linter
  range_PY <- c(-abs(min(PCY, na.rm = TRUE)), abs(max(PCY, na.rm = TRUE))) # nolint object_name_linter
  range_LX <- c(-abs(min(LX, na.rm = TRUE)), abs(max(LX, na.rm = TRUE))) # nolint object_name_linter
  range_LY <- c(-abs(min(LY, na.rm = TRUE)), abs(max(LY, na.rm = TRUE))) # nolint object_name_linter

  loadings_scaling_factor <- 0.8 / max(range_LX / range_PX, range_LY / range_PY)

  # Identify outliers
  if (!is.na(outlierDefX) && !is.na(outlierDefY)) {
    scores <- scores |>
      tibble::rownames_to_column(var = "SampleID") |>
      dplyr::mutate(
        PCX_low = mean(PCX, na.rm = TRUE) -
          outlierDefX * sd(PCX, na.rm = TRUE),
        PCX_high = mean(PCX, na.rm = TRUE) +
          outlierDefX * sd(PCX, na.rm = TRUE),
        PCY_low = mean(PCY, na.rm = TRUE) -
          outlierDefY * sd(PCY, na.rm = TRUE),
        PCY_high = mean(PCY, na.rm = TRUE) +
          outlierDefY * sd(PCY, na.rm = TRUE)
      ) |>
      dplyr::mutate(
        Outlier = dplyr::if_else(
          PCX < PCX_high &  # nolint object_usage_linter
            PCX > PCX_low & # nolint object_usage_linter
            PCY > PCY_low & # nolint object_usage_linter
            PCY < PCY_high, # nolint object_usage_linter
          0, 1
        )
      )
  }


  return(list(
    scores = scores,
    loading = loadings,
    loadings_scaling_factor = loadings_scaling_factor,
    PoV = PoV
  ))
}


olink_pca_plot.internal <- function( # nolint object_name_linter
  df,
  color_g = "QC_Warning",
  x_val = 1,
  y_val = 2,
  label_samples = FALSE,
  drop_assays = FALSE,
  drop_samples = FALSE,
  n_loadings = 0,
  loadings_list = NULL,
  outlierDefX, # nolint object_name_linter
  outlierDefY, # nolint object_name_linter
  outlierLines, # nolint object_name_linter
  label_outliers,
  verbose = verbose,
  ...
) {
  # Ensure one unique color value per SampleID (required by
  # npxProcessing_forDimRed)
  df <- df |>
    dplyr::group_by(SampleID) |> # nolint object_usage_linter
    dplyr::mutate(
      !!rlang::sym(color_g) := dplyr::first(stats::na.omit(.data[[color_g]]))
    ) |>
    dplyr::ungroup()

  # Data pre-processing
  procData <- npxProcessing_forDimRed( # nolint object_name_linter
    df = df,
    color_g = color_g,
    drop_assays = drop_assays,
    drop_samples = drop_samples,
    verbose = verbose
  )

  # Did we drop any of the of the assays specified in the loadings_list?
  if (!is.null(loadings_list)) {
    # Dropped because of NAs
    dropped_loadings <- intersect(
      procData$dropped_assays.na,
      loadings_list
    )

    if (length(dropped_loadings) > 0) {
      if (verbose) {
        cli::cli_warn(
          "The loading(s) {paste0(dropped_loadings, collapse = ', ')} from the
          loadings_list contain NA and are dropped."
        )
      }

      loadings_list <- setdiff(loadings_list, dropped_loadings)
    }


    # Dropped because of to high missingness
    dropped_loadings <- intersect(
      procData$dropped_assays.missingness,
      loadings_list
    )

    if (length(dropped_loadings) > 0) {
      if (verbose) {
        cli::cli_warn(
          "The loading(s) {paste0(dropped_loadings, collapse = ', ')} from the
          loadings_list are dropped due to high missingness."
        )
      }

      loadings_list <- setdiff(loadings_list, dropped_loadings)
    }

    if (length(loadings_list) == 0) {
      loadings_list <- NULL
    }
  }

  #### PCA ####
  pca_results <- olink_calculate_pca(
    procData = procData,
    x_val = x_val,
    y_val = y_val,
    outlierDefX = outlierDefX,
    outlierDefY = outlierDefY
  )

  scores <- pca_results$scores

  if (!"SampleID" %in% colnames(scores)) {
    scores <- scores |>
      dplyr::mutate(SampleID = rownames(scores))
  }

  if (is.numeric(scores$SampleID)) {
    message("SampleID converted to character.")
    scores$SampleID <- as.character(scores$SampleID)
  }
  if (is.numeric(procData$df_wide$SampleID)) {
    message("SampleID converted to character.")
    procData$df_wide$SampleID <- as.character(procData$df_wide$SampleID) # nolint object_name_linter
  }

  scores <- scores |>
    dplyr::left_join(
      procData$df_wide[, c("SampleID", "colors")],
      by = c("SampleID")
    )

  loadings <- pca_results$loading
  loadings_scaling_factor <- pca_results$loadings_scaling_factor
  PoV <- pca_results$PoV # nolint object_name_linter

  # Plotting
  pca_plot <- ggplot2::ggplot(
    scores,
    ggplot2::aes(x = PCX, y = PCY) # nolint object_usage_linter
  ) +
    ggplot2::xlab(
      paste0("PC", x_val, " (", round(PoV[x_val] * 100, digits = 2), "%)")
    ) +
    ggplot2::ylab(
      paste0("PC", y_val, " (", round(PoV[y_val] * 100, digits = 2), "%)")
    )


  # Drawing scores
  if (label_samples) {
    pca_plot <- pca_plot +
      ggplot2::geom_text(
        ggplot2::aes(label = SampleID, color = colors), # nolint object_usage_linter
        size = 3
      ) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")
  } else {
    pca_plot <- pca_plot +
      ggplot2::geom_point(ggplot2::aes(color = colors), size = 2.5) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")
  }

  # Continuous color scale for OSI columns
  osi_cols <- c(
    "OSITimeToCentrifugation",
    "OSIPreparationTemperature",
    "OSISummary"
  )
  if (color_g %in% osi_cols) {
    # Ensure numeric for continuous scale (handles character/factor inputs)
    if (is.factor(scores$colors)) scores$colors <- as.character(scores$colors)
    scores$colors <- suppressWarnings(as.numeric(scores$colors))

    pca_plot <- pca_plot +
      ggplot2::scale_color_gradient(
        low = "#FFB200FF",
        high = "#332D56FF",
        limits = c(0L, 1L),
        breaks = seq(from = 0L, to = 1L, by = 0.25),
        oob = scales::squish
      )
  }

  # Drawing loadings

  if (n_loadings > 0 || !is.null(loadings_list)) {
    N_loadings <- data.frame( # nolint object_name_linter
      matrix(vector(), 0, ncol(loadings)),
      stringsAsFactors = FALSE
    )
    colnames(N_loadings) <- colnames(loadings) # nolint object_name_linter

    L_loadings <- N_loadings # nolint object_name_linter

    if (n_loadings > 0) {
      # Largest loadings based on Pythagoras

      N_loadings <- loadings |> # nolint object_name_linter
        dplyr::mutate(abs_loading = sqrt(LX^2 + LY^2)) |> # nolint object_usage_linter
        dplyr::arrange(desc(abs_loading)) |> # nolint object_usage_linter
        utils::head(n_loadings) |>
        dplyr::select(-abs_loading)
    }

    if (!is.null(loadings_list)) {
      # Selected loadings

      L_loadings <- loadings |> # nolint object_name_linter
        dplyr::filter(variables %in% loadings_list) # nolint object_usage_linter
    }

    loadings <- rbind(
      N_loadings,
      L_loadings
    ) |>
      dplyr::distinct()

    pca_plot <- pca_plot +
      ggplot2::geom_segment(
        data = loadings,
        ggplot2::aes(
          x = 0,
          y = 0,
          xend = LX * loadings_scaling_factor, # nolint object_usage_linter
          yend = LY * loadings_scaling_factor  # nolint object_usage_linter
        ),
        arrow = ggplot2::arrow(length = grid::unit(1 / 2, "picas")),
        color = "black"
      ) +
      ggrepel::geom_label_repel(
        data = loadings,
        ggplot2::aes(
          x = LX * loadings_scaling_factor,
          y = LY * loadings_scaling_factor,
          label = variables # nolint object_usage_linter
        ),
        box.padding = 1,
        show.legend = FALSE,
        segment.colour = "gray"
      )
  }

  # Label outliers in figure
  if (!is.na(outlierDefX) && !is.na(outlierDefY) && label_outliers) {
    pca_plot <- pca_plot +
      ggrepel::geom_label_repel(
        data = pca_plot$data |>
          dplyr::mutate(
            SampleIDPlot = dplyr::case_when(
              Outlier == 1 ~ SampleID,
              TRUE ~ ""
            )
          ),
        ggplot2::aes(label = SampleIDPlot), # nolint object_usage_linter
        box.padding = 0.5,
        min.segment.length = 0.1,
        show.legend = FALSE,
        size = 3
      )
  }

  # Add outlier lines
  if (outlierLines) {
    pca_plot <- pca_plot +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = PCY_low), # nolint object_usage_linter
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = PCY_high), # nolint object_usage_linter
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = PCX_low), # nolint object_usage_linter
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = PCX_high), # nolint object_usage_linter
        linetype = "dashed",
        color = "grey"
      )
  }

  pca_plot <- pca_plot +
    OlinkAnalyze::set_plot_theme()

  osi_cols <- c(
    "OSITimeToCentrifugation",
    "OSIPreparationTemperature",
    "OSISummary"
  )
  if (!(color_g %in% osi_cols)) {
    pca_plot <- pca_plot +
      OlinkAnalyze::olink_color_discrete(..., drop = FALSE)
  }

  return(pca_plot)
}
