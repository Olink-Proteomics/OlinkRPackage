#' Function to make a UMAP plot from the data
#'
#' @description
#' Computes a manifold approximation and projection using umap::umap and plots
#' the two specified components. Unique sample names are required and imputation
#' by the median is done for assays with missingness <10\% for multi-plate
#' projects and <5\% for single plate projects.
#'
#' @details
#' The plot is printed, and a list of ggplot objects is returned.
#'
#' If byPanel = TRUE, the data processing (imputation of missing values etc) and
#' subsequent UMAP is performed separately per panel. A faceted plot is printed,
#' while the individual ggplot objects are returned. The arguments outlierDefX
#' and outlierDefY can be used to identify outliers in the UMAP results. Samples
#' more than +/- outlierDefX and outlierDefY standard deviations from the mean
#' of the plotted UMAP component will be labelled. Both arguments have to be
#' specified.
#'
#' NOTE: UMAP is a non-linear data transformation that might not accurately
#' preserve the properties of the data. Distances in the UMAP plane should
#' therefore be interpreted with caution.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice
#' for colors.
#' @param color_g Character value indicating which column to use for colors
#' (default QC_Warning)
#' @param x_val Integer indicating which UMAP component to plot along the x-axis
#' (default 1)
#' @param y_val Integer indicating which UMAP component to plot along the y-axis
#' (default 2)
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param label_samples Logical. If TRUE, points are replaced with SampleID
#' (default FALSE)
#' @param config object of class umap.config, specifying the parameters for the
#' UMAP algorithm (default umap::umap.defaults)
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
#'
#' @return A list of objects of class "ggplot", each plot contains scatter plot
#' of UMAPs
#'
#' @keywords NPX UMAP
#'
#' @export
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("umap"))) {
#'
#'   npx_data <- npx_data1 |>
#'     dplyr::mutate(
#'       SampleID = paste(.data[["SampleID"]], "_", .data[["Index"]], sep = "")
#'     )
#'
#'   # UMAP using all the data
#'   OlinkAnalyze::olink_umap_plot(
#'     df = npx_data,
#'     color_g = "QC_Warning"
#'   )
#'
#'   # UMAP per panel
#'   g <- OlinkAnalyze::olink_umap_plot(
#'     df = npx_data,
#'     color_g = "QC_Warning",
#'     byPanel = TRUE
#'   )
#'   # Plot only the Inflammation panel
#'   g$Inflammation
#'
#'   # Label outliers
#'   OlinkAnalyze::olink_umap_plot(
#'     df = npx_data,
#'     color_g = "QC_Warning",
#'     outlierDefX = 2L,
#'     outlierDefY = 4L
#'   )
#'
#'   OlinkAnalyze::olink_umap_plot(
#'     df = npx_data,
#'     color_g = "QC_Warning",
#'     outlierDefX = 3L,
#'     outlierDefY = 2L,
#'     byPanel = TRUE
#'   )
#'
#'   # Retrieve outliers
#'   p <- OlinkAnalyze::olink_umap_plot(
#'     df = npx_data,
#'     color_g = "QC_Warning",
#'     outlierDefX = 3L,
#'     outlierDefY = 2L,
#'     byPanel = TRUE
#'   )
#'   outliers <- lapply(p, function(x) x$data) |>
#'     dplyr::bind_rows() |>
#'     dplyr::filter(
#'       .data[["Outlier"]] == 1L
#'     )
#' }
#' }
#'
olink_umap_plot <- function(df,
                            color_g = "QC_Warning",
                            x_val = 1L,
                            y_val = 2L,
                            check_log = NULL,
                            config = NULL,
                            label_samples = FALSE,
                            drop_assays = FALSE,
                            drop_samples = FALSE,
                            byPanel = FALSE, # nolint: object_name_linter
                            outlierDefX = NA, # nolint: object_name_linter
                            outlierDefY = NA, # nolint: object_name_linter
                            outlierLines = FALSE, # nolint: object_name_linter
                            label_outliers = TRUE,
                            quiet = FALSE,
                            verbose = TRUE,
                            ...) {
  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("umap"),
    call = rlang::caller_env()
  )

  if (is.null(config)) {
    config <- umap::umap.defaults
  }

  # checking ellipsis
  if (length(list(...)) > 0L) {
    ellipsis_variables <- names(list(...))
    if (length(ellipsis_variables) == 1L) {
      if (!(ellipsis_variables == "coloroption")) {
        cli::cli_abort(
          c(
            "x" = "The {.arg ...} option only takes the coloroption argument.
            {.arg ...} currently contains the variable
            {.val {ellipsis_variables}}."
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )
      }
    } else {
      cli::cli_abort(
        c(
          "x" = "The {.arg ...} option only takes one argument. {.arg ...}
          currently contains the variables {.val {ellipsis_variables}}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }
  }

  # Filtering on valid OlinkID
  df <- df |>
    dplyr::filter(
      stringr::str_detect(
        string = .data[["OlinkID"]],
        pattern = "OID[0-9]{5}"
      )
    )

  # Check data format
  check_log <- run_check_npx(df = df, check_log = check_log)

  # Exclude assays that have all NA:s
  df <- df |>
    dplyr::filter(
      !(.data[["OlinkID"]] %in% check_log$assay_na)
    )

  # Check that the user didn't specify just one of outlierDefX and outlierDefY
  if (sum(c(is.numeric(outlierDefX), is.numeric(outlierDefY))) == 1L) {
    stop(
      paste("To label outliers, both outlierDefX and outlierDefY have to be",
            "specified as numerical values")
    )
  }

  # If outlierLines=TRUE, both outlierDefX and outlierDefY have to be specified
  if (outlierLines) {
    if (!all(is.numeric(outlierDefX), is.numeric(outlierDefY))) {
      stop(
        paste("outlierLines requested but boundaries not specified. To draw",
              "lines, both outlierDefX and outlierDefY have to be specified as",
              "numerical values")
      )
    }
  }

  if (byPanel) {
    # Convert color_g variable to factor
    if (!is.factor(df[[paste(color_g)]])) {
      df[[paste(color_g)]] <- as.factor(df[[paste(color_g)]])
    }

    # Strip "Olink" from the panel names
    df <- df |>
      dplyr::mutate(
        Panel = stringr::str_replace(string = .data[["Panel"]],
                                     replacement = "Olink ",
                                     pattern = "")
      )

    plotList <- lapply(unique(df$Panel), function(x) { # nolint: object_name_linter
      g <- df |>
        dplyr::filter(
          .data[["Panel"]] == .env[["x"]]
        ) |>
        olink_umap_plot.internal(
          color_g = color_g,
          x_val = x_val,
          y_val = y_val,
          check_log = check_log,
          label_samples = label_samples,
          config = config,
          drop_assays = drop_assays,
          drop_samples = drop_samples,
          outlierDefX = outlierDefX,
          outlierDefY = outlierDefY,
          outlierLines = outlierLines,
          label_outliers = label_outliers,
          verbose = verbose,
          ...
        ) +
        ggplot2::labs(
          title = x
        )

      #Add Panel info inside the ggplot object
      g$data <- g$data |>
        dplyr::mutate(
          Panel = .env[["x"]]
        )

      return(g)
    })
    names(plotList) <- unique(df$Panel) # nolint: object_name_linter

    if (quiet == FALSE) {
      print(ggpubr::ggarrange(plotlist = plotList, common.legend = TRUE))
    }

  } else {
    umap_plot <- olink_umap_plot.internal(
      df = df,
      color_g = color_g,
      x_val = x_val,
      y_val = y_val,
      check_log = check_log,
      label_samples = label_samples,
      config = config,
      drop_assays = drop_assays,
      drop_samples = drop_samples,
      outlierDefX = outlierDefX,
      outlierDefY = outlierDefY,
      outlierLines = outlierLines,
      label_outliers = label_outliers,
      verbose = verbose,
      ...
    )

    if (quiet == FALSE) {
      print(umap_plot)
    }

    # For consistency, return a list even when there's just one plot
    plotList <- list(umap_plot) # nolint: object_name_linter
  }

  return(invisible(plotList))
}

olink_umap_plot.internal <- function(df, # nolint: object_name_linter
                                     color_g,
                                     x_val,
                                     y_val,
                                     check_log = NULL,
                                     label_samples,
                                     config,
                                     drop_assays,
                                     drop_samples,
                                     outlierDefX, # nolint: object_name_linter
                                     outlierDefY, # nolint: object_name_linter
                                     outlierLines, # nolint: object_name_linter
                                     label_outliers,
                                     verbose = TRUE,
                                     ...) {

  ### Data pre-processing ###
  procData <- npxProcessing_forDimRed( # nolint: object_name_linter
    df = df,
    check_log = check_log,
    color_g = color_g,
    drop_assays = drop_assays,
    drop_samples = drop_samples,
    verbose = verbose
  )

  #### UMAP ####
  #Determine number of UMAP components
  n_components <- config$n_components
  if (max(c(x_val, y_val)) > n_components) {
    n_components <- max(c(x_val, y_val))
  }

  umap_fit <- umap::umap(
    d = procData$df_wide_matrix,
    config = config,
    n_components = n_components
  )

  umapX <- umap_fit$layout[, x_val] # nolint: object_name_linter
  umapY <- umap_fit$layout[, y_val] # nolint: object_name_linter
  observation_names <- procData$df_wide[["SampleID"]]
  observation_colors <- procData$df_wide[["colors"]]
  scores <- data.frame(umapX, umapY)

  #Identify outliers
  if (!is.na(outlierDefX) && !is.na(outlierDefY)) {
    scores <- scores |>
      tibble::rownames_to_column(
        var = "SampleID"
      ) |>
      dplyr::mutate(
        umapX_low = mean(x = .data[["umapX"]], na.rm = TRUE) -
          .env[["outlierDefX"]] * sd(x = .data[["umapX"]], na.rm = TRUE),
        umapX_high = mean(x = .data[["umapX"]], na.rm = TRUE) +
          .env[["outlierDefX"]] * sd(x = .data[["umapX"]], na.rm = TRUE),
        umapY_low = mean(x = .data[["umapY"]], na.rm = TRUE) -
          .env[["outlierDefY"]] * sd(x = .data[["umapY"]], na.rm = TRUE),
        umapY_high = mean(x = .data[["umapY"]], na.rm = TRUE) +
          .env[["outlierDefY"]] * sd(x = .data[["umapY"]], na.rm = TRUE)
      ) |>
      dplyr::mutate(
        Outlier = dplyr::if_else(
          .data[["umapX"]] < .data[["umapX_high"]] &
            .data[["umapX"]] > .data[["umapX_low"]] &
            .data[["umapY"]] > .data[["umapY_low"]] &
            .data[["umapY"]] < .data[["umapY_high"]],
          0L,
          1L
        )
      )
  }

  #### Plotting ####
  umap_plot <- ggplot2::ggplot(
    data = scores,
    mapping = ggplot2::aes(
      x = .data[["umapX"]],
      y = .data[["umapY"]]
    )
  ) +
    ggplot2::xlab(
      paste0("UMAP", x_val)
    ) +
    ggplot2::ylab(
      paste0("UMAP", y_val)
    )

  # Drawing scores

  if (label_samples) {
    umap_plot <- umap_plot +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = .env[["observation_names"]],
          color = .env[["observation_colors"]]
        ),
        size = 3L
      )
  } else {
    umap_plot <- umap_plot +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          color = .env[["observation_colors"]]
        ),
        size = 2.5
      )
  }

  umap_plot <- umap_plot +
    ggplot2::labs(
      color = color_g
    ) +
    ggplot2::guides(
      size = "none"
    )

  # Label outliers in figure
  if (!is.na(outlierDefX) && !is.na(outlierDefY) && label_outliers) {
    umap_plot <- umap_plot +
      ggrepel::geom_label_repel(
        data = umap_plot$data |>
          dplyr::mutate(
            SampleIDPlot = dplyr::if_else(
              .data[["Outlier"]] == 1L,
              .data[["SampleID"]],
              ""
            )
          ),
        mapping = ggplot2::aes(
          label = .data[["SampleIDPlot"]]
        ),
        box.padding = 0.5,
        min.segment.length = 0.1,
        show.legend = FALSE,
        size = 3L
      )
  }

  # Add outlier lines
  if (outlierLines) {
    umap_plot <- umap_plot +
      ggplot2::geom_hline(
        mapping = ggplot2::aes(
          yintercept = .data[["umapY_low"]]
        ),
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_hline(
        mapping = ggplot2::aes(
          yintercept = .data[["umapY_high"]]
        ),
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = .data[["umapX_low"]]
        ),
        linetype = "dashed",
        color = "grey"
      ) +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = .data[["umapX_high"]]
        ),
        linetype = "dashed",
        color = "grey"
      )
  }

  umap_plot <- umap_plot +
    OlinkAnalyze::set_plot_theme() +
    OlinkAnalyze::olink_color_discrete(..., drop = FALSE)

  return(umap_plot)
}
