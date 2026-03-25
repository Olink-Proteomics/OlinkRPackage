set.seed(10)

# Load reference results
ref_results <- get_example_data("reference_results.rds")

# Load data with unique SampleID
npx_data1_uniqueid <- npx_data1 |>
  dplyr::mutate(
    SampleID = paste(.data[["SampleID"]], "_", Index, sep = "")
  )

check_log_uniqueid <- check_npx(npx_data1_uniqueid) |>
  suppressMessages() |>
  suppressWarnings()

# Load data with unique SampleID and complete treatment
npx_data1_treatment <- npx_data1 |>
  dplyr::mutate(
    SampleID = paste(.data[["SampleID"]], "_", Index, sep = "")
  ) |>
  dplyr::filter(!is.na(.data[["Treatment"]]))

check_log_treatment <- check_npx(npx_data1_treatment) |>
  suppressMessages() |>
  suppressWarnings()

test_that(
  "olink_pca_plot - works - OSI",
  {
    skip_if_not_installed(pkg = c("ggrepel"))

    # Load OSI data
    osi_data <- get_example_data("example_osi_data.rds")

    # ----------------------------
    # OSICategory invalid value
    # ----------------------------
    df_bad_cat <- osi_data |>
      dplyr::mutate(
        OSICategory = dplyr::if_else(
          dplyr::row_number() == 1L, "7", as.character(.data[["OSICategory"]])
        )
      )
    df_bad_cat_check <- check_npx(df = df_bad_cat) |>
      suppressMessages() |>
      suppressWarnings()

    # No error when not using OSI column
    expect_no_error(
      object = expect_no_message(
        object = expect_no_warning(
          object = olink_pca_plot(
            df = df_bad_cat,
            color_g = "QC_Warning",
            check_log = df_bad_cat_check
          )
        )
      )
    )

    expect_error(
      object = expect_no_message(
        object = expect_no_warning(
          object = olink_pca_plot(
            df = df_bad_cat,
            color_g = "OSICategory",
            check_log = df_bad_cat_check
          )
        )
      ),
      regexp = "Invalid values detected in column \"OSICategory\" of `df`!"
    )

    # ----------------------------
    # OSICategory all NA
    # ----------------------------
    df_cat_all_na <- osi_data |>
      dplyr::mutate(OSICategory = NA)

    expect_error(
      object = olink_pca_plot(
        df = df_cat_all_na,
        color_g = "OSICategory",
        check_log = check_npx(df = df_cat_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste(
        "All values are 'NA' in the column \"OSICategory\" of the",
        "dataset `df`!"
      )
    )

    # ------------------------------------------
    # Continuous OSI column non-numeric value
    # ------------------------------------------
    df_bad_cont_nonnum <- osi_data |>
      dplyr::mutate(
        OSITimeToCentrifugation = dplyr::if_else(
          dplyr::row_number() == 1L,
          "oops",
          as.character(.data[["OSITimeToCentrifugation"]])
        )
      )

    expect_error(
      object = olink_pca_plot(
        df = df_bad_cont_nonnum,
        color_g = "OSITimeToCentrifugation",
        check_log = check_npx(df = df_bad_cont_nonnum) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste(
        "Non-numeric values detected in column",
        "\"OSITimeToCentrifugation\" of `df`!"
      )
    )

    # ----------------------------
    # Continuous OSI column all NA
    # ----------------------------
    df_cont_all_na <- osi_data |>
      dplyr::mutate(OSISummary = NA)

    expect_error(
      object = olink_pca_plot(
        df = df_cont_all_na,
        color_g = "OSISummary",
        check_log = check_npx(df = df_cont_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste(
        "All values are 'NA' in the column \"OSISummary\" of the",
        "dataset `df`!"
      )
    )

    # ------------------------------------------------------
    # Valid OSI values should NOT trigger OSI error strings
    # ------------------------------------------------------

    check_log <- check_npx(df = osi_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_pca_plot(
            df = osi_data,
            color_g = "OSICategory",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_pca_plot(
            df = osi_data,
            color_g = "OSISummary",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_pca_plot(
            df = osi_data,
            color_g = "OSIPreparationTemperature",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_pca_plot(
            df = osi_data,
            color_g = "OSITimeToCentrifugation",
            check_log = check_log
          )
        )
      )
    )
  }
)

test_that(
  "olink_pca_plot - works",
  {
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("ggrepel"))

    # -------------------------------
    # Test dropped assays and samples
    # -------------------------------
    expect_warning(
      object = expect_warning(
        object = olink_pca_plot(
          df = npx_data1_uniqueid,
          check_log = check_log_uniqueid,
          color_g = "QC_Warning",
          drop_assays = TRUE,
          drop_samples = TRUE,
          quiet = TRUE
        ),
        regexp = "160 samples contain NA and are dropped."
      ),
      regexp = "0 assays contain NA and are dropped."
    )

    # -----------------
    # Test PCA by Panel
    # -----------------
    expect_no_error(
      object = pca_plot_by_panel <- olink_pca_plot(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        color_g = "QC_Warning",
        byPanel = TRUE,
        quiet = TRUE
      )
    )

    # -------------------------------
    # Test outliers samples and panel
    # -------------------------------
    expect_no_error(
      object = pca_plot_outliers <- olink_pca_plot(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        color_g = "QC_Warning",
        byPanel = TRUE,
        outlierDefX = 4,
        outlierDefY = 2.5,
        quiet = TRUE
      )
    )

    expect_equal(
      object = lapply(pca_plot_outliers, function(x) {
        return(x$data)
      }) |>
        dplyr::bind_rows() |>
        dplyr::filter(Outlier == 1L) |>
        dplyr::select(dplyr::all_of(c("SampleID", "Panel"))) |>
        dplyr::as_tibble(),
      expected = dplyr::tibble(
        SampleID = c("B4_83", "A14_15", "A15_16", "A19_21"),
        Panel = c("Cardiometabolic", rep("Inflammation", 3L))
      )
    )
  }
)

test_that(
  "olink_pca_plot - works - vdiffr",
  {
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("vdiffr"))
    skip_if_not_installed(pkg = c("ggrepel"))
    skip_on_cran()

    # --------
    # PCA plot
    # --------
    expect_no_message(
      object = pca_plot <- olink_pca_plot(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        quiet = TRUE
      )
    )

    pca_plot_name <- "PCA plot"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_name,
      pca_plot[[1]]
    )

    # ------------------------------------
    # PCA plot with dropped assays/samples
    # ------------------------------------
    expect_warning(
      object = expect_warning(
        object = pca_plot_drop <- olink_pca_plot(
          df = npx_data1_uniqueid,
          check_log = check_log_uniqueid,
          color_g = "QC_Warning",
          drop_assays = TRUE,
          drop_samples = TRUE,
          quiet = TRUE
        ),
        regexp = "160 samples contain NA and are dropped."
      ),
      regexp = "0 assays contain NA and are dropped."
    )

    pca_plot_drop_name <- "PCA plot drop_assays and drop_samples"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_drop_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_drop_name,
      pca_plot_drop[[1]]
    )

    # -----------------
    # PCA plot by panel
    # -----------------
    expect_no_message(
      object = pca_plot_by_panel <- olink_pca_plot(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        color_g = "QC_Warning",
        byPanel = TRUE,
        quiet = TRUE
      )
    )

    pca_plot_by_panel_name1 <- "PCA plot panel 1"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_by_panel_name1
    )
    vdiffr::expect_doppelganger(
      pca_plot_by_panel_name1,
      pca_plot_by_panel[[1]]
    )

    pca_plot_by_panel_name2 <- "PCA plot panel 2"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_by_panel_name2
    )
    vdiffr::expect_doppelganger(
      pca_plot_by_panel_name2,
      pca_plot_by_panel[[2]]
    )

    # ---------------------
    # PCA plot by treatment
    # ---------------------
    expect_no_message(
      object = pca_plot_treatment <- olink_pca_plot(
        df = npx_data1_treatment,
        check_log = check_log_treatment,
        color_g = "Treatment",
        quiet = TRUE
      )
    )

    pca_plot_treat_name <- "PCA plot color by treatment"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_treat_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_treat_name,
      pca_plot_treatment[[1]]
    )

    # PCA plot by treatment and loading
    expect_no_message(
      object = pca_plot_treat_top_loading <- olink_pca_plot(
        df = npx_data1_treatment,
        check_log = check_log_treatment,
        color_g = "Treatment",
        loadings_list = {
          ref_results$t_test |>
            utils::head(5) |>
            dplyr::pull(.data[["OlinkID"]])
        },
        quiet = TRUE
      )
    )

    # pca_plot_treat_top_loadings_name <- "PCA plot with loadings"
    # check_snap_exist(
    #   test_dir_name = "pca_plot",
    #   snap_name = pca_plot_treat_top_loadings_name
    # )
    # vdiffr::expect_doppelganger(
    #   pca_plot_treat_top_loadings_name,
    #   pca_plot_treat_top_loading[[1]]
    # )
  }
)

test_that(
  "olink_pca_plot.internal - works",
  {
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("vdiffr"))
    skip_if_not_installed(pkg = c("ggrepel"))
    skip_on_cran()

    # -----------------
    # PCA plot internal
    # -----------------
    expect_no_message(
      object = pca_plot_internal <- olink_pca_plot.internal(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        color_g = "QC_Warning",
        outlierDefX = NA,
        outlierDefY = NA,
        label_outliers = FALSE,
        outlierLines = FALSE,
        verbose = TRUE
      )
    )

    pca_plot_internal_name <- "PCA plot internal"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_internal_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_internal_name,
      pca_plot_internal
    )

    # ------------------------------
    # PCA plot internal with outlier
    # ------------------------------
    # Note: PCA plot internal 2 - 4 are redudent

    # PCA plot internal 2
    expect_no_message(
      object = pca_plot_internal_outlier <- olink_pca_plot.internal(
        df = npx_data1_uniqueid,
        check_log = check_log_uniqueid,
        color_g = "QC_Warning",
        outlierDefX = NA,
        outlierDefY = NA,
        label_outliers = TRUE,
        outlierLines = FALSE,
        verbose = TRUE
      )
    )

    pca_plot_internal_outlier_name <- "PCA plot internal 2"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_internal_outlier_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_internal_outlier_name,
      pca_plot_internal_outlier
    )

    # PCA plot internal 3
    pca_plot_internal_outlier_name <- "PCA plot internal 3"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_internal_outlier_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_internal_outlier_name,
      pca_plot_internal_outlier
    )

    # PCA plot internal 4
    pca_plot_internal_outlier_name <- "PCA plot internal 4"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_internal_outlier_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_internal_outlier_name,
      pca_plot_internal_outlier
    )
  }
)


# PCA calculation output order 1
# With set local

old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")

pca_outside <- npx_data1_uniqueid |>
  npxProcessing_forDimRed(check_log = check_log_uniqueid) |>
  olink_calculate_pca()

locale_outside <- Sys.getlocale(category = "LC_ALL")
Sys.setlocale("LC_COLLATE", old_collate)

test_that(
  "olink_calculate_pca - works - output order",
  {
    skip_if_not(file.exists(test_path("_snaps", "pca_plot.md")))
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("vdiffr"))
    skip_if_not_installed(pkg = c("ggrepel"))
    skip_on_cran()

    # ------------------------------
    # PCA calculation output order 2
    # ------------------------------
    pca <- npx_data1_uniqueid |>
      npxProcessing_forDimRed(check_log = check_log_uniqueid) |>
      olink_calculate_pca()

    locale_inside <- Sys.getlocale(category = "LC_ALL")

    expect_equal(
      object = locale_outside,
      expected = locale_inside
    )

    expect_equal(
      object = rownames(pca$scores),
      expected = rownames(pca_outside$scores)
    )

    expect_snapshot_value(pca$scores, style = "deparse")
    expect_snapshot_value(pca$loadings, style = "deparse")
  }
)

# Without set local
pca_outside <- npx_data1_uniqueid |>
  npxProcessing_forDimRed(check_log = check_log_uniqueid) |>
  olink_calculate_pca()

test_that(
  "PCA calculation - output order 2 & output values",
  {
    pca <- npx_data1_uniqueid |>
      npxProcessing_forDimRed(check_log = check_log_uniqueid) |>
      olink_calculate_pca()

    expect_equal(
      object = rownames(pca$scores),
      expected = rownames(pca_outside$scores)
    )

    expect_equal(
      object = pca$loadings_scaling_factor,
      expected = pca_outside$loadings_scaling_factor
    )

    expect_equal(
      object = pca$loadings,
      expected = pca_outside$loadings
    )

    expect_equal(
      object = pca$PoV,
      expected = pca_outside$PoV
    )

    expect_equal(
      object = pca$scores,
      expected = pca_outside$scores
    )
  }
)

test_that(
  "PCA basic plotting",
  {
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("vdiffr"))
    skip_if_not_installed(pkg = c("ggrepel"))
    skip_on_cran()

    pca_input <- npx_data1_uniqueid |>
      npxProcessing_forDimRed(check_log = check_log_uniqueid) |>
      olink_calculate_pca()

    pca_basic_plot <- pca_input$scores |>
      ggplot2::ggplot(mapping = ggplot2::aes(x = PCX, y = PCY)) +
      ggplot2::geom_point()

    pca_basic_plot_name <- "PCA basic plotting"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_basic_plot_name
    )
    vdiffr::expect_doppelganger(
      pca_basic_plot_name,
      pca_basic_plot
    )
  }
)

test_that(
  "Minimal PCA plot",
  {
    skip_if_not_installed(pkg = "ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed(pkg = c("vdiffr"))
    skip_if_not_installed(pkg = c("ggrepel"))
    skip_on_cran()

    pca_plot <- npx_data1_uniqueid |>
      olink_pca_plot(
        check_log = check_log_uniqueid,
        label_outliers = FALSE
      )

    pca_mini_plot_name <- "PCA plot - not label outliers"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_mini_plot_name
    )
    vdiffr::expect_doppelganger(
      pca_mini_plot_name,
      pca_plot[[1]]
    )

    pca_plot_outliers <- npx_data1_uniqueid |>
      olink_pca_plot(
        check_log = check_log_uniqueid,
        label_outliers = TRUE
      )

    pca_plot_outliers_name <- "PCA plot - label outliers"
    check_snap_exist(
      test_dir_name = "pca_plot",
      snap_name = pca_plot_outliers_name
    )
    vdiffr::expect_doppelganger(
      pca_plot_outliers_name,
      pca_plot_outliers[[1]]
    )

    # Remove index
    npx_data_reindex <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste(SampleID, "_", Index, sep = ""),
        Index = dplyr::if_else(
          Panel == "Olink Cardiometabolic", Index + 1L, Index
        )
      )
    check_log_reindex <- check_npx(npx_data_reindex) |>
      suppressMessages() |>
      suppressWarnings()

    pca_plot_reindex <- olink_pca_plot(
      df = npx_data_reindex,
      check_log = check_log_reindex
    )

    expect_true(all(
      abs(sort(pca_plot_reindex[[1]]$data$PCX) -
            sort(pca_plot[[1]]$data$PCX)) == 0L
    ))

    expect_true(all(
      abs(sort(pca_plot_reindex[[1]]$data$PCY) -
            sort(pca_plot[[1]]$data$PCY)) == 0L
    ))
  }
)

# prcomp
c <- chol(s <- toeplitz(.9^(0:31))) # Cov.matrix and its root
set.seed(17)
x <- matrix(rnorm(32000), 1000, 32)
z <- x %*% c ## ==>  cov(Z) ~=  C'C = S

p_z_outside <- prcomp(z, tol = 0.1)

test_that(
  "prcomp - works",
  {
    p_z_inside <- prcomp(z, tol = 0.1)

    expect_equal(
      object = p_z_outside,
      expected = p_z_inside
    )
  }
)

# PCA calculation
# local
old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")
Sys.setlocale("LC_COLLATE", old_collate)

# inside
test_that(
  "PCA calculation",
  {
    locale_inside <- Sys.getlocale(category = "LC_ALL")

    expect_equal(
      object = locale_outside,
      expected = locale_inside
    )
  }
)
