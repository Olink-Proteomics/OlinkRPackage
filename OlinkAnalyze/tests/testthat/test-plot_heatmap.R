test_that(
  "olink_heatmap_plot - works",
  {
    skip_if_not_installed("ggplotify")
    skip_if_not_installed("pheatmap")
    skip_if_not_installed("vdiffr")

    # Load data with hidden/excluded assays (all NPX=NA)
    npx_data_format_oct <- get_example_data("npx_data_format-Oct-2022.rds")
    check_log_oct <- check_npx(df = npx_data_format_oct) |>
      suppressWarnings() |>
      suppressMessages()
    npx_data_format <- clean_npx(df = npx_data_format_oct,
                                 check_log = check_log_oct,
                                 verbose = FALSE) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = olink_heatmap_plot(
        df = npx_data_format,
        check_log = check_log_oct,
        variable_row_list = "treatment2"
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    npx_data <- npx_data1 |>
      dplyr::filter(
        !stringr::str_detect(
          string = .data[["SampleID"]],
          pattern = "CONT"
        )
      )
    check_log <- check_npx(df = npx_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = olink_heatmap_plot(
        df = npx_data,
        check_log = check_log,
        variable_row_list = c("Time", "Site")
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    expect_no_error(
      object = olink_heatmap_plot(
        df = npx_data,
        check_log = check_log,
        cutree_rows = 3
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )
  }
)

test_that(
  "olink_heatmap_plot - works - snapshots",
  {
    skip_if_not_installed("ggplotify")
    skip_if_not_installed("pheatmap")
    skip_if_not_installed("vdiffr")

    check_npx_data1 <- check_npx(
      df = npx_data1
    ) |>
      suppressMessages() |>
      suppressWarnings()

    npx_data1_clean <- OlinkAnalyze::clean_npx(
      df = OlinkAnalyze::npx_data1,
      check_log = check_npx_data1
    ) |>
      suppressMessages() |>
      suppressWarnings()

    check_npx_data1_clean <- OlinkAnalyze::check_npx(
      df = npx_data1_clean
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # v1 - 1 annotation on rows - auto color scale ----

    heatmap_v1 <- OlinkAnalyze::olink_heatmap_plot(
      df = npx_data1_clean,
      check_log = check_npx_data1_clean,
      variable_row_list = "Treatment"
    )

    heatmap_plot_name_v1 <- "heatmap_v1"
    # check_snap_exist(test_dir_name = "plot_heatmap",
    #                  snap_name = heatmap_plot_name_v1)
    withCallingHandlers({
      vdiffr::expect_doppelganger(
        title = heatmap_plot_name_v1,
        fig = heatmap_v1,
        cran = FALSE
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "font family 'Arial Regular' not found"))
        invokeRestart("muffleWarning")
    })

    # v2 - 2 annotations on rows - auto color scale ----

    heatmap_v2 <- OlinkAnalyze::olink_heatmap_plot(
      df = npx_data1_clean,
      check_log = check_npx_data1_clean,
      variable_row_list = c("Treatment", "Site")
    )

    heatmap_plot_name_v2 <- "heatmap_v2"
    # check_snap_exist(test_dir_name = "plot_heatmap",
    #                  snap_name = heatmap_plot_name_v2)
    withCallingHandlers({
      vdiffr::expect_doppelganger(
        title = heatmap_plot_name_v2,
        fig = heatmap_v2,
        cran = FALSE
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "font family 'Arial Regular' not found"))
        invokeRestart("muffleWarning")
    })

    # v3 - 1 annotation on rows - manual color scale ----

    heatmap_v3 <- OlinkAnalyze::olink_heatmap_plot(
      df = npx_data1_clean,
      check_log = check_npx_data1_clean,
      variable_row_list = "Treatment",
      annotation_colors = list(
        Treatment = c(
          Treated = "green",
          Untreated = "tomato"
        )
      )
    )

    heatmap_plot_name_v3 <- "heatmap_v3"
    # check_snap_exist(test_dir_name = "plot_heatmap",
    #                  snap_name = heatmap_plot_name_v3)
    withCallingHandlers({
      vdiffr::expect_doppelganger(
        title = heatmap_plot_name_v3,
        fig = heatmap_v3,
        cran = FALSE
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "font family 'Arial Regular' not found"))
        invokeRestart("muffleWarning")
    })

    # v4 - 2 annotations on rows - 1 manual and 1 auto color scale ----

    heatmap_v4 <- OlinkAnalyze::olink_heatmap_plot(
      df = npx_data1_clean,
      check_log = check_npx_data1_clean,
      variable_row_list = c("Treatment", "Site"),
      annotation_colors = list(
        Treatment = c(
          Treated = "green",
          Untreated = "tomato"
        )
      )
    )

    heatmap_plot_name_v4 <- "heatmap_v4"
    # check_snap_exist(test_dir_name = "plot_heatmap",
    #                  snap_name = heatmap_plot_name_v4)
    withCallingHandlers({
      vdiffr::expect_doppelganger(
        title = heatmap_plot_name_v4,
        fig = heatmap_v4,
        cran = FALSE
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "font family 'Arial Regular' not found"))
        invokeRestart("muffleWarning")
    })

    # v5 - 2 annotations on rows - 2 manual color scale ----

    heatmap_v5 <- OlinkAnalyze::olink_heatmap_plot(
      df = npx_data1_clean,
      check_log = check_npx_data1_clean,
      variable_row_list = c("Treatment", "Site"),
      annotation_colors = list(
        Treatment = c(
          Treated = "green",
          Untreated = "tomato"
        ),
        Site = c(
          Site_A = "blue",
          Site_B = "orange",
          Site_C = "purple",
          Site_D = "cyan",
          Site_E = "magenta"
        )
      )
    )

    heatmap_plot_name_v5 <- "heatmap_v5"
    # check_snap_exist(test_dir_name = "plot_heatmap",
    #                  snap_name = heatmap_plot_name_v5)
    withCallingHandlers({
      vdiffr::expect_doppelganger(
        title = heatmap_plot_name_v5,
        fig = heatmap_v5,
        cran = FALSE
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "font family 'Arial Regular' not found"))
        invokeRestart("muffleWarning")
    })
  }
)

