testthat:test_that(
  "olink_heatmap_plot - works",
  {
    skip_if_not_installed("ggplotify")
    skip_if_not_installed("pheatmap")

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
      )
    )
  }
)
