npx_data_format_oct <- get_example_data("npx_data_format-Oct-2022.rds")
check_log_oct <- check_npx(df = npx_data_format_oct) |>
  suppressWarnings() |>
  suppressMessages()
npx_data_format <- clean_npx(df = npx_data_format_oct,
                             check_log = check_log_oct,
                             verbose = FALSE) |>
  suppressWarnings() |>
  suppressMessages()

# Test plot_heatmap_check_inputs ----

test_that(
  "plot_heatmap_check_inputs - works",
  {
    expect_error(
      object = plot_heatmap_check_inputs(
        colnames = "wrong_answer"
      ),
      regexp = "`colnames` has to be \"assay\", \"oid\", or \"both\"!"
    )

    expect_null(
      object = plot_heatmap_check_inputs(
        colnames = "both"
      )
    )

    expect_warning(
      object = plot_heatmap_check_inputs(
        colnames = "both",
        mat = "1234"
      ),
      regexp = paste("Argument \"mat\" cannot be manually set in `pheatmap()`!",
                     "Ignoring!"),
      fixed = TRUE
    )

    expect_warning(
      object = plot_heatmap_check_inputs(
        colnames = "both",
        mat = "1234",
        scale = 3L
      ),
      regexp = paste("Arguments \"mat\" and \"scale\" cannot be manually set",
                     "in `pheatmap()`! Ignoring!"),
      fixed = TRUE
    )
  }
)

# Test plot_heatmap_clean_df ----

test_that(
  "plot_heatmap_clean_df - works",
  {
    # both, oid, assay ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = df_both <- plot_heatmap_clean_df(
              df = npx_data_format,
              check_log = check_log_oct,
              colnames = "both"
            ),
            regexp = "Excluding 17 assays with only \"NA\" values"
          ),
          regexp = "No column marking control assays in dataset"
        )
      )
    )

    expect_identical(
      object = df_both$both,
      expected = paste(df_both$assay, df_both$oid, sep = "_")
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = plot_heatmap_clean_df(
              df = npx_data_format,
              check_log = check_log_oct,
              colnames = "oid"
            ),
            regexp = "Excluding 17 assays with only \"NA\" values"
          ),
          regexp = "No column marking control assays in dataset"
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = plot_heatmap_clean_df(
              df = npx_data_format,
              check_log = check_log_oct,
              colnames = "assay"
            ),
            regexp = "Excluding 17 assays with only \"NA\" values"
          ),
          regexp = "No column marking control assays in dataset"
        )
      )
    )

    # assays with no war are removed

    expect_no_match(
      object = plot_heatmap_clean_df(
        df = npx_data_format |>
          dplyr::mutate(
            NPX = dplyr::if_else(
              .data[["OlinkID"]] == "OID30538",
              1,
              .data[["NPX"]]
            )
          ),
        check_log = check_log_oct,
        colnames = "assay"
      ) |>
        suppressMessages() |>
        suppressWarnings() |>
        dplyr::pull(.data[["oid"]]) |>
        unique(),
      "OID30538"
    )
  }
)

# Test plot_heatmap_df_to_wide ----

test_that(
  "plot_heatmap_df_to_wide - works",
  {
    npx_data_format_clean <- plot_heatmap_clean_df(
      df = npx_data_format,
      check_log = check_log_oct,
      colnames = "assay"
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = npx_data_format_wide <- plot_heatmap_df_to_wide(
            df = npx_data_format_clean,
            check_log = check_log_oct,
            colnames = "assay"
          )
        )
      )
    )

    expect_identical(
      object = colnames(npx_data_format_wide) |> sort(),
      expected = npx_data_format_clean$assay |> unique() |> sort()
    )

    expect_identical(
      object = rownames(npx_data_format_wide) |> sort(),
      expected = npx_data_format_clean$SampleID |> unique() |> sort()
    )

    expect_equal(
      object = npx_data_format_wide |>
        dplyr::select(
          dplyr::all_of("HIF1A")
        ) |>
        tibble::rownames_to_column(
          var = "SampleID"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]]
        ) |>
        tibble::column_to_rownames(
          var = "SampleID"
        ) |>
        dplyr::pull(
          .data[["HIF1A"]]
        ),
      expected = npx_data_format_clean |>
        dplyr::filter(
          .data[["assay"]] == "HIF1A"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]]
        ) |>
        dplyr::pull(
          .data[["NPX"]]
        ),
      tolerance = 1e-5
    )

    expect_true(
      object = sapply(npx_data_format_wide, is.numeric) |> all()
    )
  }
)

# Test plot_heatmap_pheatmap_args ----

test_that(
  "plot_heatmap_pheatmap_args - works",
  {
    npx_data_format_clean <- plot_heatmap_clean_df(
      df = npx_data_format,
      check_log = check_log_oct,
      colnames = "assay"
    ) |>
      suppressMessages() |>
      suppressWarnings()
    npx_data_format_wide <- plot_heatmap_df_to_wide(
      df = npx_data_format_clean,
      check_log = check_log_oct,
      colnames = "assay"
    )

    expect_equal(
      object = plot_heatmap_pheatmap_args(
        df_wide = npx_data_format_wide,
        df = npx_data_format_clean,
        check_log = check_log_oct,
        variable_col_list = NULL,
        variable_row_list = NULL,
        center_scale = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        colnames = "assay",
        fontsize = 10,
        na_col = "black"
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10
      )
    )

    expect_equal(
      object = plot_heatmap_pheatmap_args(
        df_wide = npx_data_format_wide,
        df = npx_data_format_clean,
        check_log = check_log_oct,
        variable_row_list = c("treatment2"),
        variable_col_list = c("Assay_Warning"),
        center_scale = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        colnames = "assay",
        fontsize = 10,
        na_col = "black"
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10,
        annotation_row = npx_data_format_clean |>
          dplyr::select(
            dplyr::all_of(
              c("SampleID", "treatment2")
            )
          ) |>
          dplyr::distinct() |>
          tibble::column_to_rownames(
            var = "SampleID"
          ),
        annotation_col = npx_data_format_clean |>
          dplyr::select(
            dplyr::all_of(
              c("assay", "Assay_Warning")
            )
          ) |>
          dplyr::distinct() |>
          tibble::column_to_rownames(
            var = "assay"
          ),
        annot_col_int = list(
          Assay_Warning = olink_pal()(5L)[1L:2L],
          treatment2 = olink_pal()(5L)[3L:5L]
        )
      )
    )

    expect_equal(
      object = plot_heatmap_pheatmap_args(
        df_wide = npx_data_format_wide,
        df = npx_data_format_clean,
        check_log = check_log_oct,
        variable_col_list = NULL,
        variable_row_list = NULL,
        center_scale = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        colnames = "assay",
        fontsize = 10,
        na_col = "black",
        cuttree_rows = 3L
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10,
        cuttree_rows = 3L
      )
    )
  }
)

# Test plot_heatmap_pheatmap_args ----

test_that(
  "pheatmap_extract_ellipsis_arg - works",
  {
    npx_data_format_clean <- plot_heatmap_clean_df(
      df = npx_data_format,
      check_log = check_log_oct,
      colnames = "assay"
    ) |>
      suppressMessages() |>
      suppressWarnings()
    npx_data_format_wide <- plot_heatmap_df_to_wide(
      df = npx_data_format_clean,
      check_log = check_log_oct,
      colnames = "assay"
    )

    expect_equal(
      object = pheatmap_extract_ellipsis_arg(
        list(
          mat = npx_data_format_wide,
          scale = "column",
          silent = TRUE,
          cluster_rows = TRUE,
          cluster_cols = TRUE,
          na_col = "black",
          show_rownames = TRUE,
          show_colnames = TRUE,
          annotation_legend = TRUE,
          fontsize = 10
        ),
        annotation_colors = "something"
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10,
        annot_col_int = "something"
      )
    )

    expect_equal(
      object = pheatmap_extract_ellipsis_arg(
        list(
          mat = npx_data_format_wide,
          scale = "column",
          silent = TRUE,
          cluster_rows = TRUE,
          cluster_cols = TRUE,
          na_col = "black",
          show_rownames = TRUE,
          show_colnames = TRUE,
          annotation_legend = TRUE,
          fontsize = 10
        ),
        annotation_colors = "something",
        cuttree_rows = 3L
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10,
        cuttree_rows = 3L,
        annot_col_int = "something"
      )
    )
  }
)

# Test pheatmap_annotate_heatmap ----

test_that(
  "pheatmap_annotate_heatmap - works",
  {
    npx_data_format_clean <- plot_heatmap_clean_df(
      df = npx_data_format,
      check_log = check_log_oct,
      colnames = "assay"
    ) |>
      suppressMessages() |>
      suppressWarnings()
    npx_data_format_wide <- plot_heatmap_df_to_wide(
      df = npx_data_format_clean,
      check_log = check_log_oct,
      colnames = "assay"
    )

    expect_equal(
      object = pheatmap_annotate_heatmap(
        df = npx_data_format_clean,
        check_log = check_log_oct,
        colnames = "assay",
        pheatmap_args = list(
          mat = npx_data_format_wide,
          scale = "column",
          silent = TRUE,
          cluster_rows = TRUE,
          cluster_cols = TRUE,
          na_col = "black",
          show_rownames = TRUE,
          show_colnames = TRUE,
          annotation_legend = TRUE,
          fontsize = 10
        ),
        variable_row_list = c("treatment2"),
        variable_col_list = c("Assay_Warning")
      ),
      list(
        mat = npx_data_format_wide,
        scale = "column",
        silent = TRUE,
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        na_col = "black",
        show_rownames = TRUE,
        show_colnames = TRUE,
        annotation_legend = TRUE,
        fontsize = 10,
        annotation_row = npx_data_format_clean |>
          dplyr::select(
            dplyr::all_of(
              c("SampleID", "treatment2")
            )
          ) |>
          dplyr::distinct() |>
          tibble::column_to_rownames(
            var = "SampleID"
          ),
        annotation_col = npx_data_format_clean |>
          dplyr::select(
            dplyr::all_of(
              c("assay", "Assay_Warning")
            )
          ) |>
          dplyr::distinct() |>
          tibble::column_to_rownames(
            var = "assay"
          )
      )
    )
  }
)
