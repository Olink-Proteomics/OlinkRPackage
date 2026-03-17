skip_if_not_installed("ggplotify")
npx_data_format_oct <- get_example_data(filename =
                                          "npx_data_format-Oct-2022.rds")
check_log_oct <- check_npx(npx_data_format_oct) |> suppressWarnings()
npx_data_format <- clean_npx(npx_data_format_oct,
                             check_log_oct,
                             verbose = FALSE)

test_that("check_heatmap_inputs - works", {
  expect_error(check_heatmap_inputs(colnames = "wrong_answer"),
               "colnames has to be")
  expect_warning(check_heatmap_inputs(colnames = "both",
                                      mat = "1234"),
                 "Argument mat cannot be manually set")

  expect_null(check_heatmap_inputs(colnames = "both"))
})

test_that("clean_heatmap_df - works", {
  expect_no_error(clean_heatmap_df(df = npx_data_format,
                                   check_log = check_log_oct,
                                   colnames = "both"))

  expect_no_error(clean_heatmap_df(df = npx_data_format,
                                   check_log = check_log_oct,
                                   colnames = "oid"))

  expect_no_error(clean_heatmap_df(df = npx_data_format,
                                   check_log = check_log_oct,
                                   colnames = "assay"))
  npx_data_format1 <- npx_data_format |>
    dplyr::mutate(NPX = ifelse(Assay == "Incubation control 1", 1, NPX))

  expect_no_match(names(clean_heatmap_df(df = npx_data_format1,
                                         check_log = check_log_oct,
                                         colnames = "assay")),
                  "Incubation control 1")
})

test_that("df_to_wide - works", {

  expect_equal(ncol(df_to_wide(df = clean_heatmap_df(df = npx_data_format,
                                                     check_log = check_log_oct,
                                                     colnames = "assay"),
                               check_log = check_log_oct,
                               colnames = "assay")),
               length(unique(npx_data_format$Assay)))
})

test_that("create_pheatmap_args - works", {
  df <- clean_heatmap_df(df = npx_data_format,
                         check_log = check_log_oct,
                         colnames = "assay")
  df_wide <- df_to_wide(df = df,
                        check_log = check_log_oct,
                        colnames = "assay")
  expect_equal(create_pheatmap_args(df_wide = df_wide,
                                    df = df,
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
                                    na_col = "black"),
               list(mat = df_wide,
                    scale = "column",
                    silent = TRUE,
                    cluster_rows = TRUE,
                    cluster_cols = TRUE,
                    na_col = "black",
                    show_rownames = TRUE,
                    show_colnames = TRUE,
                    annotation_legend = TRUE,
                    fontsize = 10))

  expect_equal(create_pheatmap_args(df_wide = df_wide,
                                    df = df,
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
                                    na_col = "black"),
               list(mat = df_wide,
                    scale = "column",
                    silent = TRUE,
                    cluster_rows = TRUE,
                    cluster_cols = TRUE,
                    na_col = "black",
                    show_rownames = TRUE,
                    show_colnames = TRUE,
                    annotation_legend = TRUE,
                    fontsize = 10,
                    annotation_row = {
                      df |>
                        dplyr::select(SampleID, treatment2) |>
                        dplyr::distinct() |>
                        tibble::column_to_rownames("SampleID")
                    },
                    annotation_col = {
                      df |>
                        dplyr::select(assay, Assay_Warning) |>
                        dplyr::distinct() |>
                        tibble::column_to_rownames("assay")
                    },
                    annot_col_int = list(
                      Assay_Warning = olink_pal()(5)[1:2],
                      treatment2 = olink_pal()(5)[3:5]
                    )))
  expect_equal(create_pheatmap_args(df_wide = df_wide,
                                    df = df,
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
                                    cuttree_rows = 3L),
               list(mat = df_wide,
                    scale = "column",
                    silent = TRUE,
                    cluster_rows = TRUE,
                    cluster_cols = TRUE,
                    na_col = "black",
                    show_rownames = TRUE,
                    show_colnames = TRUE,
                    annotation_legend = TRUE,
                    fontsize = 10,
                    cuttree_rows = 3L))
})

test_that("extract_ellipsis_arg - works", {
  expect_equal(
    extract_ellipsis_arg(list(mat = df_wide,
                              scale = "column",
                              silent = TRUE,
                              cluster_rows = TRUE,
                              cluster_cols = TRUE,
                              na_col = "black",
                              show_rownames = TRUE,
                              show_colnames = TRUE,
                              annotation_legend = TRUE,
                              fontsize = 10),
                         annotation_colors = "something"),
    list(mat = df_wide,
         scale = "column",
         silent = TRUE,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         na_col = "black",
         show_rownames = TRUE,
         show_colnames = TRUE,
         annotation_legend = TRUE,
         fontsize = 10,
         annot_col_int = "something")
  )

  expect_equal(
    extract_ellipsis_arg(list(mat = df_wide,
                              scale = "column",
                              silent = TRUE,
                              cluster_rows = TRUE,
                              cluster_cols = TRUE,
                              na_col = "black",
                              show_rownames = TRUE,
                              show_colnames = TRUE,
                              annotation_legend = TRUE,
                              fontsize = 10),
                         annotation_colors = "something",
                         cuttree_rows = 3L),
    list(mat = df_wide,
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
         annot_col_int = "something")
  )
})

test_that("annotate_heatmap - works", {
  expect_equal(annotate_heatmap(df = df,
                                check_log = check_log_oct,
                                colnames = "assay",
                                pheatmap_args = list(mat = df_wide,
                                                     scale = "column",
                                                     silent = TRUE,
                                                     cluster_rows = TRUE,
                                                     cluster_cols = TRUE,
                                                     na_col = "black",
                                                     show_rownames = TRUE,
                                                     show_colnames = TRUE,
                                                     annotation_legend = TRUE,
                                                     fontsize = 10),
                                variable_row_list = c("treatment2"),
                                variable_col_list = c("Assay_Warning")),
               list(mat = df_wide,
                    scale = "column",
                    silent = TRUE,
                    cluster_rows = TRUE,
                    cluster_cols = TRUE,
                    na_col = "black",
                    show_rownames = TRUE,
                    show_colnames = TRUE,
                    annotation_legend = TRUE,
                    fontsize = 10,
                    annotation_row = {
                      df |>
                        dplyr::select(SampleID, treatment2) |>
                        dplyr::distinct() |>
                        tibble::column_to_rownames("SampleID")
                    },
                    annotation_col = {
                      df |>
                        dplyr::select(assay, Assay_Warning) |>
                        dplyr::distinct() |>
                        tibble::column_to_rownames("assay")
                    }))
})
