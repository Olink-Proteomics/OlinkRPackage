test_that("shape_long_to_wide supports both orientations and output types", {
  df <- tibble::tibble(sample = c("S1", "S1", "S2"),
                       assay = c("A1", "A2", "A1"),
                       value = c(1, 2, 3))
  args <- list(df = df, sample_col = "sample", assay_col = "assay",
               value_col = "value", names_sort = TRUE)
  wide <- do.call(shape_long_to_wide, args)
  expect_equal(wide, tibble::tibble(sample = c("S1", "S2"),
                                   A1 = c(1, 3), A2 = c(2, NA_real_)))
  args$orientation <- "samples_in_columns"
  wide <- do.call(shape_long_to_wide, args)
  expect_equal(wide, tibble::tibble(assay = c("A1", "A2"),
                                   S1 = c(1, 2), S2 = c(3, NA_real_)))
  args$out <- "matrix"
  mat <- do.call(shape_long_to_wide, args)
  expect_equal(mat, matrix(c(1, 2, 3, NA_real_), nrow = 2,
                           dimnames = list(c("A1", "A2"), c("S1", "S2"))))
})

test_that("shape_long_to_wide applies drop flags by entity in either orientation", {
  df <- tibble::tibble(sample = c("S1", "S1", "S2", "S2"),
                       assay = c("A1", "A2", "A1", "A2"),
                       value = c(1, 2, 3, NA_real_))
  for (orientation in c("assays_in_columns", "samples_in_columns")) {
    for (drop_assays in c(FALSE, TRUE)) {
      for (drop_samples in c(FALSE, TRUE)) {
        result <- shape_long_to_wide(
          df, sample_col = "sample", assay_col = "assay", value_col = "value",
          orientation = orientation, drop_assays = drop_assays,
          drop_samples = drop_samples, return_details = TRUE
        )
        expect_identical(result$dropped_assays,
                         if (drop_assays) "A2" else character())
        expect_identical(result$dropped_samples,
                         if (drop_samples && !drop_assays) "S2" else character())
        expect_equal(nrow(result$data), if (orientation == "assays_in_columns") {
          if (drop_samples && !drop_assays) 1L else 2L
        } else if (drop_assays) 1L else 2L)
      }
    }
  }
  filtered <- shape_long_to_wide(df, sample_col = "sample", assay_col = "assay",
                                 value_col = "value", drop_missing_values = TRUE)
  expect_true(is.na(filtered$A2[2L]))
})

test_that("shape_long_to_wide resolves check logs and explicit overrides", {
  df <- OlinkAnalyze::npx_data1 |>
    dplyr::distinct(.data[["SampleID"]], .data[["OlinkID"]], .keep_all = TRUE)
  log <- suppressWarnings(suppressMessages(check_npx(df)))
  expected <- shape_long_to_wide(df, sample_col = log$col_names$sample_id,
                                 assay_col = log$col_names$olink_id,
                                 value_col = log$col_names$quant)
  expect_equal(shape_long_to_wide(df, check_log = log), expected)
  expect_equal(shape_long_to_wide(new_olink_class(df, log)), expected)
  df$alternate <- df[[log$col_names$quant]] + 1
  actual <- shape_long_to_wide(df, check_log = log, value_col = "alternate")
  expect_equal(actual[[2L]], expected[[2L]] + 1)
  expect_error(shape_long_to_wide(df, check_log = log, sample_col = "SampleID",
                                  assay_col = "OlinkID", value_col = "NPX"),
               "unnecessary")
})

test_that("shape_long_to_wide validates matrix and identifier contracts", {
  df <- tibble::tibble(sample = c("S1", "S1"), assay = c("A1", "A1"),
                       value = c(1, 2), batch = c("B1", "B2"))
  args <- list(df = df, sample_col = "sample", assay_col = "assay", value_col = "value")
  expect_warning(wide <- do.call(shape_long_to_wide, args), "not uniquely")
  expect_type(wide$A1, "list")
  args$out <- "matrix"
  expect_warning(expect_error(do.call(shape_long_to_wide, args), "numeric and unique"),
                 "not uniquely")
  args$id_cols <- "batch"
  expect_error(do.call(shape_long_to_wide, args), "Additional row identifiers")
  args$out <- "tibble"
  expect_equal(nrow(do.call(shape_long_to_wide, args)), 2L)
  args$drop_samples <- TRUE
  expect_error(do.call(shape_long_to_wide, args), "Additional row identifiers")
  args$id_cols <- NULL
  args$drop_samples <- FALSE
  args$sample_col <- "absent"
  expect_error(do.call(shape_long_to_wide, args), "not all present")
})
test_that("shape_long_to_wide handles log precedence and Arrow input", {
  df <- OlinkAnalyze::npx_data1 |>
    dplyr::distinct(.data[["SampleID"]], .data[["OlinkID"]], .keep_all = TRUE) |>
    dplyr::mutate(alternative = .data[["NPX"]] + 10)
  log1 <- suppressWarnings(suppressMessages(check_npx(df, preferred_names = c(quant = "NPX"))))
  log2 <- suppressWarnings(suppressMessages(check_npx(df, preferred_names = c(quant = "alternative"))))
  attached <- new_olink_class(df, log1)
  result <- shape_long_to_wide(attached, check_log = log2)
  expected <- shape_long_to_wide(df, sample_col = "SampleID", assay_col = "OlinkID",
                                 value_col = "NPX")
  expect_equal(result, expected)
  arrow_df <- arrow::as_arrow_table(df)
  expect_warning(result_arrow <- shape_long_to_wide(arrow_df, check_log = log1),
                 "No `olink_check_log` metadata")
  expect_equal(result_arrow, expected)
})

test_that("shape_long_to_wide preserves missing observations and ordering", {
  df <- tibble::tibble(`sample id` = c("s2", "s1", "s1"),
                       `assay id` = c("b", "a", "b"),
                       val = c(NA_real_, 2, 1))
  args <- list(df = df, sample_col = "sample id", assay_col = "assay id", value_col = "val")
  expect_identical(names(do.call(shape_long_to_wide, args)), c("sample id", "b", "a"))
  args$names_sort <- TRUE
  expect_identical(names(do.call(shape_long_to_wide, args)), c("sample id", "a", "b"))
  args$drop_missing_values <- TRUE
  result <- do.call(shape_long_to_wide, args)
  expect_identical(result[["sample id"]], "s1")
  df[["sample id"]][1] <- NA_character_
  args$df <- df
  args$out <- "matrix"
  expect_error(do.call(shape_long_to_wide, args), "identifiers must not be missing")
})

test_that("heatmap widening preserves missing values and assay labels", {
  df <- tibble::tibble(SampleID = c("s1", "s1", "s2"),
                       both = c("a_1", "b_2", "a_1"),
                       NPX = c(1, 2, 3), extra = 10)
  log <- list(col_names = list(sample_id = "SampleID", quant = "NPX"))
  result <- plot_heatmap_df_to_wide(df, log, "both")
  expect_equal(rownames(result), c("s1", "s2"))
  expect_equal(names(result), c("a_1", "b_2"))
  expect_equal(result[["a_1"]], c(1, 3))
  expect_equal(result[["b_2"]], c(2, NA_real_))
})

test_that("shape_long_to_wide handles all-missing data and duplicate drop requests", {
  df <- tibble::tibble(sample = c("s1", "s2"), assay = c("a1", "a1"),
                       value = c(NA_real_, NA_real_))
  result <- shape_long_to_wide(df, sample_col = "sample", assay_col = "assay",
                               value_col = "value", drop_missing_values = TRUE,
                               return_details = TRUE)
  expect_equal(nrow(result$data), 0L)
  expect_identical(result$dropped_assays, character())
  df <- tibble::tibble(sample = c("s1", "s1"), assay = c("a1", "a1"),
                       value = c(1, 2))
  expect_warning(
    expect_error(shape_long_to_wide(df, sample_col = "sample", assay_col = "assay",
                                    value_col = "value", drop_assays = TRUE),
                 "unique measurements"),
    "not uniquely"
  )
})

test_that("assay cutoff is strict and works in both orientations", {
  df <- tidyr::crossing(sample = paste0("s", seq_len(20)),
                        assay = c("a1", "a2", "a3")) |>
    dplyr::mutate(value = dplyr::case_when(
      .data[["assay"]] == "a2" & .data[["sample"]] == "s1" ~ NA_real_,
      .data[["assay"]] == "a3" & .data[["sample"]] %in% c("s1", "s2") ~ NA_real_,
      TRUE ~ 1
    ))
  for (orientation in c("assays_in_columns", "samples_in_columns")) {
    result <- shape_long_to_wide(
      df, sample_col = "sample", assay_col = "assay", value_col = "value",
      orientation = orientation, assay_missingness_cutoff = 0.05,
      return_details = TRUE
    )
    expect_identical(result$dropped_assays_missingness, "a3")
    assay_ids <- if (orientation == "assays_in_columns") {
      names(result$data)[-1L]
    } else {
      result$data$assay
    }
    expect_setequal(assay_ids, c("a1", "a2"))
  }
  expect_error(shape_long_to_wide(df, sample_col = "sample", assay_col = "assay",
                                  value_col = "value", assay_missingness_cutoff = 1.1),
               "between 0 and 1")
})

test_that("sample removal occurs before calculating assay missingness", {
  df <- tidyr::crossing(sample = paste0("s", 1:4), assay = c("a1", "a2")) |>
    dplyr::mutate(value = dplyr::if_else(
      .data[["sample"]] == "s1" & .data[["assay"]] == "a2", NA_real_, 1
    ))
  for (orientation in c("assays_in_columns", "samples_in_columns")) {
    result <- shape_long_to_wide(
      df, sample_col = "sample", assay_col = "assay", value_col = "value",
      orientation = orientation, drop_samples = TRUE,
      assay_missingness_cutoff = 0.1, return_details = TRUE
    )
    expect_identical(result$dropped_samples, "s1")
    expect_identical(result$dropped_assays_missingness, character())
  }
})
