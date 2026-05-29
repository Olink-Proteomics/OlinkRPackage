# Test olink_lod ----

test_that(
  "olink_lod - works - Explore HT",
  {
    # input is tibble ----

    df_ht <- get_example_data("example_HT_data.rds")

    # add missing column ExtNPX for olink_lod to work
    df_ht <- df_ht |>
      dplyr::mutate(
        ExtNPX = .data[["NPX"]]
      )

    check_log <- check_npx(df = df_ht) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = df_ht_lod <- olink_lod(
        data = df_ht,
        check_log = check_log,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Explore%20HT_Fixed%20LOD.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_tibble(x = df_ht_lod, error = FALSE))
    expect_s3_class(object = df_ht_lod, class = "olink_class")
    expect_true(object = { ncol(x = df_ht_lod) == ncol(x = df_ht) + 2L })
    expect_false(object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht)))
    expect_true(object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_lod)))

    # input is arrow ----

    df_ht_arrow <- arrow::arrow_table(df_ht)

    check_log_arrow <- check_npx(df = df_ht_arrow) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = df_ht_lod_arrow <- olink_lod(
        data = df_ht_arrow,
        check_log = check_log_arrow,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Explore%20HT_Fixed%20LOD.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_arrow_object(x = df_ht_lod_arrow,
                                               error = FALSE))
    expect_true(object = "olink_check_log" %in% names(df_ht_lod_arrow$metadata))
    expect_true(
      object = { ncol(x = df_ht_lod_arrow) == ncol(x = df_ht_arrow) + 2L }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_arrow))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_lod_arrow))
    )

    # input is olink_class ----

    expect_message(
      object = df_ht_obj <- attach_check_log(df = df_ht, out_df = "tibble"),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_message(
      object = df_ht_obj_lod <- olink_lod(
        data = df_ht_obj,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Explore%20HT_Fixed%20LOD.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_tibble(x = df_ht_obj_lod, error = FALSE))
    expect_s3_class(object = df_ht_obj_lod, class = "olink_class")
    expect_true(
      object = { ncol(x = df_ht_obj_lod) == ncol(x = df_ht_obj) + 2L }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_obj))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_obj_lod))
    )
    expect_true(
      object = all(
        c( "LOD", "PCNormalizedLOD") %in%
          olink_check_log(df = df_ht_obj_lod)$col_names$lod
      )
    )

    # input is arrow with check_npx metadata ----

    expect_message(
      object = df_ht_arrow_obj <- attach_check_log(
        df = df_ht_arrow,
        out_df = "arrow"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_message(
      object = df_ht_arrow_obj_lod <- olink_lod(
        data = df_ht_arrow_obj,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Explore%20HT_Fixed%20LOD.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_arrow_object(x = df_ht_arrow_obj_lod,
                                               error = FALSE))
    expect_true(
      object = "olink_check_log" %in% names(df_ht_arrow_obj_lod$metadata)
    )
    expect_true(
      object = {
        ncol(x = df_ht_arrow_obj_lod) == ncol(x = df_ht_arrow_obj) + 2L
      }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_arrow_obj))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_ht_arrow_obj_lod))
    )
    expect_true(
      object = all(
        c( "LOD", "PCNormalizedLOD") %in%
          olink_check_log(df = df_ht_arrow_obj_lod)$col_names$lod
      )
    )
  }
)

test_that(
  "olink_lod - works - Reveal",
  {
    # input is tibble ----

    df_r <- get_example_data("example_Reveal_data.rds")

    # add missing column ExtNPX for olink_lod to work
    df_r <- df_r |>
      dplyr::mutate(
        ExtNPX = .data[["NPX"]]
      )

    check_log <- check_npx(df = df_r) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = df_r_lod <- olink_lod(
        data = df_r,
        check_log = check_log,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Reveal%20Fixed%20LOD%20-%20csv%20file.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_tibble(x = df_r_lod, error = FALSE))
    expect_s3_class(object = df_r_lod, class = "olink_class")
    expect_true(object = { ncol(x = df_r_lod) == ncol(x = df_r) + 2L })
    expect_false(object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r)))
    expect_true(object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_lod)))

    # input is arrow ----

    df_r_arrow <- arrow::arrow_table(df_r)

    check_log_arrow <- check_npx(df = df_r_arrow) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = df_r_lod_arrow <- olink_lod(
        data = df_r_arrow,
        check_log = check_log_arrow,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Reveal%20Fixed%20LOD%20-%20csv%20file.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_arrow_object(x = df_r_lod_arrow,
                                               error = FALSE))
    expect_true(object = "olink_check_log" %in% names(df_r_lod_arrow$metadata))
    expect_true(
      object = { ncol(x = df_r_lod_arrow) == ncol(x = df_r_arrow) + 2L }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_arrow))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_lod_arrow))
    )

    # input is olink_class ----

    expect_message(
      object = df_r_obj <- attach_check_log(df = df_r, out_df = "tibble"),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_message(
      object = df_r_obj_lod <- olink_lod(
        data = df_r_obj,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Reveal%20Fixed%20LOD%20-%20csv%20file.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_tibble(x = df_r_obj_lod, error = FALSE))
    expect_s3_class(object = df_r_obj_lod, class = "olink_class")
    expect_true(
      object = { ncol(x = df_r_obj_lod) == ncol(x = df_r_obj) + 2L }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_obj))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_obj_lod))
    )
    expect_true(
      object = all(
        c( "LOD", "PCNormalizedLOD") %in%
          olink_check_log(df = df_r_obj_lod)$col_names$lod
      )
    )

    # input is arrow with check_npx metadata ----

    expect_message(
      object = df_r_arrow_obj <- attach_check_log(
        df = df_r_arrow,
        out_df = "arrow"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_message(
      object = df_r_arrow_obj_lod <- olink_lod(
        data = df_r_arrow_obj,
        lod_file_path = "https://7074596.fs1.hubspotusercontent-na1.net/hubfs/7074596/000-documents/10-excel%20file/Reveal%20Fixed%20LOD%20-%20csv%20file.csv", # nolint: line_length_linter
        lod_method = "FixedLOD"
      ),
      regexp = paste("More than one column names in `df` was associated with",
                     "certain key. One was selected based on an ordered list")
    )

    expect_true(object = check_is_arrow_object(x = df_r_arrow_obj_lod,
                                               error = FALSE))
    expect_true(
      object = "olink_check_log" %in% names(df_r_arrow_obj_lod$metadata)
    )
    expect_true(
      object = {
        ncol(x = df_r_arrow_obj_lod) == ncol(x = df_r_arrow_obj) + 2L
      }
    )
    expect_false(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_arrow_obj))
    )
    expect_true(
      object = all(c("LOD", "PCNormalizedLOD") %in% names(df_r_arrow_obj_lod))
    )
    expect_true(
      object = all(
        c( "LOD", "PCNormalizedLOD") %in%
          olink_check_log(df = df_r_arrow_obj_lod)$col_names$lod
      )
    )
  }
)

# Test check_ht_fixed_lod_version ----

test_that(
  "no message when darid_invalid is empty",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = character(0L),
        "PanelDataArchiveVersion" = character(0L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT",
      "Version" = "7.0.0"
    )

    expect_no_message(
      check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      )
    )
  }
)

test_that(
  "warn when missing Version column",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT"
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Outdated version of Fixed LOD file detected."
    )
  }
)

test_that(
  "warn when multiple version values are presented",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = rep("Explore_HT", 2L),
      "Version" = c("7.0.0", "9.0.0")
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Multiple versions detected: \"7.0.0\" and \"9.0.0\"."
    )
  }
)

test_that(
  "warn when version < 6.0.0",
  {
    check_log = list(
      darid_invalid = dplyr::tibble(
        "DataAnalysisRefID" = c("D10007", "D20007", "D30007"),
        "PanelDataArchiveVersion" = rep("1.3.0", 3L)
      )
    )

    lod_file <- dplyr::tibble(
      "Panel" = "Explore_HT",
      "Version" = "5.0.0"
    )

    expect_warning(
      object = check_ht_fixed_lod_version(
        check_log = check_log,
        lod_file  = lod_file
      ),

      regexp = "Outdated version of Fixed LOD file detected."
    )

  }
)
